module RegisterAllocator
  ( Allocation
  , allocateProgramRegs
  , showAllocation
  )
where

import           Control.Applicative
import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import qualified Data.Set                      as Set
import           Data.Tuple
import qualified Data.Vector                   as V

import           Assembly
import           Liveness

-- http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf

type Allocation = Map.Map VirtualRegister Register

computeLivenessInterval :: Ord reg => Liveness reg -> reg -> (Int, Int)
computeLivenessInterval intervalMap reg =
  let indices = V.findIndices (\int -> reg `Set.member` fst int) intervalMap
  in  (V.head indices, V.last indices + 1)

tryAllocateFunctionRegs
  :: Liveness VirtualRegister -> Either [Temporary] Allocation
tryAllocateFunctionRegs liveness =
  let
    allLiveIntervals =
      Map.fromListWithKey
          (\int t1 t2 -> if t1 == t2
            then t1
            else
              error
              $  "registers "
              ++ show t1
              ++ " and "
              ++ show t2
              ++ " have the same live interval "
              ++ show int
          )
        . map (\reg -> (computeLivenessInterval liveness reg, reg))
        . concatMap
            (\(liveIn, liveOut) -> Set.toList liveIn ++ Set.toList liveOut)
        . V.toList
        $ liveness
    -- nb intervals in active set are flipped to sort by end point
    allocate liveIntervals activeIntervals freeRegisters allocation spilledTemps
      = case Map.minViewWithKey liveIntervals of
        Nothing -> (allocation, spilledTemps)
        Just ((liveInterval, virtualRegister), liveIntervals') ->
          -- I don't *think* we need to worry about Map.split
          -- dropping keys that compare equal. I could be wrong
          -- though.
          let
            (expiredIntervals, activeIntervals') =
              Map.split liveInterval activeIntervals
            freeRegisters' =
              foldr (Set.insert . (allocation Map.!)) freeRegisters
                . Map.elems
                $ expiredIntervals
          in
            case virtualRegister of
              Virtual temporary -> case Set.minView freeRegisters' of
                Just (freeRegister, freeRegisters'') ->
                  let allocation' =
                          Map.insert virtualRegister freeRegister allocation
                      activeIntervals'' = Map.insert (swap liveInterval)
                                                     virtualRegister
                                                     activeIntervals'
                  in  allocate liveIntervals'
                               activeIntervals''
                               freeRegisters''
                               allocation'
                               spilledTemps
                Nothing ->
                  let (spilledInterval, potentialSpill) =
                          Map.findMin activeIntervals'
                      stolenRegister = allocation Map.! potentialSpill
                  in  case
                          (potentialSpill, fst spilledInterval > snd liveInterval)
                        of
                          -- we can only spill retroactively if the
                          -- active virtual register is actually a
                          -- temporary
                          (Virtual spilledTemp, True) ->
                            let
                              allocation' = Map.insert virtualRegister
                                                       stolenRegister
                                                       allocation
                              spilledTemps' = spilledTemp : spilledTemps
                              activeIntervals'' =
                                Map.delete spilledInterval activeIntervals'
                              activeIntervals''' = Map.insert
                                (swap liveInterval)
                                virtualRegister
                                activeIntervals''
                            in
                              allocate liveIntervals'
                                       activeIntervals'''
                                       freeRegisters'
                                       allocation'
                                       spilledTemps'
                          _ ->
                            let spilledTemps' = temporary : spilledTemps
                            in  allocate liveIntervals'
                                         activeIntervals'
                                         freeRegisters'
                                         allocation
                                         spilledTemps'
              -- handle preallocations, see section 6.4
              Physical usedRegister ->
                case
                    find
                      ((== Just usedRegister) . (`Map.lookup` allocation) . snd)
                    . Map.toList
                    $ activeIntervals'
                  of
                    Nothing -> allocate liveIntervals'
                                        activeIntervals'
                                        freeRegisters'
                                        allocation
                                        spilledTemps
                    Just (_, Virtual spilledTemp) ->
                      let spilledTemps' = spilledTemp : spilledTemps
                      in  allocate liveIntervals'
                                   activeIntervals'
                                   freeRegisters'
                                   allocation
                                   spilledTemps'
                    Just (_, Physical _) ->
                      error
                        "same physical register somehow has two live intervals"
    initialAllocation =
      Map.fromList . map (\reg -> (fromRegister reg, reg)) $ allRegisters
  in
    case
      allocate allLiveIntervals
               Map.empty
               (Set.fromList dataRegisters)
               initialAllocation
               []
    of
      (allocation, []     ) -> Right allocation
      (_         , spilled) -> Left spilled

spillMem :: Eq reg => reg -> Mem reg -> Bool
spillMem reg (Mem _ base msi) =
  base
    == reg
    || (case msi of
         Nothing       -> False
         Just (_, idx) -> idx == reg
       )

spillInstr :: Eq reg => reg -> Mem reg -> Instruction reg -> [Instruction reg]
spillInstr dir ind (OP op (IR imm reg)) | reg == dir = [OP op (IM imm ind)]
spillInstr dir ind (OP op (IM imm mem)) | spillMem dir mem =
  [OP MOV $ MR ind dir, OP op (IM imm mem)]
spillInstr dir ind (OP op (RR src dst))
  | src == dir && dst == dir = [OP MOV $ MR ind dir, OP op $ RM dir ind]
  | src == dir               = [OP MOV $ MR ind dst]
  | dst == dir               = [OP MOV $ RM src ind]
spillInstr dir ind (OP op (MR mem dst))
  | spillMem dir mem && dst == dir
  = [OP MOV $ MR ind dir, OP op $ MR mem dir, OP MOV $ RM dir ind]
  | spillMem dir mem
  = [OP MOV $ MR ind dir, OP op $ MR mem dst]
  | dst == dir
  = [OP MOV $ MR mem dir, OP op $ RM dir ind]
spillInstr dir ind (OP op (RM src mem))
  | spillMem dir mem = [OP MOV $ MR ind dir, OP op $ RM src mem]
  | src == dir       = [OP MOV $ MR ind dir, OP op $ RM dir mem]
spillInstr dir ind (UN op (R dst)) | dst == dir = [UN op (M ind)]
spillInstr dir ind (UN op (M mem)) | spillMem dir mem =
  [OP MOV $ MR ind dir, UN op (M mem)]
spillInstr dir ind (MOV64 imm dst) | dst == dir =
  [MOV64 imm dir, OP MOV $ RM dir ind]
spillInstr dir ind (SHIFT amt op dst) | dst == dir =
  [OP MOV $ MR ind dir, SHIFT amt op dir, OP MOV $ RM dir ind]
spillInstr dir ind (LEA mem dst) | spillMem dir mem && dst == dir =
  [OP MOV $ MR ind dir, LEA mem dst, OP MOV $ RM dir ind]
spillInstr dir ind (LEA mem dst) | spillMem dir mem =
  [OP MOV $ MR ind dir, LEA mem dst]
spillInstr dir ind (LEA mem dst) | dst == dir =
  [LEA mem dir, OP MOV $ RM dir ind]
spillInstr dir ind (IDIV src) | src == dir = [OP MOV $ MR ind dir, IDIV dir]
spillInstr _ _ instr                       = [instr]

spillFunction :: Eq reg => reg -> Mem reg -> Function reg -> Function reg
spillFunction dir ind (Function name stackSpace instrs) =
  Function name stackSpace . concatMap (spillInstr dir ind) $ instrs

spillTemporary :: Int -> Temporary -> VirtualFunction -> VirtualFunction
spillTemporary spillIdx temp = spillFunction
  (Virtual temp)
  (Mem (Right . fromIntegral $ (spillIdx + 1) * 8) rbp Nothing)

allocateFunctionRegs
  :: Maybe (Liveness VirtualRegister)
  -> Set.Set Temporary
  -> VirtualFunction
  -> ( PhysicalFunction
     , Liveness VirtualRegister
     , Allocation
     , Set.Set Temporary
     )
allocateFunctionRegs origLiveness allSpilled fn@(Function stackSpace name instrs)
  = let liveness = assertNoFreeVariables . computeLiveness $ instrs
    in
      case tryAllocateFunctionRegs liveness of
        Right allocation ->
          ( Function
            (stackSpace + length allSpilled * 8)
            name
            (map
              (mapInstr
                (\reg -> case reg `Map.lookup` allocation of
                  Nothing ->
                    error $ "register " ++ show reg ++ " was never live"
                  Just reg' -> reg'
                )
              )
              instrs
            )
          , fromMaybe liveness origLiveness
          , allocation
          , allSpilled
          )
        Left spilled ->
          allocateFunctionRegs (origLiveness <|> Just liveness)
                               (allSpilled `Set.union` Set.fromList spilled)
            $ foldr (uncurry spillTemporary)
                    fn
                    (zip (iterate (+ 1) (Set.size allSpilled)) spilled)

allocateProgramRegs
  :: Program VirtualRegister
  -> ( Program Register
     , ProgramLiveness VirtualRegister
     , Allocation
     , Set.Set Temporary
     )
allocateProgramRegs (Program main fns datums) =
  let
    allocate = allocateFunctionRegs Nothing Set.empty
    (main', mainLiveness, mainAllocation, mainSpilled) = allocate main
    (fns', restLiveness, restAllocation, restSpilled) =
      unzip4 (map allocate fns)
    liveness   = (main, mainLiveness) : zip fns restLiveness
    allocation = Map.unions (mainAllocation : restAllocation)
    spilled    = Set.unions (mainSpilled : restSpilled)
  in
    (Program main' fns' datums, liveness, allocation, spilled)

showAllocation :: Allocation -> Set.Set Temporary -> String
showAllocation allocation spilled = concatMap
  (\(virt, phys) ->
    show virt
      ++ " -> "
      ++ show phys
      ++ (case virt of
           Virtual temp | temp `Set.member` spilled -> " (spilled)"
           _ -> ""
         )
      ++ "\n"
  )
  (Map.toList allocation)
