module RegisterAllocator
  ( Allocation
  , allocateProgramRegs
  , showAllocation
  )
where

import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Assembly
import           Liveness

-- http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf

type Allocation = Map.Map VirtualRegister Register

computeLivenessInterval :: Ord reg => Liveness reg -> reg -> (Int, Int)
computeLivenessInterval intervalMap reg =
  let indices = filter
        (\idx ->
          let liveness = (intervalMap Map.! idx)
          in  reg
                `Set.member` instrLiveIn liveness
                ||           reg
                `Set.member` instrDefined liveness
        )
        (Map.keys intervalMap)
  in  (head indices, last indices + 1)

intervalsIntersect :: (Int, Int) -> (Int, Int) -> Bool
intervalsIntersect (a, b) (c, d) = not (b <= c || d <= a)

-- arguably should use Temporary internally instead of
-- VirtualRegister. fix later!
tryAllocateFunctionRegs
  :: Liveness VirtualRegister
  -> Set.Set VirtualRegister
  -> Either [Temporary] Allocation
tryAllocateFunctionRegs liveness spillBlacklist =
  let
    allRegs =
      nub
          ( concatMap
              (\il -> Set.toList (instrUsed il) ++ Set.toList (instrDefined il))
          $ Map.elems liveness
          )
        \\ map fromRegister specialRegisters
    intervalMap = Map.fromList
      $ map (\reg -> (reg, computeLivenessInterval liveness reg)) allRegs
    disallowed = Map.mapWithKey
      (\reg _ -> Set.fromList $ filter
        (\dataReg ->
          Physical dataReg `Map.member` intervalMap && intervalsIntersect
            (intervalMap Map.! reg)
            (intervalMap Map.! Physical dataReg)
        )
        dataRegisters
      )
      intervalMap
    (spilled, allocation) = allocate
      []
      (Map.fromList (map (\reg -> (fromRegister reg, reg)) specialRegisters))
      -- allocate to smaller live intervals first, hopefully meaning
      -- we spill less. also allocate to already-spilled registers
      -- first, so we don't spill the same register repeatedly and
      -- fall into an infinite loop.
      (sortOn
        (\reg ->
          let (start, end) = intervalMap Map.! reg
          in  (reg `Set.notMember` spillBlacklist, end - start)
        )
        allRegs
      )
     where
      allocate spills allocs [] = (spills, allocs)
      allocate spills allocs (cur@(Physical phys) : rst) =
        allocate spills (Map.insert cur phys allocs) rst
      allocate spills allocs (cur@(Virtual temp) : rst) =
        case
            filter
              (\dataReg ->
                not (dataReg `Set.member` (disallowed Map.! cur)) && not
                  (any
                    (\other ->
                      Map.lookup other allocs
                        == Just dataReg
                        && intervalsIntersect (intervalMap Map.! cur)
                                              (intervalMap Map.! other)
                    )
                    allRegs
                  )
              )
              dataRegisters
          of
            []       -> allocate (temp : spills) allocs rst
            free : _ -> allocate spills (Map.insert cur free allocs) rst
  in
    case spilled of
      [] -> Right allocation
      _  -> Left spilled

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
  (Mem (Right . fromIntegral $ -(spillIdx + 1) * 8) rbp Nothing)

allocateFunctionRegs
  :: Set.Set Temporary
  -> Liveness VirtualRegister
  -> VirtualFunction
  -> (PhysicalFunction, Allocation, Set.Set Temporary)
allocateFunctionRegs allSpilled liveness fn@(Function stackSpace name instrs) =
  case tryAllocateFunctionRegs liveness (Set.map Virtual allSpilled) of
    Right allocation ->
      ( Function
        (stackSpace + length allSpilled * 8)
        name
        (map
          (mapInstr
            (\reg -> case reg `Map.lookup` allocation of
              Nothing   -> error $ "register " ++ show reg ++ " was never live"
              Just reg' -> reg'
            )
          )
          instrs
        )
      , allocation
      , allSpilled
      )
    Left spilled ->
      let fn'@(Function _ _ instrs') = foldr
            (uncurry spillTemporary)
            fn
            (zip (iterate (+ 1) (Set.size allSpilled)) spilled)
          liveness' = assertNoFreeVariables . computeLiveness $ instrs'
      in  allocateFunctionRegs (allSpilled `Set.union` Set.fromList spilled)
                               liveness'
                               fn'

allocateProgramRegs
  :: Program VirtualRegister
  -> ProgramLiveness VirtualRegister
  -> (Program Register, Allocation, Set.Set Temporary)
allocateProgramRegs (Program main fns datums) liveness =
  let allocate = allocateFunctionRegs Set.empty
      (main', mainAllocation, mainSpilled) =
          allocate (snd . head $ liveness) main
      (fns', restAllocation, restSpilled) =
          unzip3 (zipWith allocate (map snd . tail $ liveness) fns)
      allocation = Map.unions (mainAllocation : restAllocation)
      spilled    = Set.unions (mainSpilled : restSpilled)
  in  (Program main' fns' datums, allocation, spilled)

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
