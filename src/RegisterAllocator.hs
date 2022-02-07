module RegisterAllocator
  ( Allocation
  , allocateProgramRegs
  , showAllocation
  ) where

import           Control.Monad
import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import qualified Data.Set                      as Set

import           Assembly
import           Liveness

{-# ANN module "HLint: ignore Use lambda-case" #-}

-- http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf

type Allocation = Map.Map VirtualRegister Register

computeLivenessIntervals :: Ord reg => Liveness reg -> Map.Map reg (Int, Int)
computeLivenessIntervals livenesses =
  let alter idx = Map.alter
        (\interval -> case interval of
          Nothing         -> Just (idx, idx + 1)
          Just (start, _) -> Just (start, idx + 1)
        )
  in  foldl'
        (\intervals (liveness, idx) -> Set.foldr
          (alter idx)
          (Set.foldr (alter idx) intervals (instrDefined liveness))
          (instrLiveIn liveness)
        )
        Map.empty
        (zip livenesses (iterate (+ 1) 0))

intervalsIntersect :: (Int, Int) -> (Int, Int) -> Bool
intervalsIntersect (a, b) (c, d) = not (b <= c || d <= a)

-- arguably should use Temporary internally instead of
-- VirtualRegister. fix later!
tryAllocateFunctionRegs
  :: Liveness VirtualRegister -> Either [Temporary] Allocation
tryAllocateFunctionRegs liveness =
  let intervalMap = computeLivenessIntervals liveness
      disallowed  = Map.mapWithKey
        (\reg _ -> Set.filter
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
          (\reg -> let (start, end) = intervalMap Map.! reg in end - start)
          (Map.keys intervalMap)
        )
         where
          allocate spills allocs [] = (spills, allocs)
          allocate spills allocs (cur@(Physical phys) : rst) =
            allocate spills (Map.insert cur phys allocs) rst
          allocate spills allocs (cur@(Virtual temp) : rst) =
            case
                let curInterval      = intervalMap Map.! cur
                    conflictingTemps = mapMaybe
                      (\(otherTemp, otherInterval) ->
                        if intervalsIntersect curInterval otherInterval
                          then Just otherTemp
                          else Nothing
                      )
                      (Map.toList intervalMap)
                    curDisallowed = foldr
                      (\conflictingTemp set ->
                        case Map.lookup conflictingTemp allocs of
                          Just phys -> Set.insert phys set
                          Nothing   -> set
                      )
                      (disallowed Map.! cur)
                      conflictingTemps
                in  Set.lookupMin (dataRegisters Set.\\ curDisallowed)
              of
                Nothing   -> allocate (temp : spills) allocs rst
                Just free -> allocate spills (Map.insert cur free allocs) rst
  in  case spilled of
        [] -> Right allocation
        _  -> Left spilled

shouldSpillMem :: Eq reg => reg -> Mem reg -> Bool
shouldSpillMem reg (Mem _ base msi) =
  base
    == reg
    || (case msi of
         Nothing       -> False
         Just (_, idx) -> idx == reg
       )

spillMem :: Eq reg => reg -> reg -> Mem reg -> Mem reg
spillMem old new (Mem disp base msi) = Mem
  disp
  (if base == old then new else base)
  (((\index -> if index == old then new else index) <$>) <$> msi)

-- FIXME: spillMem needs to be able to substitute

spillInstr
  :: VirtualRegister
  -> Mem VirtualRegister
  -> VirtualInstruction
  -> Stateful [VirtualInstruction]
spillInstr orig ind (OP op (IR imm reg)) | reg == orig =
  return [OP op (IM imm ind)]
spillInstr orig ind (OP op (IM imm mem)) | shouldSpillMem orig mem = do
  dir <- newTemp
  return [OP MOV $ MR ind dir, OP op (IM imm (spillMem orig dir mem))]
spillInstr orig ind (OP op (RR src dst))
  | src == orig && dst == orig = do
    dir <- newTemp
    return [OP MOV $ MR ind dir, OP op $ RM dir ind]
  | src == orig = return [OP MOV $ MR ind dst]
  | dst == orig = return [OP MOV $ RM src ind]
spillInstr orig ind (OP op (MR mem dst))
  | shouldSpillMem orig mem && dst == orig = do
    dir <- newTemp
    return
      [ OP MOV $ MR ind dir
      , OP op $ MR (spillMem orig dir mem) dir
      , OP MOV $ RM dir ind
      ]
  | shouldSpillMem orig mem = do
    dir <- newTemp
    return [OP MOV $ MR ind dir, OP op $ MR (spillMem orig dir mem) dst]
  | dst == orig = do
    dir <- newTemp
    return [OP MOV $ MR (spillMem orig dir mem) dir, OP op $ RM dir ind]
spillInstr orig ind (OP op (RM src mem))
  | shouldSpillMem orig mem = do
    dir <- newTemp
    return [OP MOV $ MR ind dir, OP op $ RM src (spillMem orig dir mem)]
  | src == orig = do
    dir <- newTemp
    return [OP MOV $ MR ind dir, OP op $ RM dir mem]
spillInstr orig ind (UN op (R dst)) | dst == orig = return [UN op (M ind)]
spillInstr orig ind (UN op (M mem)) | shouldSpillMem orig mem = do
  dir <- newTemp
  return [OP MOV $ MR ind dir, UN op (M (spillMem orig dir mem))]
spillInstr orig ind (MOVBRM src mem)
  | shouldSpillMem orig mem = do
    dir <- newTemp
    return [OP MOV $ MR ind dir, MOVBRM src (spillMem orig dir mem)]
  | src == orig = do
    dir <- newTemp
    return [OP MOV $ MR ind dir, MOVBRM dir (spillMem orig dir mem)]
spillInstr orig ind (MOVBMR mem dst)
  | shouldSpillMem orig mem && dst == orig = do
    dir <- newTemp
    return
      [ OP MOV $ MR ind dir
      , MOVBMR (spillMem orig dir mem) dir
      , OP MOV $ RM dir ind
      ]
  | shouldSpillMem orig mem = do
    dir <- newTemp
    return [OP MOV $ MR ind dir, MOVBMR (spillMem orig dir mem) dst]
  | dst == orig = do
    dir <- newTemp
    return [OP MOV $ MR (spillMem orig dir mem) dir, MOVBRM dir ind]
spillInstr orig ind (MOV64 imm dst) | dst == orig = do
  dir <- newTemp
  return [MOV64 imm dir, OP MOV $ RM dir ind]
spillInstr orig ind (SHIFT amt op dst) | dst == orig = do
  dir <- newTemp
  return [OP MOV $ MR ind dir, SHIFT amt op dir, OP MOV $ RM dir ind]
spillInstr orig ind (LEA mem dst) | shouldSpillMem orig mem && dst == orig = do
  dir <- newTemp
  return
    [OP MOV $ MR ind dir, LEA (spillMem orig dir mem) dst, OP MOV $ RM dir ind]
spillInstr orig ind (LEA mem dst) | shouldSpillMem orig mem = do
  dir <- newTemp
  return [OP MOV $ MR ind dir, LEA (spillMem orig dir mem) dst]
spillInstr orig ind (LEA mem dst) | dst == orig = do
  dir <- newTemp
  return [LEA (spillMem orig dir mem) dir, OP MOV $ RM dir ind]
spillInstr orig ind (IDIV src) | src == orig = do
  dir <- newTemp
  return [OP MOV $ MR ind dir, IDIV dir]
spillInstr _ _ instr = return [instr]

spillFunction
  :: VirtualRegister
  -> Mem VirtualRegister
  -> VirtualFunction
  -> Stateful VirtualFunction
spillFunction dir ind (Function name stackSpace instrs) =
  Function name stackSpace . concat <$> mapM (spillInstr dir ind) instrs

spillTemporary
  :: Int -> Temporary -> VirtualFunction -> Stateful VirtualFunction
spillTemporary spillIdx temp = spillFunction
  (Virtual temp)
  (Mem (Right . fromIntegral $ -(spillIdx + 1) * 8) rbp Nothing)

-- step 1: only do max of 2 passes for liveness analysis
-- step 2: only collect jump-target instructions into the map
--           (otherwise operate on lists in linear time)

allocateFunctionRegs
  :: Set.Set Temporary
  -> Liveness VirtualRegister
  -> VirtualFunction
  -> Stateful (PhysicalFunction, Allocation, Set.Set Temporary)
allocateFunctionRegs allSpilled liveness fn@(Function stackSpace name instrs) =
  case tryAllocateFunctionRegs liveness of
    Right allocation -> return
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
    Left spilled -> do
      fn'@(Function _ fnName instrs') <- foldM
        (flip $ uncurry spillTemporary)
        fn
        (zip (iterate (+ 1) (Set.size allSpilled)) spilled)
      let liveness' = assertNoFreeVariables fnName . computeLiveness $ instrs'
      allocateFunctionRegs (allSpilled `Set.union` Set.fromList spilled)
                           liveness'
                           fn'

allocateProgramRegs
  :: Program VirtualRegister
  -> ProgramLiveness VirtualRegister
  -> Stateful (Program Register, Allocation, Set.Set Temporary)
allocateProgramRegs (Program main fns datums) liveness = do
  let allocate = allocateFunctionRegs Set.empty
  (main', mainAllocation, mainSpilled) <- allocate (snd . head $ liveness) main
  (fns' , restAllocation, restSpilled) <-
    unzip3 <$> zipWithM allocate (map snd . tail $ liveness) fns
  let allocation = Map.unions (mainAllocation : restAllocation)
  let spilled    = Set.unions (mainSpilled : restSpilled)
  return (Program main' fns' datums, allocation, spilled)

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
