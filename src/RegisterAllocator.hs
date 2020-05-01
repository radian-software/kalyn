module RegisterAllocator
  ( allocateProgramRegs
  )
where

import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Assembly
import           Liveness

-- http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf

computeLivenessInterval
  :: Ord reg => Map.Map Int (Set.Set reg, Set.Set reg) -> reg -> (Int, Int)
computeLivenessInterval intervalMap reg =
  let indices = filter (\idx -> reg `Set.member` fst (intervalMap Map.! idx))
                       (Map.keys intervalMap)
  in  (head indices, last indices + 1)

intervalsIntersect :: (Int, Int) -> (Int, Int) -> Bool
intervalsIntersect (a, b) (c, d) = not (b <= c || d <= a)

tryAllocateFunctionRegs
  :: VirtualFunction -> Either [Temporary] PhysicalFunction
tryAllocateFunctionRegs fn@(Function instrs) =
  let allRegs =
          ( nub
          $ concatMap
              (\(liveIn, liveOut) -> Set.toList liveIn ++ Set.toList liveOut)
          $ Map.elems liveness
          )
      liveness    = computeLiveness instrs
      intervalMap = Map.fromList
        $ map (\reg -> (reg, computeLivenessInterval liveness reg)) allRegs
      disallowed = Map.mapWithKey
        (\reg _ -> Set.fromList $ filter
          (\dataReg -> intervalsIntersect (intervalMap Map.! reg)
                                          (intervalMap Map.! Physical dataReg)
          )
          dataRegisters
        )
        intervalMap
      (spilled, allocation) = allocate [] Map.empty allRegs
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
  in  case spilled of
        [] -> Right $ mapFunction (allocation Map.!) fn
        _  -> Left spilled

spillTemporary :: Int -> Temporary -> VirtualFunction -> VirtualFunction
spillTemporary = undefined

allocateFunctionRegs :: Int -> VirtualFunction -> PhysicalFunction
allocateFunctionRegs numSpilled fn = case tryAllocateFunctionRegs fn of
  Right fn'     -> fn'
  Left  spilled -> allocateFunctionRegs (numSpilled + length spilled) $ foldr
    (uncurry spillTemporary)
    fn
    (zip (iterate (+ 1) numSpilled) spilled)

allocateProgramRegs :: Program VirtualRegister -> Program Register
allocateProgramRegs (Program main fns datums) =
  Program (allocateFunctionRegs 0 main) (map allocateFunctionRegs 0 fns) datums
