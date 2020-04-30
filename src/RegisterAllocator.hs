module RegisterAllocator
  ( allocateProgramRegs
  )
where

import           Assembly

-- http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf

tryAllocateFunctionRegs
  :: VirtualFunction -> Either [Temporary] PhysicalFunction
tryAllocateFunctionRegs _ = undefined

spillTemporary :: Temporary -> VirtualFunction -> VirtualFunction
spillTemporary = undefined

allocateFunctionRegs :: VirtualFunction -> PhysicalFunction
allocateFunctionRegs fn = case tryAllocateFunctionRegs fn of
  Right fn'     -> fn'
  Left  spilled -> allocateFunctionRegs $ foldr spillTemporary fn spilled

allocateProgramRegs :: Program VirtualRegister -> Program Register
allocateProgramRegs (Program main fns datums) =
  Program (allocateFunctionRegs main) (map allocateFunctionRegs fns) datums
