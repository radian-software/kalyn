module RegisterAllocator
  ( allocateProgramRegs
  )
where

import           Assembly

tryAllocateFunctionRegs
  :: Function VirtualRegister -> Either [Temporary] (Function Register)
tryAllocateFunctionRegs _ = undefined

spillTemporary
  :: Temporary -> Function VirtualRegister -> Function VirtualRegister
spillTemporary = undefined

allocateFunctionRegs :: Function VirtualRegister -> Function Register
allocateFunctionRegs fn = case tryAllocateFunctionRegs fn of
  Right fn'     -> fn'
  Left  spilled -> allocateFunctionRegs $ foldr spillTemporary fn spilled

allocateProgramRegs :: Program VirtualRegister -> Program Register
allocateProgramRegs (Program fns datums) =
  Program (map allocateFunctionRegs fns) datums
