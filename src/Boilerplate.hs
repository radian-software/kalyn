module Boilerplate
  ( addProgramBoilerplate
  )
where

import           Assembly

addFnBoilerplate :: PhysicalFunction -> PhysicalFunction
addFnBoilerplate (Function instrs) =
  let clobberedRegs =
          filter (\reg -> reg `elem` dataRegisters && reg /= RAX)
            $ concatMap (snd . getRegisters) instrs
  in  Function
        $  [UN PUSH $ R RBP, LEA (Mem (Right 8) RSP Nothing) RBP]
        ++ map (UN PUSH . R) clobberedRegs
        ++ concatMap
             (\instr -> case instr of
               RET -> map (UN POP . R) (RBP : clobberedRegs) ++ [instr]
               _   -> [instr]
             )
             instrs

addProgramBoilerplate :: Program Register -> Program Register
addProgramBoilerplate (Program mainFn fns datums) =
  Program mainFn (map addFnBoilerplate fns) datums
