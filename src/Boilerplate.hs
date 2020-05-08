module Boilerplate
  ( addProgramBoilerplate
  )
where

import           Data.List

import           Assembly

addFnBoilerplate :: PhysicalFunction -> PhysicalFunction
addFnBoilerplate (Function name instrs) =
  let clobberedRegs =
          nub
            $ filter (\reg -> reg `elem` dataRegisters && reg /= RAX)
            $ concatMap (snd . getRegisters) instrs
  in  Function name
        $  [UN PUSH $ R RBP, LEA (Mem (Right 16) RSP Nothing) RBP]
        ++ map (UN PUSH . R) clobberedRegs
        ++ concatMap
             (\instr -> case instr of
               RET ->
                 map (UN POP . R) (reverse $ RBP : clobberedRegs) ++ [instr]
               _ -> [instr]
             )
             instrs

addProgramBoilerplate :: Program Register -> Program Register
addProgramBoilerplate (Program mainFn fns datums) =
  Program mainFn (map addFnBoilerplate fns) datums
