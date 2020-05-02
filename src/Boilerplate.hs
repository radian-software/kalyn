module Boilerplate where

import           Assembly

addBoilerplate :: PhysicalFunction -> PhysicalFunction
addBoilerplate (Function instrs) =
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
