module Boilerplate
  ( addProgramBoilerplate
  )
where

import           Data.List
import qualified Data.Set                      as Set

import           Assembly

addFnBoilerplate :: PhysicalFunction -> PhysicalFunction
addFnBoilerplate (Function stackSpace name instrs) =
  let clobberedRegs =
          nub
            $ filter (\reg -> reg `Set.member` dataRegisters && reg /= RAX)
            $ concatMap (snd . getRegisters) instrs
  in  function name
        $  [UN PUSH $ R RBP, OP MOV $ RR RSP RBP]
        ++ [ OP SUB $ IR (fromIntegral stackSpace) RSP | stackSpace /= 0 ]
        ++ map (UN PUSH . R) clobberedRegs
        ++ concatMap
             (\instr -> case instr of
               RET ->
                 map (UN POP . R) (reverse clobberedRegs)
                   ++ [ OP ADD $ IR (fromIntegral stackSpace) RSP
                      | stackSpace /= 0
                      ]
                   ++ [UN POP $ R RBP, instr]
               _ -> [instr]
             )
             instrs

addProgramBoilerplate :: Program Register -> Program Register
addProgramBoilerplate (Program mainFn fns datums) =
  Program mainFn (map addFnBoilerplate fns) datums
