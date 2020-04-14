module Primitives where

import           Assembly
import           Subroutines

basicOp :: String -> Op -> Stateful (Function VirtualRegister)
basicOp name op = do
  temp <- newTemp
  return $ function
    name
    [ Right
        [ OP MOV $ MR (getArg 2) temp
        , OP op $ MR (getArg 1) temp
        , OP MOV $ RR temp rax
        , RET
        ]
    ]

plus :: Stateful (Function VirtualRegister)
plus = basicOp "plus" ADD

minus :: Stateful (Function VirtualRegister)
minus = basicOp "minus" SUB

times :: Stateful (Function VirtualRegister)
times = basicOp "times" IMUL

divOp
  :: String
  -> [Instruction VirtualRegister]
  -> Stateful (Function VirtualRegister)
divOp name post = do
  temp <- newTemp
  return $ function
    name
    [ Right
      $  [ OP MOV $ MR (getArg 2) rax
         , CQTO
         , OP MOV $ MR (getArg 1) temp
         , IDIV temp
         ]
      ++ post
      ++ [RET]
    ]

divide :: Stateful (Function VirtualRegister)
divide = divOp "divide" []

modulo :: Stateful (Function VirtualRegister)
modulo = divOp "modulo" [OP MOV $ RR rdx rax]

and :: Stateful (Function VirtualRegister)
and = basicOp "and" AND

or :: Stateful (Function VirtualRegister)
or = basicOp "or" OR

xor :: Stateful (Function VirtualRegister)
xor = basicOp "xor" XOR

bitnot :: Stateful (Function VirtualRegister)
bitnot = do
  temp <- newTemp
  return $ function
    "not"
    [Right [OP MOV $ MR (getArg 1) temp, NOT temp, OP MOV $ RR temp rax]]

shiftOp :: String -> Shift -> Stateful (Function VirtualRegister)
shiftOp name op = do
  arg       <- newTemp
  fixup     <- newLabel
  fixupDone <- newLabel
  let needsZeroing = case op of
        SHL -> True
        SAL -> True
        SHR -> True
        SAR -> False
  return $ function
    name
    [ Right
      [ OP MOV $ MR (getArg 2) arg
      , OP MOV $ MR (getArg 1) rcx
      , OP CMP $ IR 64 rcx
      , JGE fixup
      ]
    , Right [SHIFT Nothing op arg]
    , Left fixupDone
    , Right [OP MOV $ RR arg rax, RET]
    , Left fixup
    , Right
      $ if needsZeroing then [OP MOV $ IR 0 arg] else [SHIFT (Just 63) op arg]
    , Right [RET]
    ]

shl :: Stateful (Function VirtualRegister)
shl = shiftOp "shl" SHL

shr :: Stateful (Function VirtualRegister)
shr = shiftOp "shr" SHR

sal :: Stateful (Function VirtualRegister)
sal = shiftOp "sal" SAL

sar :: Stateful (Function VirtualRegister)
sar = shiftOp "sar" SAR

print :: Stateful (Function VirtualRegister)
print = do
  temp <- newTemp
  return $ function
    "print"
    [ Right
        [ OP MOV $ MR (getArg 1) temp
        , PUSH temp
        , CALL "memoryPackString"
        , OP MOV $ IR 1 rax
        , OP MOV $ IR 1 rdi
        , LEA (Mem (Right 1) rax Nothing) rsi
        , OP MOV $ MR (deref rax) rdx
        , SYSCALL 3 -- write
        , RET
        ]
    ]
