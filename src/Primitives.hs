module Primitives where

import           Assembly
import           Subroutines

-- https://filippo.io/linux-syscall-table/
-- see also section 2 of the Linux man pages

basicOp :: String -> Op -> Stateful (Function VirtualRegister)
basicOp name op = do
  temp <- newTemp
  return $ function
    name
    [ OP MOV $ MR (getArg 2) temp
    , OP op $ MR (getArg 1) temp
    , OP MOV $ RR temp rax
    , RET
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
  return
    $  function name
    $ [OP MOV $ MR (getArg 2) rax, CQTO, OP MOV $ MR (getArg 1) temp, IDIV temp]
    ++ post
    ++ [RET]

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
    [OP MOV $ MR (getArg 1) temp, NOT temp, OP MOV $ RR temp rax]

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
    [ OP MOV $ MR (getArg 2) arg
    , OP MOV $ MR (getArg 1) rcx
    , OP CMP $ IR 64 rcx
    , JGE fixup
    , SHIFT Nothing op arg
    , LABEL fixupDone
    , OP MOV $ RR arg rax
    , RET
    , LABEL fixup
    , if needsZeroing then OP MOV $ IR 0 arg else SHIFT (Just 63) op arg
    , RET
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
    [ OP MOV $ MR (getArg 1) temp
    , PUSH temp
    , CALL "memoryPackString"
    , unpush 1
    , OP MOV $ IR 1 rax
    , OP MOV $ IR 1 rdi
    , LEA (Mem (Right 1) rax Nothing) rsi
    , OP MOV $ MR (deref rax) rdx
    , SYSCALL 3 -- write
    , RET
    ]

writeFile :: Stateful (Function VirtualRegister)
writeFile = do
  temp       <- newTemp
  filename   <- newTemp
  contents   <- newTemp
  fd         <- newTemp
  ptr        <- newTemp
  bytesLeft  <- newTemp
  writeStart <- newLabel
  writeDone  <- newLabel
  return $ function
    "writeFile"
    [ OP MOV $ MR (getArg 2) temp
    , PUSH temp
    , CALL "memoryPackString"
    , unpush 1
    , OP MOV $ RR rax filename
    , OP MOV $ MR (getArg 1) temp
    , PUSH temp
    , CALL "memoryPackString"
    , unpush 1
    , OP MOV $ RR rax contents
    , OP MOV $ IR 87 rax
    , LEA (getField 1 filename) rdi
    , SYSCALL 1 -- unlink
    , OP CMP $ IR 0 rax
    , JL "crash"
    , OP MOV $ IR 2 rax
    , LEA (getField 1 filename) rdi
    , OP MOV $ IR 0x41 rsi
    , OP MOV $ IR 0o666 rdx
    , SYSCALL 2 -- open
    , OP CMP $ IR 0 rax
    , JL "crash"
    , OP MOV $ RR rax fd
    , LEA (getField 1 contents) ptr
    , OP MOV $ MR (getField 0 contents) bytesLeft
    , LABEL writeStart
    , OP CMP $ IR 0 bytesLeft
    , JLE writeDone
    , OP MOV $ IR 1 rax
    , OP MOV $ RR fd rdi
    , OP MOV $ RR ptr rsi
    , OP MOV $ RR bytesLeft rdx
    , SYSCALL 3 -- write
    , OP CMP $ IR 0 rax
    , JL "crash"
    , OP ADD $ RR rax ptr
    , OP SUB $ RR rax bytesLeft
    , JMP writeStart
    , LABEL writeDone
    , OP MOV $ IR 3 rax
    , OP MOV $ RR fd rdi
    , SYSCALL 1 -- close
    , OP CMP $ IR 0 rax
    , JL "crash"
    , RET
    ]

setFileMode :: Stateful (Function VirtualRegister)
setFileMode = do
  temp     <- newTemp
  filename <- newTemp
  return $ function
    "setFileMode"
    [ OP MOV $ MR (getArg 2) temp
    , PUSH temp
    , CALL "memoryPackString"
    , unpush 1
    , OP MOV $ RR rax filename
    , OP MOV $ IR 90 rax
    , LEA (getField 1 filename) rdi
    , OP MOV $ MR (getArg 1) rsi
    , SYSCALL 2 -- chmod
    , RET
    ]
