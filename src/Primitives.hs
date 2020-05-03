module Primitives where

import           Control.Exception

import           Assembly
import           Subroutines

-- https://filippo.io/linux-syscall-table/
-- see also section 2 of the Linux man pages

basicOp :: String -> BinOp -> Stateful VirtualFunction
basicOp name op = do
  temp <- newTemp
  return $ function
    name
    [ OP MOV $ MR (getArg 1) temp
    , OP op $ MR (getArg 2) temp
    , OP MOV $ RR temp rax
    , RET
    ]

plus :: Stateful VirtualFunction
plus = basicOp "plus" ADD

minus :: Stateful VirtualFunction
minus = basicOp "minus" SUB

times :: Stateful VirtualFunction
times = basicOp "times" IMUL

divOp :: String -> [VirtualInstruction] -> Stateful VirtualFunction
divOp name post = do
  temp <- newTemp
  return
    $  function name
    $ [OP MOV $ MR (getArg 2) rax, CQTO, OP MOV $ MR (getArg 1) temp, IDIV temp]
    ++ post
    ++ [RET]

divide :: Stateful VirtualFunction
divide = divOp "divide" []

modulo :: Stateful VirtualFunction
modulo = divOp "modulo" [OP MOV $ RR rdx rax]

and :: Stateful VirtualFunction
and = basicOp "and" AND

or :: Stateful VirtualFunction
or = basicOp "or" OR

xor :: Stateful VirtualFunction
xor = basicOp "xor" XOR

bitnot :: Stateful VirtualFunction
bitnot = do
  temp <- newTemp
  return $ function
    "not"
    [OP MOV $ MR (getArg 1) temp, UN NOT $ R temp, OP MOV $ RR temp rax]

shiftOp :: String -> Shift -> Stateful VirtualFunction
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
    , JUMP JGE fixup
    , SHIFT Nothing op arg
    , LABEL fixupDone
    , OP MOV $ RR arg rax
    , RET
    , LABEL fixup
    , if needsZeroing then OP MOV $ IR 0 arg else SHIFT (Just 63) op arg
    , RET
    ]

shl :: Stateful VirtualFunction
shl = shiftOp "shl" SHL

shr :: Stateful VirtualFunction
shr = shiftOp "shr" SHR

sal :: Stateful VirtualFunction
sal = shiftOp "sal" SAL

sar :: Stateful VirtualFunction
sar = shiftOp "sar" SAR

print :: Stateful VirtualFunction
print = do
  temp <- newTemp
  return $ function
    "print"
    [ OP MOV $ MR (getArg 1) temp
    , UN PUSH $ R temp
    , JUMP CALL "memoryPackString"
    , unpush 1
    , OP MOV $ IR 1 rax
    , OP MOV $ IR 1 rdi
    , LEA (Mem (Right 8) rax Nothing) rsi
    , OP MOV $ MR (deref rax) rdx
    , SYSCALL 3 -- write
    , RET
    ]

writeFile :: Stateful VirtualFunction
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
    , UN PUSH $ R temp
    , JUMP CALL "memoryPackString"
    , unpush 1
    , OP MOV $ RR rax filename
    , OP MOV $ MR (getArg 1) temp
    , UN PUSH $ R temp
    , JUMP CALL "memoryPackString"
    , unpush 1
    , OP MOV $ RR rax contents
    , OP MOV $ IR 87 rax
    , LEA (getField 1 filename) rdi
    , SYSCALL 1 -- unlink
    , OP CMP $ IR 0 rax
    , JUMP JL "crash"
    , OP MOV $ IR 2 rax
    , LEA (getField 1 filename) rdi
    , OP MOV $ IR 0x41 rsi
    , OP MOV $ IR 0o666 rdx
    , SYSCALL 2 -- open
    , OP CMP $ IR 0 rax
    , JUMP JL "crash"
    , OP MOV $ RR rax fd
    , LEA (getField 1 contents) ptr
    , OP MOV $ MR (getField 0 contents) bytesLeft
    , LABEL writeStart
    , OP CMP $ IR 0 bytesLeft
    , JUMP JLE writeDone
    , OP MOV $ IR 1 rax
    , OP MOV $ RR fd rdi
    , OP MOV $ RR ptr rsi
    , OP MOV $ RR bytesLeft rdx
    , SYSCALL 3 -- write
    , OP CMP $ IR 0 rax
    , JUMP JL "crash"
    , OP ADD $ RR rax ptr
    , OP SUB $ RR rax bytesLeft
    , JUMP JMP writeStart
    , LABEL writeDone
    , OP MOV $ IR 3 rax
    , OP MOV $ RR fd rdi
    , SYSCALL 1 -- close
    , OP CMP $ IR 0 rax
    , JUMP JL "crash"
    , RET
    ]

setFileMode :: Stateful VirtualFunction
setFileMode = do
  temp     <- newTemp
  filename <- newTemp
  return $ function
    "setFileMode"
    [ OP MOV $ MR (getArg 2) temp
    , UN PUSH $ R temp
    , JUMP CALL "memoryPackString"
    , unpush 1
    , OP MOV $ RR rax filename
    , OP MOV $ IR 90 rax
    , LEA (getField 1 filename) rdi
    , OP MOV $ MR (getArg 1) rsi
    , SYSCALL 2 -- chmod
    , RET
    ]

primitiveError :: Stateful VirtualFunction
primitiveError = return $ function
  "error"
  [ UN PUSH $ M (getArg 1)
  , JUMP CALL "memoryPackString"
  , unpush 1
  , OP MOV $ MR (deref rax) rsi
  , OP MOV $ IR 2 rdi
  , LEA (Mem (Right 8) rax Nothing) rdx
  , OP MOV $ IR 1 rax
  , SYSCALL 3 -- write
  , OP MOV $ IR 60 rax
  , OP MOV $ IR 1 rdi
  , SYSCALL 1 -- exit
  ]

equals :: Stateful VirtualFunction
equals = do
  temp <- newTemp
  yes  <- newLabel
  return $ function
    "equals"
    [ OP MOV $ MR (getArg 1) temp
    , OP CMP $ MR (getArg 2) temp
    , JUMP JE yes
    , OP MOV $ IR 0 rax
    , RET
    , LABEL yes
    , OP MOV $ IR 1 rax
    , RET
    ]

lessThan :: Stateful VirtualFunction
lessThan = do
  temp <- newTemp
  yes  <- newLabel
  return $ function
    "equals"
    [ OP MOV $ MR (getArg 1) temp
    , OP CMP $ MR (getArg 2) temp
    , JUMP JL yes
    , OP MOV $ IR 1 rax
    , RET
    , LABEL yes
    , OP MOV $ IR 0 rax
    , RET
    ]

monadPure :: Stateful VirtualFunction
monadPure = return $ function "pure" [OP MOV $ MR (getArg 1) rax, RET]

monadBind :: Stateful VirtualFunction
monadBind = do
  monad          <- newTemp
  arg            <- newTemp
  fn             <- newTemp
  firstCallCode  <- translateCall monad Nothing
  secondCallCode <- translateCall fn (Just arg)
  return $ function
    "bind"
    (  [OP MOV $ MR (getArg 1) monad]
    ++ firstCallCode
    ++ [OP MOV $ RR rax arg, OP MOV $ MR (getArg 2) fn]
    ++ secondCallCode
    -- we should never actually get here
    ++ [RET]
    )

monadify :: Int -> String -> Stateful VirtualFunction
monadify numArgs fnName = do
  fnPtr <- newTemp
  arg   <- newTemp
  return $ function
    (fnName ++ "__monadified")
    (  [ PUSHI (fromIntegral $ (numArgs + 2) * 8)
       , JUMP CALL "memoryAlloc"
       , unpush 1
       , LEA (memLabel fnName) fnPtr
       , OP MOV $ RM fnPtr (getField 0 rax)
       , OP MOV $ IM (fromIntegral numArgs) (getField 1 rax)
       ]
    ++ concatMap
         (\i ->
           [ OP MOV $ MR (getArg i) arg
           , OP MOV $ RM arg (getField (i + 1) rax)
           ]
         )
         [1 .. numArgs]
    ++ [RET]
    )

curryify :: Int -> String -> Stateful [VirtualFunction]
curryify numArgs fnName = do
  let _ = assert (numArgs >= 2)
  mapM
    (\numCurried -> do
      fnPtr     <- newTemp
      nextFnPtr <- newTemp
      arg       <- newTemp
      let nextFnName = if numCurried == numArgs - 1
            then fnName
            else fnName ++ "__curried" ++ show (numCurried + 1)
      return $ function
        (fnName ++ "__curried" ++ show numCurried)
        (  [ PUSHI (fromIntegral $ (numCurried + 3) * 8)
           , JUMP CALL "memoryAlloc"
           , unpush 1
           , OP MOV $ RR rax fnPtr
           , LEA (memLabel nextFnName) nextFnPtr
           , OP MOV $ RM nextFnPtr (getField 0 fnPtr)
           , OP MOV $ IM (fromIntegral $ numCurried + 1) (getField 1 fnPtr)
           ]
        ++ concatMap
             (\i ->
               [ OP MOV $ MR (getArg i) arg
               , OP MOV $ RM arg (getField (i + 1) fnPtr)
               ]
             )
             [1 .. numCurried + 1]
        ++ [OP MOV $ RR fnPtr rax, RET]
        )
    )
    [0 .. numArgs - 1]
