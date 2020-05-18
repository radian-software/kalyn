module Primitives where

import           Assembly
import           OS
import           Subroutines

-- https://filippo.io/linux-syscall-table/
-- see also section 2 of the Linux man pages

basicOp :: String -> BinOp -> Stateful VirtualFunction
basicOp name op = do
  temp <- newTemp
  return $ function
    (name ++ "__uncurried")
    [ OP MOV $ MR (getArg 2) temp
    , OP op $ MR (getArg 1) temp
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
    $  function (name ++ "__uncurried")
    $ [OP MOV $ MR (getArg 2) rax, CQTO, OP MOV $ MR (getArg 1) temp, IDIV temp]
    ++ post
    ++ [RET]

divide :: Stateful VirtualFunction
divide = divOp "divide" []

modulo :: Stateful VirtualFunction
modulo = divOp "modulo" [OP MOV $ RR rdx rax]

bitAnd :: Stateful VirtualFunction
bitAnd = basicOp "and" AND

bitOr :: Stateful VirtualFunction
bitOr = basicOp "or" OR

xor :: Stateful VirtualFunction
xor = basicOp "xor" XOR

bitNot :: Stateful VirtualFunction
bitNot = do
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
    (name ++ "__uncurried")
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
    , OP MOV $ RR arg rax
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

monadPrint :: Stateful VirtualFunction
monadPrint = do
  temp <- newTemp
  str  <- newTemp
  return $ function
    "print__uncurried__unmonadified"
    [ OP MOV $ MR (getArg 1) temp
    , UN PUSH $ R temp
    , JUMP CALL "memoryPackString"
    , OP MOV $ RR rax str
    , unpush 1
    , OP MOV $ IR 1 rax
    , OP MOV $ IR 1 rdi
    , LEA (Mem (Right 8) str Nothing) rsi
    , OP MOV $ MR (deref str) rdx
    , SYSCALL 3 -- write
    , OP MOV $ IR 0 rax
    , RET
    ]

monadWriteFile :: Stateful VirtualFunction
monadWriteFile = do
  temp       <- newTemp
  filename   <- newTemp
  contents   <- newTemp
  fd         <- newTemp
  ptr        <- newTemp
  bytesLeft  <- newTemp
  notExists  <- newLabel
  writeStart <- newLabel
  writeDone  <- newLabel
  crash      <- newLabel
  msg        <- newTemp
  return $ function
    "writeFile__uncurried__unmonadified"
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
    , OP CMP $ IR (-2) rax
    , JUMP JE notExists
    , OP CMP $ IR 0 rax
    , JUMP JL crash
    , LABEL notExists
    , OP MOV $ IR 2 rax
    , LEA (getField 1 filename) rdi
    , OP MOV $ IR 0x41 rsi
    , OP MOV $ IR 0o666 rdx
    , SYSCALL 3 -- open
    , OP CMP $ IR 0 rax
    , JUMP JL crash
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
    , JUMP JL crash
    , OP ADD $ RR rax ptr
    , OP SUB $ RR rax bytesLeft
    , JUMP JMP writeStart
    , LABEL writeDone
    , OP MOV $ IR 3 rax
    , OP MOV $ RR fd rdi
    , SYSCALL 1 -- close
    , OP CMP $ IR 0 rax
    , JUMP JL crash
    , OP MOV $ IR 0 rax
    , RET
    , LABEL crash
    , LEA (memLabel "msgWriteFileFailed") msg
    , UN PUSH $ R msg
    , JUMP CALL "crash"
    ]

setFileMode :: Stateful VirtualFunction
setFileMode = do
  temp     <- newTemp
  filename <- newTemp
  crash    <- newLabel
  msg      <- newTemp
  return $ function
    "setFileMode__uncurried__unmonadified"
    [ OP MOV $ MR (getArg 2) temp
    , UN PUSH $ R temp
    , JUMP CALL "memoryPackString"
    , unpush 1
    , OP MOV $ RR rax filename
    , OP MOV $ IR 90 rax
    , LEA (getField 1 filename) rdi
    , OP MOV $ MR (getArg 1) rsi
    , SYSCALL 2 -- chmod
    , OP CMP $ IR 0 rax
    , JUMP JL crash
    , OP MOV $ IR 0 rax
    , RET
    , LABEL crash
    , LEA (memLabel "msgSetFileModeFailed") msg
    , UN PUSH $ R msg
    , JUMP CALL "crash"
    ]

monadGetWorkingDirectory :: Stateful VirtualFunction
monadGetWorkingDirectory = do
  msg   <- newTemp
  crash <- newLabel
  return $ function
    "getWorkingDirectory__unmonadified"
    [ OP MOV $ IR 79 rax
    , LEA (memLabel "syscallBuffer") rdi
    , OP MOV $ IR (fromIntegral syscallBufferSize) rsi
    , SYSCALL 2  -- getcwd
    , UN PUSH $ R rax
    , PUSHI (-1)
    , JUMP CALL "memoryUnpackString"
    , unpush 2
    , RET
    , LABEL crash
    , LEA (memLabel "msgGetWorkingDirectoryFailed") msg
    , UN PUSH $ R msg
    , JUMP CALL "crash"
    ]

monadReadFile :: Stateful VirtualFunction
monadReadFile = do
  buffer          <- newTemp
  filename        <- newTemp
  fd              <- newTemp
  strStart        <- newTemp
  strEnd          <- newTemp
  bytesRead       <- newTemp
  msg             <- newTemp
  newString       <- newTemp
  allocedLength   <- newTemp
  readStart       <- newLabel
  readDone        <- newLabel
  skipStartUpdate <- newLabel
  skipEndUpdate   <- newLabel
  crash           <- newLabel
  return $ function
    "readFile__uncurried__unmonadified"
    [ LEA (memLabel "syscallBuffer") buffer
    , UN PUSH $ M (getArg 1)
    , JUMP CALL "memoryPackString"
    , unpush 1
    , OP MOV $ RR rax filename
    , OP MOV $ IR 2 rax
    , LEA (getField 1 filename) rdi
    , OP MOV $ IR 0 rsi
    , SYSCALL 2  -- open
    , OP MOV $ RR rax fd
    , OP MOV $ IR 0 strStart
    , OP MOV $ IR 0 strEnd
    , LABEL readStart
    , OP MOV $ IR 0 rax
    , OP MOV $ RR fd rdi
    , OP MOV $ RR buffer rsi
    , OP MOV $ IR (fromIntegral syscallBufferSize) rdx
    , SYSCALL 3  -- read
    , OP MOV $ RR rax bytesRead
    , OP CMP $ IR 0 bytesRead
    , JUMP JE readDone
    , JUMP JL crash
    , UN PUSH $ R buffer
    , UN PUSH $ R bytesRead
    , JUMP CALL "memoryUnpackString"
    , unpush 2
    , OP MOV $ RR rax newString
    , OP CMP $ IR 0 strStart
    , JUMP JNE skipStartUpdate
    , OP MOV $ RR newString strStart
    , JUMP JMP skipEndUpdate
    , LABEL skipStartUpdate
    , OP MOV $ RM newString (deref strEnd)
    , LABEL skipEndUpdate
    , OP MOV $ RR bytesRead allocedLength
    , OP IMUL $ IR 24 allocedLength
    , LEA (Mem (Right $ -8) newString (Just (Scale1, allocedLength))) strEnd
    , JUMP JMP readStart
    , LABEL readDone
    , OP MOV $ IR 3 rax
    , OP MOV $ RR fd rdi
    , SYSCALL 1  -- close
    , OP CMP $ IR 0 rax
    , JUMP JL crash
    , OP MOV $ RR strStart rax
    , RET
    , LABEL crash
    , LEA (memLabel "msgReadFileFailed") msg
    , UN PUSH $ R msg
    , JUMP CALL "crash"
    ]

primitiveError :: Stateful VirtualFunction
primitiveError = return $ function
  "error__uncurried"
  [ UN PUSH $ M (getArg 1)
  , JUMP CALL "memoryPackString"
  , unpush 1
  , UN PUSH $ R rax
  , JUMP CALL "crash"
  ]

compareOp :: String -> Jump -> Stateful VirtualFunction
compareOp name op = do
  temp <- newTemp
  yes  <- newLabel
  return $ function
    name
    [ OP MOV $ MR (getArg 2) temp
    , OP CMP $ MR (getArg 1) temp
    , JUMP op yes
    , OP MOV $ IR 0 rax
    , RET
    , LABEL yes
    , OP MOV $ IR 1 rax
    , RET
    ]

equal :: Stateful VirtualFunction
equal = compareOp "equal__uncurried" JE

notEqual :: Stateful VirtualFunction
notEqual = compareOp "notEqual__uncurried" JNE

lessThan :: Stateful VirtualFunction
lessThan = compareOp "lessThan__uncurried" JL

lessThanEqual :: Stateful VirtualFunction
lessThanEqual = compareOp "lessThanEqual__uncurried" JLE

greaterThan :: Stateful VirtualFunction
greaterThan = compareOp "greaterThan__uncurried" JG

greaterThanEqual :: Stateful VirtualFunction
greaterThanEqual = compareOp "greaterThanEqual__uncurried" JGE

monadReturn :: Stateful VirtualFunction
monadReturn = return $ function "return__uncurried__unmonadified"
                                [OP MOV $ MR (getArg 1) rax, RET]

monadBind :: Stateful VirtualFunction
monadBind = do
  firstMonad     <- newTemp
  secondMonad    <- newTemp
  arg            <- newTemp
  fn             <- newTemp
  firstCallCode  <- translateCall firstMonad Nothing
  secondCallCode <- translateCall fn (Just arg)
  thirdCallCode  <- translateCall secondMonad Nothing
  return $ function
    "bind__uncurried__unmonadified"
    (  [OP MOV $ MR (getArg 2) firstMonad]
    ++ firstCallCode
    ++ [OP MOV $ RR rax arg, OP MOV $ MR (getArg 1) fn]
    ++ secondCallCode
    ++ [OP MOV $ RR rax secondMonad]
    ++ thirdCallCode
    ++ [RET]
    )

primitiveCrash :: Stateful VirtualFunction
primitiveCrash = do
  msg <- newTemp
  return $ function
    "crash"
    [ OP MOV $ MR (getArg 1) msg
    , OP MOV $ IR 1 rax
    , OP MOV $ IR 2 rdi
    , LEA (Mem (Right 8) msg Nothing) rsi
    , OP MOV $ MR (deref msg) rdx
    , SYSCALL 3 -- write
    , OP MOV $ IR 60 rax
    , OP MOV $ IR 1 rdi
    , SYSCALL 1 -- exit
    ]

monadify :: Int -> String -> Stateful VirtualFunction
monadify numArgs fnName = do
  fnPtr <- newTemp
  arg   <- newTemp
  return $ function
    fnName
    (  [ PUSHI (fromIntegral $ (numArgs + 2) * 8)
       , JUMP CALL "memoryAlloc"
       , unpush 1
       , LEA (memLabel $ fnName ++ "__unmonadified") fnPtr
       , OP MOV $ RM fnPtr (getField 0 rax)
       , OP MOV $ IM (fromIntegral numArgs) (getField 1 rax)
       ]
    ++ concatMap
         (\i ->
           [ OP MOV $ MR (getArg $ numArgs + 1 - i) arg
           , OP MOV $ RM arg (getField (i + 1) rax)
           ]
         )
         [1 .. numArgs]
    ++ [RET]
    )

primitiveTrace :: Stateful VirtualFunction
primitiveTrace = return $ function
  "trace__uncurried"
  [ UN PUSH $ M (getArg 2)
  , JUMP CALL "print__uncurried__unmonadified"
  , unpush 1
  , OP MOV $ MR (getArg 1) rax
  , RET
  ]
