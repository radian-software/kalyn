module Subroutines where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           Data.Maybe

import           Assembly
import           OS

getField :: Int -> VirtualRegister -> Mem VirtualRegister
getField n reg = Mem (Right $ fromIntegral $ 8 * n) reg Nothing

deref :: VirtualRegister -> Mem VirtualRegister
deref = getField 0

unpush :: Int -> VirtualInstruction
unpush n = OP ADD $ IR (fromIntegral $ 8 * n) rsp

-- warning: gets arguments in reverse order! indexed from 1
getArg :: Int -> Mem VirtualRegister
getArg n = getField (n + 1) rbp

translateCall
  :: VirtualRegister -> Maybe VirtualRegister -> Stateful [VirtualInstruction]
translateCall lhsTemp rhsTemp = do
  argPtr    <- newTemp
  argsLeft  <- newTemp
  popAmt    <- newTemp
  pushStart <- newLabel
  pushDone  <- newLabel
  return
    $  [ OP MOV $ MR (getField 1 lhsTemp) argsLeft
       , LEA (getField 2 lhsTemp) argPtr
       , LABEL pushStart
       , OP CMP $ IR 0 argsLeft
       , JUMP JLE pushDone
       , UN PUSH $ M (deref argPtr)
       , OP ADD $ IR 8 argPtr
       , UN DEC $ R argsLeft
       , JUMP JMP pushStart
       , LABEL pushDone
       ]
    ++ (case rhsTemp of
         Nothing   -> []
         Just temp -> [UN PUSH $ R temp]
       )
    ++ [ UN ICALL $ M (getField 0 lhsTemp)
       , OP MOV $ MR (getField 1 lhsTemp) popAmt
       ]
    ++ [ LEA
           (Mem (Right (if isJust rhsTemp then 8 else 0))
                rsp
                (Just (Scale8, popAmt))
           )
           rsp
       ]

curryify :: Int -> String -> Stateful [VirtualFunction]
curryify numArgs fnName = do
  if numArgs >= 1
    then return ()
    else error "can't curry a function with no arguments"
  topFn <- do
    fnPtr     <- newTemp
    nextFnPtr <- newTemp
    return $ function
      fnName
      [ PUSHI 16
      , JUMP CALL "memoryAlloc"
      , unpush 1
      , OP MOV $ RR rax fnPtr
      , LEA
        (  memLabel
        $  fnName
        ++ (if numArgs >= 2 then "__curried0" else "__uncurried")
        )
        nextFnPtr
      , OP MOV $ RM nextFnPtr (getField 0 fnPtr)
      , OP MOV $ IM 0 (getField 1 fnPtr)
      , OP MOV $ RR fnPtr rax
      , RET
      ]
  subFns <- mapM
    (\numCurried -> do
      fnPtr     <- newTemp
      nextFnPtr <- newTemp
      arg       <- newTemp
      let curFnName = fnName ++ "__curried" ++ show numCurried
      let nextFnName = if numCurried == numArgs - 2
            then fnName ++ "__uncurried"
            else fnName ++ "__curried" ++ show (numCurried + 1)
      return $ function
        curFnName
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
               [ OP MOV $ MR (getArg $ numCurried + 2 - i) arg
               , OP MOV $ RM arg (getField (i + 1) fnPtr)
               ]
             )
             [1 .. numCurried + 1]
        ++ [OP MOV $ RR fnPtr rax, RET]
        )
    )
    [0 .. numArgs - 2]
  return . reverse $ topFn : subFns

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

packString :: Stateful VirtualFunction
packString = do
  arg         <- newTemp
  ptr         <- newTemp
  strLength   <- newTemp
  allocLength <- newTemp
  result      <- newTemp
  mptr        <- newTemp
  temp        <- newTemp
  zero        <- newTemp
  lengthStart <- newLabel
  lengthDone  <- newLabel
  copyStart   <- newLabel
  copyDone    <- newLabel
  return $ function
    "packString"
    [ OP MOV $ MR (getArg 1) arg
    , OP MOV $ IR 0 strLength
    , OP MOV $ RR arg ptr
    , LABEL lengthStart
    , OP CMP $ IM 0 (getField 0 ptr)
    , JUMP JE lengthDone
    , UN INC $ R strLength
    , OP MOV $ MR (getField 2 ptr) ptr
    , JUMP JMP lengthStart
    , LABEL lengthDone
    , LEA (Mem (Right 9) strLength Nothing) allocLength
    , UN PUSH $ R allocLength
    , JUMP CALL "memoryAlloc"
    , unpush 1
    , OP MOV $ RR rax result
    , OP MOV $ RM strLength (deref rax)
    , LEA (getField 1 rax) mptr
    , OP MOV $ RR arg ptr
    , LABEL copyStart
    , OP CMP $ IM 0 (getField 0 ptr)
    , JUMP JE copyDone
    , OP MOV $ MR (getField 1 ptr) temp
    , MOVBRM temp (deref mptr)
    , OP MOV $ MR (getField 2 ptr) ptr
    , UN INC $ R mptr
    , JUMP JMP copyStart
    , LABEL copyDone
    , OP MOV $ IR 0 zero
    , MOVBRM zero (deref mptr)
    , OP MOV $ RR result rax
    , RET
    ]

unpackString :: Stateful VirtualFunction
unpackString = do
  str       <- newTemp
  strptr    <- newTemp
  allocSize <- newTemp
  retval    <- newTemp
  bufPtr    <- newTemp
  lstPtr    <- newTemp
  lstEnd    <- newTemp
  char      <- newTemp
  next      <- newTemp
  lenStart  <- newLabel
  lenDone   <- newLabel
  copyStart <- newLabel
  copyDone  <- newLabel
  return $ function
    "unpackString"
    [ OP MOV $ MR (getArg 2) str
    , OP MOV $ MR (getArg 1) allocSize
    , OP CMP $ IR 0 allocSize
    , JUMP JGE lenDone
    , OP MOV $ RR str strptr
    , OP MOV $ IR 0 allocSize
    , LABEL lenStart
    , OP CMP $ IM 0 (deref strptr)
    , JUMP JE lenDone
    , UN INC $ R allocSize
    , UN INC $ R strptr
    , LABEL lenDone
    , OP IMUL $ IR 24 allocSize
    , OP ADD $ IR 8 allocSize
    , UN PUSH (R allocSize)
    , JUMP CALL "memoryAlloc"
    , unpush 1
    , OP MOV $ RR rax retval
    , OP MOV $ RR str bufPtr
    , OP MOV $ RR rax lstPtr
    , LEA (Mem (Right $ -8) lstPtr (Just (Scale1, allocSize))) lstEnd
    , LABEL copyStart
    , OP CMP $ RR lstEnd lstPtr
    , JUMP JGE copyDone
    , OP MOV $ IM 1 (getField 0 lstPtr)
    , OP MOV $ IR 0 char
    , MOVBMR (deref bufPtr) char
    , OP MOV $ RM char (getField 1 lstPtr)
    , LEA (getField 3 lstPtr) next
    , OP MOV $ RM next (getField 2 lstPtr)
    , UN INC (R bufPtr)
    , OP ADD $ IR 24 lstPtr
    , JUMP JMP copyStart
    , LABEL copyDone
    , OP MOV $ IM 0 (deref lstPtr)
    , OP MOV $ RR retval rax
    , RET
    ]

packMsg :: String -> B.ByteString
packMsg str =
  toLazyByteString $ int64LE (fromIntegral $ length str) <> stringUtf8 str

msgDatums :: [Datum]
msgDatums =
  [ ("msgPatternMatchFailed"       , packMsg "pattern match failed\n")
  , ("msgMemoryAllocFailed"        , packMsg "memoryAlloc failed\n")
  , ("msgWriteFileFailed"          , packMsg "writeFile failed\n")
  , ("msgSetFileModeFailed"        , packMsg "setFileMode failed\n")
  , ("msgReadFileFailed"           , packMsg "readFile failed\n")
  , ("msgGetWorkingDirectoryFailed", packMsg "getWorkingDirectory failed\n")
  ]

syscallBuffer :: Datum
syscallBuffer = ("syscallBuffer", B.pack (replicate syscallBufferSize 0))
