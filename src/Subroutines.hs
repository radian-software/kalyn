module Subroutines where

import qualified Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Builder
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
