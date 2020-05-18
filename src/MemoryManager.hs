module MemoryManager where

import           Data.ByteString.Lazy.Builder

import           Assembly
import           OS
import           Subroutines

memoryFirstFree :: Datum
memoryFirstFree = ("mmFirstFree", toLazyByteString $ word64LE 0)

memoryProgramBreak :: Datum
memoryProgramBreak = ("mmProgramBreak", toLazyByteString $ word64LE 0)

memoryInit :: Stateful VirtualFunction
memoryInit = return $ function
  "memoryInit"
  [ OP MOV $ IR 12 rax
  , OP MOV $ IR 0 rdi
  , SYSCALL 1 -- brk
  , OP MOV $ RM rax (memLabel $ fst memoryProgramBreak)
  , OP MOV $ RM rax (memLabel $ fst memoryFirstFree)
  , RET
  ]

memoryAlloc :: Stateful VirtualFunction
memoryAlloc = do
  firstFree <- newTemp
  ptr       <- newTemp
  brk       <- newLabel
  done      <- newLabel
  crash     <- newLabel
  msg       <- newTemp
  return $ function
    "memoryAlloc"
    [ OP MOV $ MR (memLabel "mmFirstFree") firstFree
    -- round up to nearest multiple of eight, see
    -- <https://stackoverflow.com/a/9194117/3538165>
    , OP ADD $ IR 7 firstFree
    , OP AND $ IR (-8) firstFree
    -- now to proceed
    , OP MOV $ RR firstFree ptr
    , OP ADD $ MR (getArg 1) firstFree
    , OP MOV $ RM firstFree (memLabel "mmFirstFree")
    , OP CMP $ MR (memLabel "mmProgramBreak") firstFree
    , JUMP JG brk
    , LABEL done
    , OP MOV $ RR ptr rax
    , RET
    , LABEL brk
    -- round up to next page boundary
    , OP ADD $ IR (fromIntegral $ pageSize - 1) firstFree
    , OP AND $ IR (fromIntegral $ -pageSize) firstFree
    , OP MOV $ IR 12 rax
    , OP MOV $ RR firstFree rdi
    , SYSCALL 1 -- brk
    , OP CMP $ RR firstFree rax
    , JUMP JL crash
    , OP MOV $ RM rax (memLabel "mmProgramBreak")
    , JUMP JMP done
    , LABEL crash
    , LEA (memLabel "msgMemoryAllocFailed") msg
    , UN PUSH $ R msg
    , JUMP CALL "crash"
    ]

memoryPackString :: Stateful VirtualFunction
memoryPackString = do
  arg         <- newTemp
  ptr         <- newTemp
  strLength   <- newTemp
  allocLength <- newTemp
  result      <- newTemp
  mptr        <- newTemp
  temp        <- newTemp
  lengthStart <- newLabel
  lengthDone  <- newLabel
  copyStart   <- newLabel
  copyDone    <- newLabel
  return $ function
    "memoryPackString"
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
    , OP MOV $ RM temp (deref mptr)
    , OP MOV $ MR (getField 2 ptr) ptr
    , UN INC $ R mptr
    , JUMP JMP copyStart
    , LABEL copyDone
    , OP MOV $ IM 0 (deref mptr)
    , OP MOV $ RR result rax
    , RET
    ]

memoryUnpackString :: Stateful VirtualFunction
memoryUnpackString = do
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
    "memoryUnpackString"
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
