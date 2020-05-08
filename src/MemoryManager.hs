module MemoryManager where

import qualified Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Builder

import           Assembly
import           OS
import           Subroutines

memoryFirstFree :: Datum
memoryFirstFree = ("mmFirstFree", toLazyByteString $ word64LE 0)

memoryProgramBreak :: Datum
memoryProgramBreak = ("mmProgramBreak", toLazyByteString $ word64LE 0)

heap :: Datum
heap = ("heap", B.empty)

memoryInit :: Stateful VirtualFunction
memoryInit = do
  temp <- newTemp
  return $ function
    "memoryInit"
    [ OP MOV $ IR 12 rax
    , OP MOV $ IR 0 rdi
    , SYSCALL 1 -- brk
    , OP MOV $ RM rax (memLabel "mmProgramBreak")
    , LEA (memLabel "heap") temp
    , OP MOV $ RM temp (memLabel $ fst memoryFirstFree)
    , RET
    ]

memoryAlloc :: Stateful VirtualFunction
memoryAlloc = do
  firstFree <- newTemp
  ptr       <- newTemp
  brk       <- newLabel
  done      <- newLabel
  crash     <- newLabel
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
    , OP ADD $ IR (fromIntegral pageSize) firstFree
    , OP MOV $ IR 12 rax
    , OP MOV $ RR firstFree rdi
    , SYSCALL 1 -- brk
    , OP CMP $ RR firstFree rax
    , JUMP JLE crash
    , OP MOV $ RM rax (memLabel "mmProgramBreak")
    , JUMP JMP done
    , LABEL crash
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
    , LABEL copyDone
    , OP MOV $ IM 0 (deref mptr)
    , OP MOV $ RR result rax
    , RET
    ]
