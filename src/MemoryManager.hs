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
    -- reserve 1000 more pages while we're at it
    , OP ADD $ IR (fromIntegral $ pageSize * 1000) firstFree
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
