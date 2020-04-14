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

memoryInit :: Stateful (Function VirtualRegister)
memoryInit = do
  temp <- newTemp
  return $ function
    "memoryInit"
    [ Right
        [ OP MOV $ IR 12 rax
        , OP MOV $ IR 0 rdi
        , SYSCALL 1 -- brk
        , OP MOV $ RM rax (memLabel "mmProgramBreak")
        , LEA (memLabel "heap") temp
        , OP MOV $ RM temp (memLabel $ fst memoryFirstFree)
        , RET
        ]
    ]

memoryAlloc :: Stateful (Function VirtualRegister)
memoryAlloc = do
  firstFree <- newTemp
  ptr       <- newTemp
  brk       <- newLabel
  done      <- newLabel
  return $ function
    "memoryAlloc"
    [ Right
      [ OP MOV $ MR (memLabel "mmFirstFree") firstFree
      -- round up to nearest multiple of eight, see
      -- <https://stackoverflow.com/a/9194117/3538165>
      , OP ADD $ IR 7 firstFree
      , OP AND $ IR (-7) firstFree
      -- now to proceed
      , OP MOV $ RR firstFree ptr
      , OP ADD $ MR (getArg 1) firstFree
      , OP MOV $ RM firstFree (memLabel "mmFirstFree")
      , OP CMP $ MR (memLabel "mmProgramBreak") firstFree
      , JG brk
      ]
    , Left done
    , Right [OP MOV $ RR ptr rax, RET]
    , Left brk
    , Right
      [ OP ADD $ IR (fromIntegral pageSize) firstFree
      , OP MOV $ IR 12 rax
      , OP MOV $ RR firstFree rdi
      , SYSCALL 1 -- brk
      , OP CMP $ RR firstFree rax
      , JLE "crash"
      , OP MOV $ RM rax (memLabel "mmProgramBreak")
      , JMP done
      ]
    ]

memoryPackString :: Stateful (Function VirtualRegister)
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
    [ Right
      [OP MOV $ MR (getArg 1) arg, OP MOV $ IR 0 strLength, OP MOV $ RR arg ptr]
    , Left lengthStart
    , Right
      [ OP CMP $ IM 0 (getField 0 ptr)
      , JE lengthDone
      , INC strLength
      , OP MOV $ MR (getField 2 ptr) ptr
      , JMP lengthStart
      ]
    , Left lengthDone
    , Right
      [ LEA (Mem (Right 1) strLength Nothing) allocLength
      , PUSH allocLength
      , CALL "memoryAlloc"
      , OP MOV $ RR rax result
      , OP MOV $ RM strLength (deref rax)
      , LEA (getField 1 rax) mptr
      , OP MOV $ RR arg ptr
      ]
    , Left copyStart
    , Right
      [ OP CMP $ IM 0 (getField 0 ptr)
      , JE copyDone
      , OP MOV $ MR (getField 1 ptr) temp
      , OP MOV $ RM temp (deref mptr)
      , OP MOV $ MR (getField 2 ptr) ptr
      , INC mptr
      ]
    , Left copyDone
    , Right [OP MOV $ RR result rax, RET]
    ]
