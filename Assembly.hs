module Assembly where

import qualified Data.ByteString.Lazy          as B
import           Data.Word

data Register = RAX | RBX | RCX | RDX
              | RDI | RSI | R8  | R9
              | R10 | R11 | R12 | R13
              | R14 | R15 | RSP | RBP
  deriving (Show)

newtype LName = LName String
  deriving (Eq, Ord, Show)

data Instruction = MOV_IR Word32 Register
                 | LEA_LR LName Register
                 | SYSCALL
  deriving (Show)

data Section = Text Instruction
             | Data B.ByteString
             | Label LName
  deriving (Show)

newtype Program = Program [Section]
  deriving (Show)
