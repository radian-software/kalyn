module Assembly where

import qualified Data.ByteString.Lazy          as B
import           Data.Word

import           Util

{-# ANN module "HLint: ignore Use lambda-case" #-}

data Register = RAX | RBX | RCX | RDX
              | RDI | RSI | R8  | R9
              | R10 | R11 | R12 | R13
              | R14 | R15 | RSP | RBP
  deriving (Show)

newtype Temporary = Temporary String

type VirtualRegister = Either Temporary Register

newtype LName = LName String
  deriving (Eq, Ord, Show)

-- reg is either Register or VisualRegister
data Instruction reg = MOV_IR Word32 reg
                     | LEA_LR LName reg
                     | SYSCALL

getRegisters :: Instruction reg -> [reg]
getRegisters (MOV_IR _ reg) = [reg]
getRegisters (LEA_LR _ reg) = [reg]
getRegisters SYSCALL        = []

data UnallocatedProgram = UnallocatedProgram
  [[(Maybe LName, Instruction VirtualRegister)]]
  [(LName, B.ByteString)]

data AllocatedProgram = AllocatedProgram
  [[(Maybe LName, Instruction Register)]]
  [(LName, B.ByteString)]
