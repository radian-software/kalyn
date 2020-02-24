module Assembly where

import qualified Data.ByteString.Lazy          as B
import           Data.Word

{-# ANN module "HLint: ignore Use lambda-case" #-}

class RegisterLike reg where
  fromRegister :: Register -> reg

data Register = RAX | RBX | RCX | RDX
              | RDI | RSI | R8  | R9
              | R10 | R11 | R12 | R13
              | R14 | R15 | RSP | RBP
  deriving (Show)

newtype Temporary = Temporary String

type VirtualRegister = Either Temporary Register

instance RegisterLike Register where
  fromRegister = id

instance RegisterLike b => RegisterLike (Either a b) where
  fromRegister = Right . fromRegister

newtype LName = LName String
  deriving (Eq, Ord, Show)

-- reg is either Register or VisualRegister
data Instruction reg = MOV_IR Word32 reg
                     | LEA_LR LName reg
                     | SYSCALL Int

getRegisters :: RegisterLike reg => Instruction reg -> [reg]
getRegisters (MOV_IR _ reg) = [reg]
getRegisters (LEA_LR _ reg) = [reg]
getRegisters (SYSCALL n   ) = map fromRegister $ take
  (n + 1)
  [ RAX
  , RDI
  , RSI
  , RDX
  , RCX
  , R8
  , R9
  , error "too many arguments for a system call"
  ]

type Function reg = [(Maybe LName, Instruction reg)]
type Datum = (LName, B.ByteString)

data Program reg = Program [Function reg] [Datum]
