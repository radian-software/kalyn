module Assembly where

import qualified Data.ByteString.Lazy          as B
import           Data.Int
import           Numeric

{-# ANN module "HLint: ignore Use lambda-case" #-}

class RegisterLike reg where
  fromRegister :: Register -> reg

data Register = RAX | RCX | RDX | RBX
              | RSP | RBP | RSI | RDI
              | R8  | R9  | R10 | R11
              | R12 | R13 | R14 | R15
  deriving (Eq)

instance Show Register where
  show RAX = "%rax"
  show RCX = "%rcx"
  show RDX = "%rdx"
  show RBX = "%rbx"
  show RSP = "%rsp"
  show RBP = "%rbp"
  show RSI = "%rsi"
  show RDI = "%rdi"
  show R8  = "%r8"
  show R9  = "%r9"
  show R10 = "%r10"
  show R11 = "%r11"
  show R12 = "%r12"
  show R13 = "%r13"
  show R14 = "%r14"
  show R15 = "%r15"

newtype Temporary = Temporary String

instance Show Temporary where
  show (Temporary name) = name

data VirtualRegister = Physical Register | Virtual Temporary

instance Show VirtualRegister where
  show (Physical reg ) = show reg
  show (Virtual  temp) = show temp

instance RegisterLike Register where
  fromRegister = id

instance RegisterLike VirtualRegister where
  fromRegister = Physical

newtype Label = Label String
  deriving (Eq, Ord)

instance Show Label where
  show (Label name) = name

data Scale = Scale1 | Scale2 | Scale4 | Scale8

data Mem reg = Mem Int32 reg (Maybe (Scale, reg))

data Args' imm reg = IR imm reg
                   | IM imm (Mem reg)
                   | RR reg reg
                   | MR (Mem reg) reg
                   | RM reg (Mem reg)
                   | LR Label reg

type Args = Args' Int32
type Args64 = Args' Int64

data Op = MOV | ADD | SUB | IMUL | AND | OR | XOR | CMP

fromImm :: Args' imm reg -> Bool
fromImm (IR _ _) = True
fromImm (IM _ _) = True
fromImm _        = False

fromMem :: Args' imm reg -> Bool
fromMem (MR _ _) = True
fromMem _        = False

-- reg is either Register or VisualRegister. We use AT&T syntax.
data Instruction reg = OP Op (Args reg)
                     | LEA (Mem reg) reg
                     | LEAL Label reg
                     | MOV64 Int64 reg
                     | CQTO
                     | IDIV reg
                     | JE Label
                     | JNE Label
                     | JL Label
                     | JLE Label
                     | JG Label
                     | JGE Label
                     | JB Label
                     | JBE Label
                     | JA Label
                     | JAE Label
                     | PUSH reg
                     | POP reg
                     | CALL Int Label
                     | RET
                     | SYSCALL Int

instance Show Scale where
  show Scale1 = "1"
  show Scale2 = "2"
  show Scale4 = "4"
  show Scale8 = "8"

instance Show reg => Show (Mem reg) where
  show (Mem disp base msi) =
    (if disp == 0 then "" else show disp)
      ++ "("
      ++ show base
      ++ (case msi of
           Nothing              -> ""
           Just (Scale1, index) -> ", " ++ show index
           Just (scale , index) -> ", " ++ show index ++ ", " ++ show scale
         )
      ++ ")"

instance (Show imm, Show reg) => Show (Args' imm reg) where
  show (IR imm   reg) = "$" ++ show imm ++ ", " ++ show reg
  show (IM imm   mem) = "$" ++ show imm ++ ", " ++ show mem
  show (RR src   dst) = show src ++ ", " ++ show dst
  show (MR mem   reg) = show mem ++ ", " ++ show reg
  show (RM reg   mem) = show reg ++ ", " ++ show mem
  show (LR label reg) = "$" ++ show label ++ "(%rip), " ++ show reg

instance Show Op where
  show MOV  = "movq"
  show ADD  = "addq"
  show SUB  = "subq"
  show IMUL = "imulq"
  show AND  = "andq"
  show OR   = "orq"
  show XOR  = "xorq"
  show CMP  = "cmpq"

instance Show reg => Show (Instruction reg) where
  show (OP    op    args) = show op ++ " " ++ show args
  show (LEA   src   dst ) = "leaq " ++ show src ++ ", " ++ show dst
  show (LEAL  label dst ) = "leaq " ++ show label ++ ", " ++ show dst
  show (MOV64 imm   dst ) = "movq $" ++ show imm ++ ", " ++ show dst
  show CQTO               = "cqto"
  show (IDIV src    )     = "idivq " ++ show src
  show (JE   label  )     = "je " ++ show label
  show (JNE  label  )     = "jne " ++ show label
  show (JL   label  )     = "jl " ++ show label
  show (JLE  label  )     = "jle " ++ show label
  show (JG   label  )     = "jg " ++ show label
  show (JGE  label  )     = "jge " ++ show label
  show (JB   label  )     = "jb " ++ show label
  show (JBE  label  )     = "jbe " ++ show label
  show (JA   label  )     = "ja " ++ show label
  show (JAE  label  )     = "jae " ++ show label
  show (PUSH src    )     = "pushq " ++ show src
  show (POP  dst    )     = "popq " ++ show dst
  show (CALL _ label)     = "callq " ++ show label
  show RET                = "retq"
  show (SYSCALL _)        = "syscall"

argumentRegisters :: [Register]
argumentRegisters = [RDI, RSI, RDX, RCX, R8, R9]

argumentRegistersOnly :: [Register]
argumentRegistersOnly =
  [RDI, RSI, RDX, RCX, R8, R9, error "too many arguments"]

callerSavedRegisters :: [Register]
callerSavedRegisters = [RAX, RCX, RDX, RDI, RSI, R8, R9, R10, R11]

getMemRegisters :: Mem reg -> [reg]
getMemRegisters (Mem _ base Nothing          ) = [base]
getMemRegisters (Mem _ base (Just (_, index))) = [base, index]

getArgRegisters :: Args' imm reg -> ([reg], [reg])
getArgRegisters (IR _   dst) = ([], [dst])
getArgRegisters (IM _   mem) = (getMemRegisters mem, [])
getArgRegisters (RR src dst) = ([src], [dst])
getArgRegisters (MR mem dst) = (getMemRegisters mem, [dst])
getArgRegisters (RM src mem) = (src : getMemRegisters mem, [])
getArgRegisters (LR _   dst) = ([], [dst])

-- returns (src, dst)
getRegisters :: RegisterLike reg => Instruction reg -> ([reg], [reg])
getRegisters (OP    _   args) = getArgRegisters args
getRegisters (LEA   mem dst ) = (getMemRegisters mem, [dst])
getRegisters (LEAL  _   dst ) = ([], [dst])
getRegisters (MOV64 _   dst ) = ([], [dst])
getRegisters CQTO             = ([], [])
getRegisters (IDIV src)       = ([src], [])
getRegisters (JE   _  )       = ([], [])
getRegisters (JNE  _  )       = ([], [])
getRegisters (JL   _  )       = ([], [])
getRegisters (JLE  _  )       = ([], [])
getRegisters (JG   _  )       = ([], [])
getRegisters (JGE  _  )       = ([], [])
getRegisters (JB   _  )       = ([], [])
getRegisters (JBE  _  )       = ([], [])
getRegisters (JA   _  )       = ([], [])
getRegisters (JAE  _  )       = ([], [])
getRegisters (PUSH src)       = ([src], [])
getRegisters (POP  dst)       = ([], [dst])
getRegisters (CALL n _) =
  ( map fromRegister $ take n argumentRegistersOnly
  , map fromRegister callerSavedRegisters
  )
getRegisters RET = ([fromRegister RAX], [])
getRegisters (SYSCALL n) =
  ( map fromRegister $ take (n + 1) (RAX : argumentRegistersOnly)
  , map fromRegister callerSavedRegisters
  )

type Function reg = [(Instruction reg, Maybe Label)]

type Datum = (Label, B.ByteString)

data Program reg = Program [Function reg] [Datum]

instance Show reg => Show (Program reg) where
  show (Program fns datums) =
    concat
        (flip map fns $ \fn -> concat
          (flip map fn $ \(instr, mlabel) ->
            (case mlabel of
                Nothing    -> ""
                Just label -> show label ++ ":\n"
              )
              ++ "\t"
              ++ show instr
              ++ "\n"
          )
        )
      ++ concat
           (flip map datums $ \(label, datum) -> show label ++ ":\n" ++ concat
             ( flip map (B.unpack datum)
             $ \byte -> "\t.byte 0x" ++ showHex byte "" ++ "\n"
             )
           )
