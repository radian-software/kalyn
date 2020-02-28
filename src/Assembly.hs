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

-- reg is either Register or VisualRegister. We use AT&T syntax.
data Instruction reg = MOV_IR Int32 reg
                     | MOV_RR reg reg
                     | ADD_IR Int32 reg
                     | ADD_RR reg reg
                     | SUB_IR Int32 reg
                     | SUB_RR reg reg
                     | IMUL_IR Int32 reg
                     | IMUL_RR reg reg
                     | CQTO
                     | IDIV reg
                     | AND_IR Int32 reg
                     | AND_RR reg reg
                     | OR_IR Int32 reg
                     | OR_RR reg reg
                     | XOR_IR Int32 reg
                     | XOR_RR reg reg
                     | CMP_IR Int32 reg
                     | CMP_RR reg reg
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
                     | LEA_LR Label reg
                     | SYSCALL Int
                     | CALL Int Label
                     | RET

instance Show reg => Show (Instruction reg) where
  show (MOV_IR  val dst)  = "movq $" ++ show val ++ ", " ++ show dst
  show (MOV_RR  src dst)  = "movq " ++ show src ++ ", " ++ show dst
  show (ADD_IR  val dst)  = "addq $" ++ show val ++ ", " ++ show dst
  show (ADD_RR  src dst)  = "addq " ++ show src ++ ", " ++ show dst
  show (SUB_IR  val dst)  = "subq $" ++ show val ++ ", " ++ show dst
  show (SUB_RR  src dst)  = "subq " ++ show src ++ ", " ++ show dst
  show (IMUL_IR val dst)  = "imulq $" ++ show val ++ ", " ++ show dst
  show (IMUL_RR src dst)  = "imulq " ++ show src ++ ", " ++ show dst
  show CQTO               = "cqto"
  show (IDIV src        ) = "idivq " ++ show src
  show (AND_IR val  dst ) = "andq $" ++ show val ++ ", " ++ show dst
  show (AND_RR src  dst ) = "andq " ++ show src ++ ", " ++ show dst
  show (OR_IR  val  dst ) = "orq $" ++ show val ++ ", " ++ show dst
  show (OR_RR  src  dst ) = "orq " ++ show src ++ ", " ++ show dst
  show (XOR_IR val  dst ) = "xorq $" ++ show val ++ ", " ++ show dst
  show (XOR_RR src  dst ) = "xorq " ++ show src ++ ", " ++ show dst
  show (CMP_IR src2 src1) = "cmpq $" ++ show src2 ++ ", " ++ show src1
  show (CMP_RR val  src ) = "cmpq " ++ show val ++ ", " ++ show src
  show (JE   label      ) = "je " ++ show label
  show (JNE  label      ) = "jne " ++ show label
  show (JL   label      ) = "jl " ++ show label
  show (JLE  label      ) = "jle " ++ show label
  show (JG   label      ) = "jg " ++ show label
  show (JGE  label      ) = "jge " ++ show label
  show (JB   label      ) = "jb " ++ show label
  show (JBE  label      ) = "jbe " ++ show label
  show (JA   label      ) = "ja " ++ show label
  show (JAE  label      ) = "jae " ++ show label
  show (PUSH src        ) = "pushq " ++ show src
  show (POP  dst        ) = "popq " ++ show dst
  show (LEA_LR label dst) = "leaq " ++ show label ++ "(%rip), " ++ show dst
  show (SYSCALL _       ) = "syscall"
  show (CALL _ label    ) = "callq " ++ show label
  show RET                = "retq"

argumentRegisters :: [Register]
argumentRegisters = [RDI, RSI, RDX, RCX, R8, R9]

argumentRegistersOnly :: [Register]
argumentRegistersOnly =
  [RDI, RSI, RDX, RCX, R8, R9, error "too many arguments"]

callerSavedRegisters :: [Register]
callerSavedRegisters = [RAX, RCX, RDX, RDI, RSI, R8, R9, R10, R11]

-- returns (src, dst)
getRegisters :: RegisterLike reg => Instruction reg -> ([reg], [reg])
getRegisters (MOV_IR  _   dst)  = ([], [dst])
getRegisters (MOV_RR  src dst)  = ([src], [dst])
getRegisters (ADD_IR  _   dst)  = ([], [dst])
getRegisters (ADD_RR  src dst)  = ([src], [dst])
getRegisters (SUB_IR  _   dst)  = ([], [dst])
getRegisters (SUB_RR  src dst)  = ([src], [dst])
getRegisters (IMUL_IR _   dst)  = ([], [dst])
getRegisters (IMUL_RR src dst)  = ([src], [dst])
getRegisters CQTO               = ([], [])
getRegisters (IDIV src        ) = ([src], [])
getRegisters (AND_IR _    dst ) = ([], [dst])
getRegisters (AND_RR src  dst ) = ([src], [dst])
getRegisters (OR_IR  _    dst ) = ([], [dst])
getRegisters (OR_RR  src  dst ) = ([src], [dst])
getRegisters (XOR_IR _    dst ) = ([], [dst])
getRegisters (XOR_RR src  dst ) = ([src], [dst])
getRegisters (CMP_IR _    src ) = ([src], [])
getRegisters (CMP_RR src2 src1) = ([src1, src2], [])
getRegisters (JE   _          ) = ([], [])
getRegisters (JNE  _          ) = ([], [])
getRegisters (JL   _          ) = ([], [])
getRegisters (JLE  _          ) = ([], [])
getRegisters (JG   _          ) = ([], [])
getRegisters (JGE  _          ) = ([], [])
getRegisters (JB   _          ) = ([], [])
getRegisters (JBE  _          ) = ([], [])
getRegisters (JA   _          ) = ([], [])
getRegisters (JAE  _          ) = ([], [])
getRegisters (PUSH src        ) = ([src], [])
getRegisters (POP  dst        ) = ([], [dst])
getRegisters (LEA_LR _ src    ) = ([src], [])
getRegisters (SYSCALL n) =
  ( map fromRegister $ take (n + 1) (RAX : argumentRegistersOnly)
  , map fromRegister callerSavedRegisters
  )
getRegisters (CALL n _) =
  ( map fromRegister $ take n argumentRegistersOnly
  , map fromRegister callerSavedRegisters
  )
getRegisters RET = ([fromRegister RAX], [])

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
