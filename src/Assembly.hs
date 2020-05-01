module Assembly where

import qualified Data.ByteString.Lazy          as B
import           Data.Int
import           Data.List
import           Data.Word
import           Numeric

{-# ANN module "HLint: ignore Use lambda-case" #-}
{-# ANN module "HLint: ignore Use tuple-section" #-}

class RegisterLike reg where
  fromRegister :: Register -> reg

data Register = RAX | RCX | RDX | RBX
              | RSP | RBP | RSI | RDI
              | R8  | R9  | R10 | R11
              | R12 | R13 | R14 | R15
              | RIP
  deriving (Eq, Ord)

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
  show RIP = "%rip"

newtype Temporary = Temporary String
  deriving (Eq, Ord)

instance Show Temporary where
  show (Temporary name) = name

data VirtualRegister = Physical Register | Virtual Temporary
  deriving (Eq, Ord)

rax :: RegisterLike reg => reg
rcx :: RegisterLike reg => reg
rdx :: RegisterLike reg => reg
rbx :: RegisterLike reg => reg
rsp :: RegisterLike reg => reg
rbp :: RegisterLike reg => reg
rsi :: RegisterLike reg => reg
rdi :: RegisterLike reg => reg
r8 :: RegisterLike reg => reg
r9 :: RegisterLike reg => reg
r10 :: RegisterLike reg => reg
r11 :: RegisterLike reg => reg
r12 :: RegisterLike reg => reg
r13 :: RegisterLike reg => reg
r14 :: RegisterLike reg => reg
r15 :: RegisterLike reg => reg
rip :: RegisterLike reg => reg

rax = fromRegister RAX
rcx = fromRegister RCX
rdx = fromRegister RDX
rbx = fromRegister RBX
rsp = fromRegister RSP
rbp = fromRegister RBP
rsi = fromRegister RSI
rdi = fromRegister RDI
r8 = fromRegister R8
r9 = fromRegister R9
r10 = fromRegister R10
r11 = fromRegister R11
r12 = fromRegister R12
r13 = fromRegister R13
r14 = fromRegister R14
r15 = fromRegister R15
rip = fromRegister RIP

instance Show VirtualRegister where
  show (Physical reg ) = show reg
  show (Virtual  temp) = show temp

instance RegisterLike Register where
  fromRegister = id

instance RegisterLike VirtualRegister where
  fromRegister = Physical

type Label = String

data Scale = Scale1 | Scale2 | Scale4 | Scale8

data Mem reg = Mem (Either Label Int32) reg (Maybe (Scale, reg))

data Args reg = IR Int32 reg
              | IM Int32 (Mem reg)
              | RR reg reg
              | MR (Mem reg) reg
              | RM reg (Mem reg)

data Op = MOV | ADD | SUB | IMUL | AND | OR | XOR | CMP
  deriving (Eq)

fromImm :: Args reg -> Bool
fromImm (IR _ _) = True
fromImm (IM _ _) = True
fromImm _        = False

fromMem :: Args reg -> Bool
fromMem (MR _ _) = True
fromMem _        = False

data Shift = SHL | SAL | SHR | SAR

-- reg is either Register or VisualRegister. We use AT&T syntax.
data Instruction reg = OP Op (Args reg)
                     | SHIFT (Maybe Word8) Shift reg
                     | LEA (Mem reg) reg
                     | MOV64 Int64 reg
                     | CQTO
                     | IDIV reg
                     | NOT reg
                     | NEG reg
                     | INC reg
                     | DEC reg
                     | JMP Label
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
                     | PUSHI Int32
                     | PUSHM (Mem reg)
                     | POPM (Mem reg)
                     | CALL Label
                     | CALLR reg
                     | CALLM (Mem reg)
                     | RET
                     | SYSCALL Int
                     | LABEL Label

type VirtualInstruction = Instruction VirtualRegister
type PhysicalInstruction = Instruction Register

memLabel :: RegisterLike reg => String -> Mem reg
memLabel name = Mem (Left name) rip Nothing

instance Show Scale where
  show Scale1 = "1"
  show Scale2 = "2"
  show Scale4 = "4"
  show Scale8 = "8"

instance Show reg => Show (Mem reg) where
  show (Mem disp base msi) =
    (case disp of
        Left  label -> show label
        Right 0     -> ""
        Right imm   -> show imm
      )
      ++ "("
      ++ show base
      ++ (case msi of
           Nothing              -> ""
           Just (Scale1, index) -> ", " ++ show index
           Just (scale , index) -> ", " ++ show index ++ ", " ++ show scale
         )
      ++ ")"

instance Show reg => Show (Args reg) where
  show (IR imm reg) = "$" ++ show imm ++ ", " ++ show reg
  show (IM imm mem) = "$" ++ show imm ++ ", " ++ show mem
  show (RR src dst) = show src ++ ", " ++ show dst
  show (MR mem reg) = show mem ++ ", " ++ show reg
  show (RM reg mem) = show reg ++ ", " ++ show mem

instance Show Op where
  show MOV  = "movq"
  show ADD  = "addq"
  show SUB  = "subq"
  show IMUL = "imulq"
  show AND  = "andq"
  show OR   = "orq"
  show XOR  = "xorq"
  show CMP  = "cmpq"

instance Show Shift where
  show SHL = "shl"
  show SAL = "sal"
  show SHR = "shr"
  show SAR = "sar"

instance Show reg => Show (Instruction reg) where
  show (OP op args) = show op ++ " " ++ show args
  show (SHIFT amt shift dst) =
    show shift
      ++ " "
      ++ maybe "%cx" (\val -> "$" ++ show val) amt
      ++ ", "
      ++ show dst
  show (LEA   src dst) = "leaq " ++ show src ++ ", " ++ show dst
  show (MOV64 imm dst) = "movq $" ++ show imm ++ ", " ++ show dst
  show CQTO            = "cqto"
  show (IDIV  src  )   = "idivq " ++ show src
  show (NOT   dst  )   = "not " ++ show dst
  show (NEG   dst  )   = "neg " ++ show dst
  show (INC   dst  )   = "inc " ++ show dst
  show (DEC   dst  )   = "dec " ++ show dst
  show (JMP   label)   = "jmp " ++ show label
  show (JE    label)   = "je " ++ show label
  show (JNE   label)   = "jne " ++ show label
  show (JL    label)   = "jl " ++ show label
  show (JLE   label)   = "jle " ++ show label
  show (JG    label)   = "jg " ++ show label
  show (JGE   label)   = "jge " ++ show label
  show (JB    label)   = "jb " ++ show label
  show (JBE   label)   = "jbe " ++ show label
  show (JA    label)   = "ja " ++ show label
  show (JAE   label)   = "jae " ++ show label
  show (PUSH  src  )   = "pushq " ++ show src
  show (POP   dst  )   = "popq " ++ show dst
  show (PUSHI src  )   = "pushq " ++ show src
  show (PUSHM src  )   = "pushq " ++ show src
  show (POPM  dst  )   = "popq " ++ show dst
  show (CALL  label)   = "callq " ++ show label
  show (CALLR reg  )   = "callq *" ++ show reg
  show (CALLM mem  )   = "callq *" ++ show mem
  show RET             = "retq"
  show (SYSCALL _    ) = "syscall"
  show (LABEL   label) = label ++ ":"

dataRegisters :: [Register]
dataRegisters =
  [RAX, RCX, RDX, RBX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15]

syscallRegisters :: [Register]
syscallRegisters =
  [RAX, RDI, RSI, RDX, RCX, R8, R9, error "too many arguments for system call"]

getMemRegisters :: Mem reg -> [reg]
getMemRegisters (Mem _ base Nothing          ) = [base]
getMemRegisters (Mem _ base (Just (_, index))) = [base, index]

getArgRegisters :: Op -> Args reg -> ([reg], [reg])
getArgRegisters op (IR _ dst) | op == MOV = ([], [dst])
                              | op == CMP = ([dst], [])
                              | otherwise = ([], [dst])
getArgRegisters _ (IM _ mem) = (getMemRegisters mem, [])
getArgRegisters op (RR src dst) | op == MOV = ([src], [dst])
                                | op == CMP = ([src, dst], [])
                                | otherwise = ([src, dst], [dst])
getArgRegisters op (MR mem dst)
  | op == MOV = (getMemRegisters mem, [dst])
  | op == CMP = (getMemRegisters mem, [])
  | otherwise = (dst : getMemRegisters mem, [dst])
getArgRegisters _ (RM src mem) = (src : getMemRegisters mem, [])

-- returns (src, dst)
getRegisters :: RegisterLike reg => Instruction reg -> ([reg], [reg])
getRegisters (OP op args          ) = getArgRegisters op args
getRegisters (SHIFT Nothing  _ dst) = ([dst, fromRegister rcx], [dst])
getRegisters (SHIFT (Just _) _ dst) = ([dst], [dst])
getRegisters (LEA   mem dst       ) = (getMemRegisters mem, [dst])
getRegisters (MOV64 _   dst       ) = ([], [dst])
getRegisters CQTO                   = ([fromRegister rax], [fromRegister rdx])
getRegisters (IDIV src) =
  ( [src, fromRegister rax, fromRegister rdx]
  , [fromRegister rax, fromRegister rdx]
  )
getRegisters (NOT   dst) = ([dst], [dst])
getRegisters (NEG   dst) = ([dst], [dst])
getRegisters (INC   dst) = ([dst], [dst])
getRegisters (DEC   dst) = ([dst], [dst])
getRegisters (JMP   _  ) = ([], [])
getRegisters (JE    _  ) = ([], [])
getRegisters (JNE   _  ) = ([], [])
getRegisters (JL    _  ) = ([], [])
getRegisters (JLE   _  ) = ([], [])
getRegisters (JG    _  ) = ([], [])
getRegisters (JGE   _  ) = ([], [])
getRegisters (JB    _  ) = ([], [])
getRegisters (JBE   _  ) = ([], [])
getRegisters (JA    _  ) = ([], [])
getRegisters (JAE   _  ) = ([], [])
getRegisters (PUSH  src) = ([src], [])
getRegisters (POP   dst) = ([], [dst])
getRegisters (PUSHI _  ) = ([], [])
getRegisters (PUSHM src) = (getMemRegisters src, [])
getRegisters (POPM  dst) = (getMemRegisters dst, [])
getRegisters (CALL  _  ) = ([], map fromRegister dataRegisters)
getRegisters (CALLR reg) = ([reg], map fromRegister dataRegisters)
getRegisters (CALLM mem) =
  (getMemRegisters mem, map fromRegister dataRegisters)
getRegisters RET = ([fromRegister rax], [])
getRegisters (SYSCALL n) =
  ( map fromRegister $ take (n + 1) syscallRegisters
  , map fromRegister syscallRegisters
  )
getRegisters (LABEL _) = ([], [])

data JumpType = Straightline | Jump Label | Branch Label

getJumpType :: Instruction reg -> JumpType
getJumpType (JMP label) = Jump label
getJumpType (JE  label) = Branch label
getJumpType (JNE label) = Branch label
getJumpType (JL  label) = Branch label
getJumpType (JLE label) = Branch label
getJumpType (JG  label) = Branch label
getJumpType (JGE label) = Branch label
getJumpType (JB  label) = Branch label
getJumpType (JBE label) = Branch label
getJumpType (JA  label) = Branch label
getJumpType (JAE label) = Branch label
getJumpType _           = Straightline

mapMem :: (reg1 -> reg2) -> Mem reg1 -> Mem reg2
mapMem f (Mem disp reg msi) = Mem disp (f reg) ((f <$>) <$> msi)

mapArgs :: (reg1 -> reg2) -> Args reg1 -> Args reg2
mapArgs f (IR imm reg) = IR imm (f reg)
mapArgs f (IM imm mem) = IM imm (mapMem f mem)
mapArgs f (RR src dst) = RR (f src) (f dst)
mapArgs f (MR mem reg) = MR (mapMem f mem) (f reg)
mapArgs f (RM reg mem) = RM (f reg) (mapMem f mem)

mapInstr :: (reg1 -> reg2) -> Instruction reg1 -> Instruction reg2
mapInstr f (OP op args         ) = OP op (mapArgs f args)
mapInstr f (SHIFT amt shift reg) = SHIFT amt shift (f reg)
mapInstr f (LEA   mem reg      ) = LEA (mapMem f mem) (f reg)
mapInstr f (MOV64 imm reg      ) = MOV64 imm (f reg)
mapInstr _ CQTO                  = CQTO
mapInstr f (IDIV  reg  )         = IDIV (f reg)
mapInstr f (NOT   reg  )         = NOT (f reg)
mapInstr f (NEG   reg  )         = NEG (f reg)
mapInstr f (INC   reg  )         = INC (f reg)
mapInstr f (DEC   reg  )         = DEC (f reg)
mapInstr _ (JMP   label)         = JMP label
mapInstr _ (JE    label)         = JMP label
mapInstr _ (JNE   label)         = JMP label
mapInstr _ (JL    label)         = JMP label
mapInstr _ (JLE   label)         = JMP label
mapInstr _ (JG    label)         = JMP label
mapInstr _ (JGE   label)         = JMP label
mapInstr _ (JB    label)         = JMP label
mapInstr _ (JBE   label)         = JMP label
mapInstr _ (JA    label)         = JMP label
mapInstr _ (JAE   label)         = JMP label
mapInstr f (PUSH  reg  )         = PUSH (f reg)
mapInstr f (POP   reg  )         = POP (f reg)
mapInstr _ (PUSHI imm  )         = PUSHI imm
mapInstr f (PUSHM mem  )         = PUSHM (mapMem f mem)
mapInstr f (POPM  mem  )         = POPM (mapMem f mem)
mapInstr _ (CALL  label)         = CALL label
mapInstr f (CALLR reg  )         = CALLR (f reg)
mapInstr f (CALLM mem  )         = CALLM (mapMem f mem)
mapInstr _ RET                   = RET
mapInstr _ (SYSCALL n   )        = SYSCALL n
mapInstr _ (LABEL   name)        = LABEL name

spillInstr :: reg -> Mem reg -> Instruction reg -> Instruction reg
spillInstr = undefined

newtype Function reg = Function [Instruction reg]

type VirtualFunction = Function VirtualRegister
type PhysicalFunction = Function Register

instance Show reg => Show (Function reg) where
  show (Function instrs) = concatMap
    (\instr ->
      let str  = show instr
          str' = if ":" `isSuffixOf` str then str else "\t" ++ str
      in  str' ++ "\n"
    )
    instrs

function :: String -> [Instruction reg] -> Function reg
function name = Function . (LABEL name :)

fnInstrs :: Function reg -> [Instruction reg]
fnInstrs (Function instrs) = instrs

mapFunction :: (reg1 -> reg2) -> Function reg1 -> Function reg2
mapFunction f (Function instrs) = Function (map (mapInstr f) instrs)

type Datum = (Label, B.ByteString)

data Program reg = Program (Function reg) [Function reg] [Datum]

instance Show reg => Show (Program reg) where
  show (Program main fns datums) =
    ".text\n.globl main\nmain:\n"
      ++ show main
      ++ concatMap show fns
      ++ ".data\n"
      ++ concat
           (flip map datums $ \(label, datum) -> show label ++ ":\n" ++ concat
             ( flip map (B.unpack datum)
             $ \byte -> "\t.byte 0x" ++ showHex byte "" ++ "\n"
             )
           )
