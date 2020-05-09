module Assembly where

import qualified Data.ByteString.Lazy          as B
import           Data.Int
import           Data.Ord
import           Data.Word
import           Numeric
import           Text.Read

{-# ANN module "HLint: ignore Use lambda-case" #-}
{-# ANN module "HLint: ignore Use tuple-section" #-}

class RegisterLike reg where
  fromRegister :: Register -> reg

-- order is important! for register allocation, try to avoid using
-- system registers because this will likely result in spilling, so
-- put them at the end of the list.
--
-- also we can't use a map for this because it causes infinite
-- recursion, sigh
registerPrecedence :: Register -> Int
registerPrecedence R8  = 0
registerPrecedence R9  = 1
registerPrecedence R10 = 2
registerPrecedence R11 = 3
registerPrecedence R12 = 4
registerPrecedence R13 = 5
registerPrecedence R14 = 6
registerPrecedence R15 = 7
registerPrecedence RBX = 8
registerPrecedence RCX = 9
registerPrecedence RDX = 10
registerPrecedence RSI = 11
registerPrecedence RDI = 12
registerPrecedence RAX = 13
registerPrecedence RSP = 14
registerPrecedence RBP = 15
registerPrecedence RIP = 16

data Register = RAX | RCX | RDX | RBX
              | RSP | RBP | RSI | RDI
              | R8  | R9  | R10 | R11
              | R12 | R13 | R14 | R15
              | RIP
  deriving (Eq)

instance Ord Register where
  compare = comparing registerPrecedence

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
  deriving (Eq)

readTemporary :: Temporary -> Maybe Int
readTemporary (Temporary ('%' : 't' : num)) = readMaybe num
readTemporary _                             = Nothing

instance Ord Temporary where
  t1 <= t2 = case (readTemporary t1, readTemporary t2) of
    (Just n1, Just n2) -> n1 <= n2
    _ ->
      let (Temporary s1) = t1
          (Temporary s2) = t2
      in  s1 <= s2

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

data Arg reg = R reg | M (Mem reg)

data BinOp = MOV | ADD | SUB | IMUL | AND | OR | XOR | CMP
  deriving (Eq)

data UnOp = NOT | NEG | INC | DEC | PUSH | POP | ICALL
  deriving (Eq)

data Jump = JMP | JE | JNE | JL | JLE | JG | JGE | JB | JBE | JA | JAE | CALL

data Shift = SHL | SAL | SHR | SAR

fromImm :: Args reg -> Bool
fromImm (IR _ _) = True
fromImm (IM _ _) = True
fromImm _        = False

fromMem :: Args reg -> Bool
fromMem (MR _ _) = True
fromMem _        = False

-- reg is either Register or VisualRegister. We use AT&T syntax.
data Instruction reg = OP BinOp (Args reg)
                     | UN UnOp (Arg reg)
                     | JUMP Jump Label
                     | MOV64 Int64 reg
                     | SHIFT (Maybe Word8) Shift reg
                     | LEA (Mem reg) reg
                     | IDIV reg
                     | CQTO
                     | PUSHI Int32
                     | RET
                     | SYSCALL Int
                     | LABEL Label
                     | SYMBOL Label

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
        Left  label -> label
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

instance Show reg => Show (Arg reg) where
  show (R reg) = show reg
  show (M mem) = show mem

instance Show BinOp where
  show MOV  = "movq"
  show ADD  = "addq"
  show SUB  = "subq"
  show IMUL = "imulq"
  show AND  = "andq"
  show OR   = "orq"
  show XOR  = "xorq"
  show CMP  = "cmpq"

instance Show UnOp where
  show NOT   = "not"
  show NEG   = "neg"
  show INC   = "inc"
  show DEC   = "dec"
  show PUSH  = "pushq"
  show POP   = "popq"
  show ICALL = "callq"

instance Show Jump where
  show JMP  = "jmp"
  show JE   = "je"
  show JNE  = "jne"
  show JL   = "jl"
  show JLE  = "jle"
  show JG   = "jg"
  show JGE  = "jge"
  show JB   = "jb"
  show JBE  = "jbe"
  show JA   = "ja"
  show JAE  = "jae"
  show CALL = "callq"

instance Show Shift where
  show SHL = "shl"
  show SAL = "sal"
  show SHR = "shr"
  show SAR = "sar"

instance Show reg => Show (Instruction reg) where
  show (OP    op    args ) = show op ++ " " ++ show args
  show (UN    ICALL arg  ) = show ICALL ++ " *" ++ show arg
  show (UN    op    arg  ) = show op ++ " " ++ show arg
  show (JUMP  op    label) = show op ++ " " ++ label
  show (MOV64 imm   dst  ) = "movq $" ++ show imm ++ ", " ++ show dst
  show (SHIFT amt shift dst) =
    show shift
      ++ " "
      ++ maybe "%cx" (\val -> "$" ++ show val) amt
      ++ ", "
      ++ show dst
  show (LEA src dst)  = "leaq " ++ show src ++ ", " ++ show dst
  show (IDIV src   )  = "idivq " ++ show src
  show CQTO           = "cqto"
  show (PUSHI imm)    = "pushq $" ++ show imm
  show RET            = "retq"
  show (SYSCALL _   ) = "syscall"
  show (LABEL   name) = name ++ ":"
  show (SYMBOL  name) = name ++ ":"

allRegisters :: [Register]
allRegisters =
  [ RAX
  , RCX
  , RDX
  , RBX
  , RSP
  , RBP
  , RSI
  , RDI
  , R8
  , R9
  , R10
  , R11
  , R12
  , R13
  , R14
  , R15
  , RIP
  ]

dataRegisters :: [Register]
dataRegisters =
  [RAX, RCX, RDX, RBX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15]

syscallRegisters :: [Register]
syscallRegisters = [RAX, RDI, RSI, RDX, RCX, R8, R9]

specialRegisters :: [Register]
specialRegisters = [RSP, RBP, RIP]

getMemRegisters :: Mem reg -> [reg]
getMemRegisters (Mem _ base Nothing          ) = [base]
getMemRegisters (Mem _ base (Just (_, index))) = [base, index]

getArgsRegisters :: BinOp -> Args reg -> ([reg], [reg])
getArgsRegisters op (IR _ dst) | op == MOV = ([], [dst])
                               | op == CMP = ([dst], [])
                               | otherwise = ([], [dst])
getArgsRegisters _ (IM _ mem) = (getMemRegisters mem, [])
getArgsRegisters op (RR src dst) | op == MOV = ([src], [dst])
                                 | op == CMP = ([src, dst], [])
                                 | otherwise = ([src, dst], [dst])
getArgsRegisters op (MR mem dst)
  | op == MOV = (getMemRegisters mem, [dst])
  | op == CMP = (getMemRegisters mem, [])
  | otherwise = (dst : getMemRegisters mem, [dst])
getArgsRegisters _ (RM src mem) = (src : getMemRegisters mem, [])

getArgRegisters :: UnOp -> Arg reg -> ([reg], [reg])
getArgRegisters op (R reg) | op `elem` [PUSH, ICALL] = ([reg], [])
                           | op == POP               = ([], [reg])
                           | otherwise               = ([reg], [reg])
getArgRegisters _ (M mem) = (getMemRegisters mem, [])

-- returns (src, dst)
getRegisters :: RegisterLike reg => Instruction reg -> ([reg], [reg])
getRegisters (OP    op   args     ) = getArgsRegisters op args
getRegisters (UN    op   arg      ) = getArgRegisters op arg
getRegisters (JUMP  CALL _        ) = ([], [fromRegister rax])
getRegisters (JUMP  _    _        ) = ([], [])
getRegisters (MOV64 _    dst      ) = ([], [dst])
getRegisters (SHIFT Nothing  _ dst) = ([dst, fromRegister rcx], [dst])
getRegisters (SHIFT (Just _) _ dst) = ([dst], [dst])
getRegisters (LEA mem dst         ) = (getMemRegisters mem, [dst])
getRegisters (IDIV src) =
  ( [src, fromRegister rax, fromRegister rdx]
  , [fromRegister rax, fromRegister rdx]
  )
getRegisters CQTO        = ([fromRegister rax], [fromRegister rdx])
getRegisters (PUSHI _)   = ([], [])
getRegisters RET         = ([fromRegister rax], [])
getRegisters (SYSCALL n) = if n + 1 >= length syscallRegisters
  then error "too many arguments for system call"
  else
    ( map fromRegister $ take (n + 1) syscallRegisters
    , map fromRegister syscallRegisters
    )
getRegisters (LABEL  _) = ([], [])
getRegisters (SYMBOL _) = ([], [])

data JumpType = Straightline | Jump Label | Branch Label

getJumpType :: Instruction reg -> JumpType
getJumpType (JUMP JMP  label) = Jump label
getJumpType (JUMP CALL _    ) = Straightline
getJumpType (JUMP _    label) = Branch label
getJumpType _                 = Straightline

mapMem :: (reg1 -> reg2) -> Mem reg1 -> Mem reg2
mapMem f (Mem disp reg msi) = Mem disp (f reg) ((f <$>) <$> msi)

mapArgs :: (reg1 -> reg2) -> Args reg1 -> Args reg2
mapArgs f (IR imm reg) = IR imm (f reg)
mapArgs f (IM imm mem) = IM imm (mapMem f mem)
mapArgs f (RR src dst) = RR (f src) (f dst)
mapArgs f (MR mem reg) = MR (mapMem f mem) (f reg)
mapArgs f (RM reg mem) = RM (f reg) (mapMem f mem)

mapArg :: (reg1 -> reg2) -> Arg reg1 -> Arg reg2
mapArg f (R reg) = R (f reg)
mapArg f (M mem) = M (mapMem f mem)

mapInstr :: (reg1 -> reg2) -> Instruction reg1 -> Instruction reg2
mapInstr f (OP    op  args     ) = OP op (mapArgs f args)
mapInstr f (UN    op  arg      ) = UN op (mapArg f arg)
mapInstr _ (JUMP  op  label    ) = JUMP op label
mapInstr f (MOV64 imm reg      ) = MOV64 imm (f reg)
mapInstr f (SHIFT amt shift reg) = SHIFT amt shift (f reg)
mapInstr f (LEA mem reg        ) = LEA (mapMem f mem) (f reg)
mapInstr f (IDIV reg           ) = IDIV (f reg)
mapInstr _ CQTO                  = CQTO
mapInstr _ (PUSHI imm)           = PUSHI imm
mapInstr _ RET                   = RET
mapInstr _ (SYSCALL n   )        = SYSCALL n
mapInstr _ (LABEL   name)        = LABEL name
mapInstr _ (SYMBOL  name)        = SYMBOL name

data Function reg = Function Int Label [Instruction reg]

function :: Label -> [Instruction reg] -> Function reg
function = Function 0

type VirtualFunction = Function VirtualRegister
type PhysicalFunction = Function Register

fnInstrs :: Function reg -> [Instruction reg]
fnInstrs (Function _ name instrs) = SYMBOL name : instrs

instance Show reg => Show (Function reg) where
  show fn = concatMap
    (\instr ->
      (case instr of
          LABEL  lname -> lname ++ ":"
          SYMBOL sname -> ".globl " ++ sname ++ "\n" ++ sname ++ ":"
          _            -> "\t" ++ show instr
        )
        ++ "\n"
    )
    (fnInstrs fn)

type Datum = (Label, B.ByteString)

data Program reg = Program (Function reg) [Function reg] [Datum]

instance Show reg => Show (Program reg) where
  show (Program mainFn fns datums) =
    ".text\n" ++ show mainFn ++ concatMap show fns ++ ".data\n" ++ concat
      (flip map datums $ \(label, datum) -> label ++ ":\n" ++ concat
        ( flip map (B.unpack datum)
        $ \byte -> "\t.byte 0x" ++ showHex byte "" ++ "\n"
        )
      )
