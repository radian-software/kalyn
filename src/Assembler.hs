module Assembler
  ( compile
  )
where

import           Control.Applicative
import           Data.Bits
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           Data.Int
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Word

import           Assembly
import           Util

data Fragment = Text (Instruction Register)
              | Data B.ByteString
              | FLabel Label

getFragments :: Program Register -> [Fragment]
getFragments (Program fns datums) =
  concat
      (flip map fns $ \fn -> concat
        (flip map fn $ \(instr, maybeLabel) -> case maybeLabel of
          Nothing    -> [Text instr]
          Just label -> [FLabel label, Text instr]
        )
      )
    ++ concat (flip map datums $ \(name, bs) -> [FLabel name, Data bs])

-- https://wiki.osdev.org/X86-64_Instruction_Encoding
-- http://ref.x86asm.net/index.html

data Mod = ModReg | ModMem | ModPC
data Reg = Reg Register | RegExt Word8
data RM = RMReg Register | RMSIB | RMPC

regCode :: Register -> (Bool, Word8)
regCode RAX = (False, 0x0)
regCode RCX = (False, 0x1)
regCode RDX = (False, 0x2)
regCode RBX = (False, 0x3)
regCode RSP = (False, 0x4)
regCode RBP = (False, 0x5)
regCode RSI = (False, 0x6)
regCode RDI = (False, 0x7)
regCode R8  = (True, 0x0)
regCode R9  = (True, 0x1)
regCode R10 = (True, 0x2)
regCode R11 = (True, 0x3)
regCode R12 = (True, 0x4)
regCode R13 = (True, 0x5)
regCode R14 = (True, 0x6)
regCode R15 = (True, 0x7)

rex :: Maybe Register -> Maybe Register -> Maybe Register -> Word8
rex reg rm index =
  0x48  -- REX.W
    .|. (case fst . regCode <$> reg of
          Just True -> 0x4  -- REX.R
          _         -> 0
        )
    .|. (case fst . regCode <$> rm of
          Just True -> 0x1  -- REX.B
          _         -> 0
        )
    .|. (case fst . regCode <$> index of
          Just True -> 0x2  -- REX.X
          _         -> 0
        )

modRM :: Mod -> Reg -> RM -> Word8
modRM modOpt reg rm =
  let modBits =
          (case modOpt of
            ModReg -> 0x3
            ModMem -> 0x2
            ModPC  -> 0x0
          )
      regBits =
          (case reg of
            Reg    r -> snd . regCode $ r
            RegExt b -> b
          )
      rmBits =
          (case rm of
            RMReg r -> snd . regCode $ r
            RMSIB   -> 0x4
            RMPC    -> 0x5
          )
  in  (modBits `shiftL` 6) .|. (regBits `shiftL` 3) .|. rmBits

sib :: Register -> Maybe (Scale, Register) -> Word8
sib base msi =
  let scaleBits =
          (case fst <$> msi of
            Just Scale1 -> 0x0
            Just Scale2 -> 0x1
            Just Scale4 -> 0x2
            Just Scale8 -> 0x3
            Nothing     -> 0x0
          )
      indexBits =
          (case snd <$> msi of
            Just r  -> snd . regCode $ r
            Nothing -> snd . regCode $ RSP
          )
      baseBits = snd . regCode $ base
  in  (scaleBits `shiftL` 6) .|. (indexBits `shiftL` 3) .|. baseBits

memInstr
  :: [Word8]
  -> Register
  -> Maybe (Scale, Register)
  -> Int32
  -> Either (Word8, Int32) Register
  -> B.ByteString
memInstr opcode base msi disp other =
  let rexBits = rex
        (case other of
          Right r -> Just r
          _       -> Nothing
        )
        (Just base)
        ((snd <$> msi) <|> Just RSP)
      modRMBits = modRM
        ModMem
        (case other of
          Right r        -> Reg r
          Left  (ext, _) -> RegExt ext
        )
        RMSIB
      sibBits = sib base msi
  in  toLazyByteString
        $  word8 rexBits
        <> mconcat (map word8 opcode)
        <> word8 modRMBits
        <> word8 sibBits
        <> int32LE disp
        <> (case other of
             Left (_, imm) -> int32LE imm
             _             -> mempty
           )

pcInstr :: [Word8] -> Int32 -> Register -> B.ByteString
pcInstr opcode disp other =
  let rexBits   = rex (Just other) Nothing Nothing
      modRMBits = modRM ModPC (Reg other) RMPC
  in  toLazyByteString
        $  word8 rexBits
        <> mconcat (map word8 opcode)
        <> word8 modRMBits
        <> int32LE disp

opInstr'
  :: (imm -> Builder)
  -> [Word8]
  -> Either Word8 Register
  -> Register
  -> Maybe imm
  -> B.ByteString
opInstr' getBuilder opcode src dst mimm =
  let rexBits = rex
        (case src of
          Right r -> Just r
          _       -> Nothing
        )
        (Just dst)
        Nothing
      modRMBits = modRM
        ModReg
        (case src of
          Left  ext -> RegExt ext
          Right r   -> Reg r
        )
        (RMReg dst)
  in  toLazyByteString
        $  word8 rexBits
        <> mconcat (map word8 opcode)
        <> word8 modRMBits
        <> (case mimm of
             Just imm -> getBuilder imm
             Nothing  -> mempty
           )

opInstr
  :: [Word8] -> Either Word8 Register -> Register -> Maybe Int32 -> B.ByteString
opInstr = opInstr' int32LE

opInstr64
  :: [Word8] -> Either Word8 Register -> Register -> Maybe Int64 -> B.ByteString
opInstr64 = opInstr' int64LE

plainInstr :: [Word8] -> B.ByteString
plainInstr opcode = toLazyByteString $ mconcat (map word8 opcode)

relInstr :: [Word8] -> Int32 -> B.ByteString
relInstr opcode rel =
  toLazyByteString $ mconcat (map word8 opcode) <> int32LE rel

regInstr :: [Word8] -> Register -> B.ByteString
regInstr opcode reg =
  toLazyByteString $ word8 (rex Nothing (Just reg) Nothing) <> mconcat
    (map word8 opcode)

compileInstr
  :: Map.Map Label Word32 -> Word32 -> Instruction Register -> B.ByteString
compileInstr labels pc instr =
  let getOffset label = case Map.lookup label labels of
        Nothing          -> error $ "no such label " ++ show label
        Just labelOffset -> fromIntegral labelOffset - fromIntegral pc
  in
    case instr of
      OP op args ->
        let errorMemDisallowed =
                error $ "cannot " ++ show op ++ " into memory address"
            (immOp, immExt, stdOp, memOp) = case op of
              MOV  -> ([0xc7], Just 0, [0x8b], [0x89])
              ADD  -> ([0x81], Just 0, [0x03], [0x01])
              SUB  -> ([0x81], Just 5, [0x2b], [0x29])
              IMUL -> ([0x69], Nothing, [0x0f, 0xaf], undefined)
              AND  -> ([0x81], Just 4, [0x23], [0x21])
              OR   -> ([0x81], Just 1, [0x0b], [0x09])
              XOR  -> ([0x81], Just 6, [0x33], [0x31])
              CMP  -> ([0x81], Just 7, [0x3b], [0x39])
        in  case args of
              IR imm dst -> opInstr
                immOp
                (case immExt of
                  Nothing  -> Right dst
                  Just ext -> Left ext
                )
                dst
                (Just imm)
              IM imm (Mem disp base msr) -> memInstr
                immOp
                base
                msr
                disp
                (Left (fromMaybe errorMemDisallowed immExt, imm))
              RR src dst -> opInstr stdOp (Right src) dst Nothing
              MR (Mem disp base msr) dst ->
                memInstr stdOp base msr disp (Right dst)
              RM src (Mem disp base msr) -> case immExt of
                Just _  -> memInstr memOp base msr disp (Right src)
                Nothing -> errorMemDisallowed
              LR label dst -> pcInstr stdOp (getOffset label) dst
      LEA  (Mem disp base msr) dst -> memInstr [0x8d] base msr disp (Right dst)
      LEAL label               dst -> pcInstr [0x8d] (getOffset label) dst
      MOV64 imm dst ->
        opInstr64 [0xb8 + (snd . regCode $ dst)] (Left 0) dst (Just imm)
      CQTO          -> plainInstr [0x99]
      IDIV    src   -> opInstr [0xf7] (Left 7) src Nothing
      JE      label -> relInstr [0x0f, 0x84] (getOffset label)
      JNE     label -> relInstr [0x0f, 0x85] (getOffset label)
      JL      label -> relInstr [0x0f, 0x8c] (getOffset label)
      JLE     label -> relInstr [0x0f, 0x8e] (getOffset label)
      JG      label -> relInstr [0x0f, 0x8f] (getOffset label)
      JGE     label -> relInstr [0x0f, 0x8d] (getOffset label)
      JB      label -> relInstr [0x0f, 0x82] (getOffset label)
      JBE     label -> relInstr [0x0f, 0x86] (getOffset label)
      JA      label -> relInstr [0x0f, 0x87] (getOffset label)
      JAE     label -> relInstr [0x0f, 0x83] (getOffset label)
      PUSH    reg   -> regInstr [0x50 + (snd . regCode $ reg)] reg
      POP     reg   -> regInstr [0x58 + (snd . regCode $ reg)] reg
      SYSCALL _     -> plainInstr [0x0f, 0x05]
      CALL _ label  -> relInstr [0xff, 0x15] (getOffset label)
      RET           -> plainInstr [0xc3]

compileFrag :: Map.Map Label Word32 -> Word32 -> Fragment -> B.ByteString
compileFrag labels pc (Text   instr) = compileInstr labels pc instr
compileFrag _      _  (Data   bs   ) = bs
compileFrag _      _  (FLabel _    ) = B.empty

compile :: Program Register -> B.ByteString
compile program =
  let frags = getFragments program
  in  B.concat $ fixedPoint (replicate (length frags) B.empty) $ \binFrags ->
        let offsets = scanl (+) 0 $ map (fromIntegral . B.length) binFrags
            labels =
                foldr
                    (\(frag, offset) ls -> case frag of
                      FLabel name -> Map.insert name offset ls
                      _           -> ls
                    )
                    Map.empty
                  $ zip frags offsets
        in  zipWith (compileFrag labels) (tail offsets) frags
