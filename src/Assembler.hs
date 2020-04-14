module Assembler
  ( compile
  )
where

import           Control.Applicative
import           Data.Bits               hiding ( shift )
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           Data.Int
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Word
import           Prelude                 hiding ( lines )

import           Assembly
import           OS
import           Util

data Line = Instruction (Instruction Register)
          | LLabel Label

getLines :: [Function Register] -> [Line]
getLines fns = concat
  (flip map fns $ \fn -> concat
    (flip map fn $ \(instr, maybeLabel) -> case maybeLabel of
      Nothing    -> [Instruction instr]
      Just label -> [LLabel label, Instruction instr]
    )
  )

-- https://www.codeproject.com/Articles/662301/x86-Instruction-Encoding-Revealed-Bit-Twiddling-fo
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
regCode RIP = error $ "can't use " ++ show RIP ++ " here"

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

rexMaybe :: Maybe Register -> Maybe Register -> Maybe Register -> Builder
rexMaybe reg rm index = case rex reg rm index of
  0x48 -> mempty
  byte -> word8 byte

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
        (case base of
          RIP -> Nothing
          _   -> Just base
        )
        (case (base, msi) of
          (RIP, Just _ ) -> error $ "can't use scale with " ++ show RIP
          (RIP, Nothing) -> Nothing
          _              -> (snd <$> msi) <|> Just RSP
        )
      modRMBits = modRM
        (case base of
          RIP -> ModPC
          _   -> ModMem
        )
        (case other of
          Right r        -> Reg r
          Left  (ext, _) -> RegExt ext
        )
        (case base of
          RIP -> RMPC
          _   -> RMSIB
        )
      maybeSIB =
          (case base of
            RIP -> mempty
            _   -> word8 $ sib base msi
          )
  in  toLazyByteString
        $  word8 rexBits
        <> mconcat (map word8 opcode)
        <> word8 modRMBits
        <> maybeSIB
        <> int32LE disp
        <> (case other of
             Left (_, imm) -> int32LE imm
             _             -> mempty
           )

opInstr'
  :: (imm -> Builder)
  -> [Word8]
  -> Register
  -> Either Word8 Register
  -> Maybe imm
  -> B.ByteString
opInstr' getBuilder opcode main other mimm =
  let rexBits = rex
        (case other of
          Right r -> Just r
          _       -> Nothing
        )
        (Just main)
        Nothing
      modRMBits = modRM
        ModReg
        (case other of
          Left  ext -> RegExt ext
          Right r   -> Reg r
        )
        (RMReg main)
  in  toLazyByteString
        $  word8 rexBits
        <> mconcat (map word8 opcode)
        <> word8 modRMBits
        <> maybe mempty getBuilder mimm

opInstr
  :: [Word8] -> Register -> Either Word8 Register -> Maybe Int32 -> B.ByteString
opInstr = opInstr' int32LE

opInstr8U
  :: [Word8] -> Register -> Either Word8 Register -> Maybe Word8 -> B.ByteString
opInstr8U = opInstr' word8

opInstr64
  :: [Word8] -> Register -> Either Word8 Register -> Maybe Int64 -> B.ByteString
opInstr64 = opInstr' int64LE

plainInstr :: [Word8] -> B.ByteString
plainInstr opcode = toLazyByteString $ mconcat (map word8 opcode)

plainInstr64 :: [Word8] -> B.ByteString
plainInstr64 opcode =
  toLazyByteString $ word8 (rex Nothing Nothing Nothing) <> mconcat
    (map word8 opcode)

relInstr :: [Word8] -> Int32 -> B.ByteString
relInstr opcode rel =
  toLazyByteString $ mconcat (map word8 opcode) <> int32LE rel

regInstr :: [Word8] -> Register -> B.ByteString
regInstr opcode reg =
  toLazyByteString $ rexMaybe Nothing (Just reg) Nothing <> mconcat
    (map word8 opcode)

compileInstr
  :: Map.Map Label Word32 -> Word32 -> Instruction Register -> B.ByteString
compileInstr labels pc instr =
  let getOffset label = case Map.lookup label labels of
        Nothing          -> error $ "no such label " ++ show label
        Just labelOffset -> fromIntegral labelOffset - fromIntegral pc
      fromDisp (Left  label) = getOffset label
      fromDisp (Right imm  ) = imm
  in  case instr of
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
                  dst
                  (case immExt of
                    Nothing  -> Right dst
                    Just ext -> Left ext
                  )
                  (Just imm)
                IM imm (Mem disp base msr) -> memInstr
                  immOp
                  base
                  msr
                  (fromDisp disp)
                  (Left (fromMaybe errorMemDisallowed immExt, imm))
                RR src dst -> opInstr stdOp src (Right dst) Nothing
                MR (Mem disp base msr) dst ->
                  memInstr stdOp base msr (fromDisp disp) (Right dst)
                RM src (Mem disp base msr) -> case immExt of
                  Just _  -> memInstr memOp base msr (fromDisp disp) (Right src)
                  Nothing -> errorMemDisallowed
        SHIFT Nothing shift dst ->
          let (op, ext) = case shift of
                SHL -> (0xd3, 4)
                SAL -> (0xd3, 6)
                SHR -> (0xd3, 5)
                SAR -> (0xd3, 7)
          in  opInstr [op] dst (Left ext) Nothing
        SHIFT (Just amt) shift dst ->
          let (op, ext) = case shift of
                SHL -> (0xc1, 4)
                SAL -> (0xc1, 6)
                SHR -> (0xc1, 5)
                SAR -> (0xc1, 7)
          in  opInstr8U [op] dst (Left ext) (Just amt)
        LEA (Mem disp base msr) dst ->
          memInstr [0x8d] base msr (fromDisp disp) (Right dst)
        MOV64 imm dst ->
          opInstr64 [0xb8 + (snd . regCode $ dst)] dst (Left 0) (Just imm)
        CQTO          -> plainInstr64 [0x99]
        IDIV    src   -> opInstr [0xf7] src (Left 7) Nothing
        NOT     dst   -> opInstr [0xf7] dst (Left 2) Nothing
        NEG     dst   -> opInstr [0xf7] dst (Left 3) Nothing
        INC     dst   -> opInstr [0xff] dst (Left 0) Nothing
        DEC     dst   -> opInstr [0xff] dst (Left 1) Nothing
        JMP     label -> relInstr [0xe9] (getOffset label)
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
        CALL    label -> relInstr [0xe8] (getOffset label)
        RET           -> plainInstr [0xc3]

compileLine :: Map.Map Label Word32 -> Word32 -> Line -> B.ByteString
compileLine labels pc (Instruction instr) = compileInstr labels pc instr
compileLine _      _  (LLabel      _    ) = B.empty

compile :: Program Register -> (B.ByteString, B.ByteString)
compile (Program fns datums) =
  let lines = getLines fns
      codeB =
          B.concat $ fixedPoint (replicate (length lines) B.empty) $ \binLines ->
            let codeOffsets =
                    scanl (+) 0 $ map (fromIntegral . B.length) binLines
                dataOffsets =
                    scanl (+) (roundUp (fromIntegral pageSize) $ last codeOffsets)
                      $ map (fromIntegral . B.length . snd) datums
                labels =
                    foldr
                        (\(label, offset) ls -> Map.insert label offset ls)
                        ( foldr
                            (\(line, offset) ls -> case line of
                              LLabel name -> Map.insert name offset ls
                              _           -> ls
                            )
                            Map.empty
                        $ zip lines codeOffsets
                        )
                      $ zip (map fst datums) dataOffsets
            in  zipWith (compileLine labels) (tail codeOffsets) lines
  in  ( codeB <> B.pack
        (replicate (leftover pageSize (fromIntegral $ B.length codeB)) 0)
      , B.concat $ map snd datums
      )
