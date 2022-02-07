module Assembler
  ( assemble
  ) where

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
  -> Either Word8 Register
  -> Maybe Int32
  -> B.ByteString
memInstr opcode base msi disp other imm =
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
          Right r   -> Reg r
          Left  ext -> RegExt ext
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
        <> (case imm of
             Just imm' -> int32LE imm'
             _         -> mempty
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

compressedInstr64 :: Word8 -> Register -> Maybe Int64 -> B.ByteString
compressedInstr64 opcode reg mimm =
  toLazyByteString
    $  word8 (rex Nothing (Just reg) Nothing)
    <> word8 (opcode + (snd . regCode $ reg))
    <> maybe mempty int64LE mimm

plainInstr :: [Word8] -> B.ByteString
plainInstr opcode = toLazyByteString $ mconcat (map word8 opcode)

plainInstr64 :: [Word8] -> B.ByteString
plainInstr64 opcode =
  toLazyByteString $ word8 (rex Nothing Nothing Nothing) <> mconcat
    (map word8 opcode)

immInstr :: [Word8] -> Int32 -> B.ByteString
immInstr opcode rel =
  toLazyByteString $ mconcat (map word8 opcode) <> int32LE rel

compileInstr
  :: Map.Map Label Word32 -> Word32 -> PhysicalInstruction -> B.ByteString
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
                IM imm (Mem disp base msi) -> memInstr
                  immOp
                  base
                  msi
                  (fromDisp disp)
                  (Left $ fromMaybe errorMemDisallowed immExt)
                  (Just imm)
                RR src dst -> opInstr stdOp src (Right dst) Nothing
                MR (Mem disp base msi) dst ->
                  memInstr stdOp base msi (fromDisp disp) (Right dst) Nothing
                RM src (Mem disp base msi) -> case immExt of
                  Just _ ->
                    memInstr memOp base msi (fromDisp disp) (Right src) Nothing
                  Nothing -> errorMemDisallowed
        UN op arg ->
          let (opcode, ext) = case op of
                NOT   -> ([0xf7], 2)
                NEG   -> ([0xf7], 3)
                INC   -> ([0xff], 0)
                DEC   -> ([0xff], 1)
                PUSH  -> ([0xff], 6)
                POP   -> ([0x8f], 0)
                ICALL -> ([0xff], 2)
          in  case arg of
                R reg -> opInstr opcode reg (Left ext) Nothing
                M (Mem disp base msi) ->
                  memInstr opcode base msi (fromDisp disp) (Left ext) Nothing
        JUMP op label ->
          let opcode = case op of
                JMP  -> [0xe9]
                JE   -> [0x0f, 0x84]
                JNE  -> [0x0f, 0x85]
                JL   -> [0x0f, 0x8c]
                JLE  -> [0x0f, 0x8e]
                JG   -> [0x0f, 0x8f]
                JGE  -> [0x0f, 0x8d]
                JB   -> [0x0f, 0x82]
                JBE  -> [0x0f, 0x86]
                JA   -> [0x0f, 0x87]
                JAE  -> [0x0f, 0x83]
                CALL -> [0xe8]
          in  immInstr opcode (getOffset label)
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
        MOVBRM src (Mem disp base msi) ->
          memInstr [0x88] base msi (fromDisp disp) (Right src) Nothing
        MOVBMR (Mem disp base msi) dst ->
          memInstr [0x8a] base msi (fromDisp disp) (Right dst) Nothing
        MOV64 imm dst -> compressedInstr64 0xb8 dst (Just imm)
        LEA (Mem disp base msi) dst ->
          memInstr [0x8d] base msi (fromDisp disp) (Right dst) Nothing
        IDIV src  -> opInstr [0xf7] src (Left 7) Nothing
        CQTO      -> plainInstr64 [0x99]
        PUSHI imm -> immInstr [0x68] imm
        RET       -> plainInstr [0xc3]
        SYSCALL _ -> plainInstr [0x0f, 0x05]
        LABEL   _ -> B.empty
        SYMBOL  _ -> B.empty

-- assumes the data section will be placed right after the code
-- section (with padding to make them both start on page boundaries)
assemble
  :: Program Register
  -> (B.ByteString, B.ByteString, Map.Map String Int, Map.Map String Int)
assemble (Program main fns datums) =
  let
    allInstrs = fnInstrs main ++ concatMap fnInstrs fns
    (binInstrs', codeSymbols', dataSymbols') =
      fixedPoint (replicate (length allInstrs) B.empty, Map.empty, Map.empty)
        $ \(binInstrs, _, _) ->
            let
              codeOffsets =
                scanl (+) 0 $ map (fromIntegral . B.length) binInstrs
              dataOffsets =
                scanl (+) (roundUp (fromIntegral pageSize) $ last codeOffsets)
                  $ map (fromIntegral . B.length . snd) datums
              (labels, codeSymbols, dataSymbols) =
                foldr
                    (\(name, offset) (ls, cs, ds) ->
                      (Map.insert name offset ls, cs, Map.insert name offset ds)
                    )
                    ( foldr
                        (\(instr, offset) (ls, cs, ds) -> case instr of
                          LABEL name -> (Map.insert name offset ls, cs, ds)
                          SYMBOL name ->
                            ( Map.insert name offset ls
                            , Map.insert name offset cs
                            , ds
                            )
                          _ -> (ls, cs, ds)
                        )
                        (Map.empty, Map.empty, Map.empty)
                    $ zip allInstrs codeOffsets
                    )
                  $ zip (map fst datums) dataOffsets
            in
              ( zipWith (compileInstr labels) (tail codeOffsets) allInstrs
              , codeSymbols
              , dataSymbols
              )
  in
    ( B.concat binInstrs'
    , B.concat $ map snd datums
    , Map.map fromIntegral codeSymbols'
    , Map.map fromIntegral dataSymbols'
    )
