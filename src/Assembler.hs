module Assembler
  ( compile
  )
where

import           Data.Bits
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
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

isRegExt :: Register -> Bool
isRegExt = (`elem` [R8, R9, R10, R11, R12, R13, R14, R15])

rexByte :: Maybe Register -> Maybe Register -> Word8
rexByte src dst =
  0x48
    .|. (if isRegExt (fromMaybe RAX src) then 0x04 else 0x00)
    .|. (if isRegExt (fromMaybe RAX dst) then 0x01 else 0x00)

regBits :: Register -> Word8
regBits reg
  | reg `elem` [RAX, R8]  = 0x00
  | reg `elem` [RCX, R9]  = 0x01
  | reg `elem` [RDX, R10] = 0x02
  | reg `elem` [RBX, R11] = 0x03
  | reg `elem` [RSP, R12] = 0x04
  | reg `elem` [RBP, R13] = 0x05
  | reg `elem` [RSI, R14] = 0x06
  | reg `elem` [RDI, R15] = 0x07
  | otherwise = error ("regBits got to unreachable code: " ++ show reg)

modBits :: Register -> Register -> Word8
modBits src dst = (regBits src `shiftL` 3) .|. regBits dst

moveByteRR :: Register -> Register -> Word8
moveByteRR src dst = 0xc0 .|. modBits src dst

moveByteIR :: Register -> Word8
moveByteIR = moveByteRR RAX

opextByte :: Word8 -> Register -> Word8
opextByte ext dst = 0xc0 .|. (ext `shiftL` 3) .|. regBits dst

moveByteIR64 :: Register -> Word8
moveByteIR64 dst = 0x80 .|. modBits RDI dst

compileInstr
  :: Map.Map Label Word32 -> Word32 -> Instruction Register -> B.ByteString
compileInstr labels pc instr = case instr of
  MOV_IR val dst ->
    toLazyByteString
      $  word8 (rexByte Nothing (Just dst))
      <> word8 0xc7
      <> word8 (moveByteIR dst)
      <> int32LE val
  MOV_IR64 val dst ->
    toLazyByteString
      $  word8 (rexByte Nothing (Just dst))
      <> word8 (moveByteIR64 dst)
      <> int64LE val
  MOV_RR src dst ->
    toLazyByteString
      $  word8 (rexByte (Just src) (Just dst))
      <> word8 0x89
      <> word8 (moveByteRR src dst)
  ADD_IR val dst ->
    toLazyByteString
      $  word8 (rexByte Nothing (Just dst))
      <> word8 0x81
      <> word8 (opextByte 0 dst)
      <> int32LE val
  ADD_RR src dst ->
    toLazyByteString
      $  word8 (rexByte (Just src) (Just dst))
      <> word8 0x01
      <> word8 (moveByteRR src dst)
  SUB_IR val dst ->
    toLazyByteString
      $  word8 (rexByte Nothing (Just dst))
      <> word8 0x81
      <> word8 (opextByte 5 dst)
      <> int32LE val
  SUB_RR src dst ->
    toLazyByteString
      $  word8 (rexByte (Just src) (Just dst))
      <> word8 0x29
      <> word8 (moveByteRR src dst)
  CQTO -> toLazyByteString $ word8 0x48 <> word8 0x99
  AND_RR src dst ->
    toLazyByteString
      $  word8 (rexByte (Just src) (Just dst))
      <> word8 0x21
      <> word8 (moveByteRR src dst)
  OR_RR src dst ->
    toLazyByteString
      $  word8 (rexByte (Just src) (Just dst))
      <> word8 0x09
      <> word8 (moveByteRR src dst)
  XOR_RR src dst ->
    toLazyByteString
      $  word8 (rexByte (Just src) (Just dst))
      <> word8 0x31
      <> word8 (moveByteRR src dst)
  CMP_RR src dst ->
    toLazyByteString
      $  word8 (rexByte (Just src) (Just dst))
      <> word8 0x39
      <> word8 (moveByteRR src dst)
  JE  _ -> toLazyByteString $ word8 0x48 <> word8 0xe3
  JNE _ -> toLazyByteString $ word8 0x48 <> word8 0xe1
  JL  _ -> toLazyByteString $ word8 0x48 <> word8 0xdf
  JLE _ -> toLazyByteString $ word8 0x48 <> word8 0xdd
  JG  _ -> toLazyByteString $ word8 0x48 <> word8 0xdb
  JGE _ -> toLazyByteString $ word8 0x48 <> word8 0xd9
  JB  _ -> toLazyByteString $ word8 0x48 <> word8 0xd7
  JBE _ -> toLazyByteString $ word8 0x48 <> word8 0xd5
  JA  _ -> toLazyByteString $ word8 0x48 <> word8 0xd3
  JAE _ -> toLazyByteString $ word8 0x48 <> word8 0xd1
  LEA_LR label reg ->
    let
      regcode = case reg of
        RAX -> 0x05
        RCX -> 0x0d
        RDX -> 0x15
        RBX -> 0x1d
        RSP -> 0x25
        RBP -> 0x2d
        RSI -> 0x35
        RDI -> 0x3d
        _ ->
          error $ "don't know how to calculate address to register " ++ show reg
    in  case Map.lookup label labels of
          Nothing -> error $ "no such label " ++ show label
          Just labelOffset ->
            toLazyByteString
              $  word8 0x48
              <> word8 0x8d
              <> word8 regcode
              <> int32LE (fromIntegral labelOffset - fromIntegral pc)
  SYSCALL _    -> toLazyByteString $ word8 0x0f <> word8 0x05
  CALL _ label -> case Map.lookup label labels of
    Nothing          -> error $ "no such label " ++ show label
    Just labelOffset -> toLazyByteString $ word8 0xff <> word8 0x15 <> int32LE
      (fromIntegral labelOffset - fromIntegral pc)
  RET -> toLazyByteString $ word8 0xc3

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
