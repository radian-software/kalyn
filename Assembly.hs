module Assembly where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map.Strict               as Map
import           Data.Word

data Register = RAX | RBX | RCX | RDX
              | RDI | RSI | R8  | R9
              | R10 | R11 | R12 | R13
              | R14 | R15 | RSP | RBP
  deriving (Show)

newtype LName = LName String
  deriving (Eq, Ord, Show)

data Instruction = MOV_IR Word32 Register
                 | LEA_LR LName Register
                 | SYSCALL
  deriving (Show)

data Section = Text [Instruction]
             | Data B.ByteString
             | Label LName
  deriving (Show)

newtype Program = Program [Section]
  deriving (Show)

opcodes :: Map.Map LName Word32 -> Word32 -> Instruction -> B.ByteString
opcodes labels index instr = case instr of
  MOV_IR val reg ->
    let
      regcode = case reg of
        RAX -> 0xc0
        RCX -> 0xc1
        RDX -> 0xc2
        RBX -> 0xc3
        RSP -> 0xc4
        RBP -> 0xc5
        RSI -> 0xc6
        RDI -> 0xc7
        _ ->
          error $ "don't know how to move immediate to register " ++ show reg
    in  toLazyByteString
          $  word8 0x48
          <> word8 0xc7
          <> word8 regcode
          <> word32LE val
  LEA_LR name reg ->
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
    in  case Map.lookup name labels of
          Nothing -> error $ "no such label " ++ show name
          Just labelIndex ->
            toLazyByteString
              $  word8 0x48
              <> word8 0x8d
              <> word8 regcode
              <> int32LE (fromIntegral labelIndex - fromIntegral index)
  SYSCALL -> toLazyByteString $ word8 0x0f <> word8 0x05

compile :: Program -> B.ByteString
compile program = case program of
  Program sections ->
    let sections' = zip
          sections
          (scanl (+) 0 $ map
            (\section ->
              (case section of
                Text  is -> fromIntegral $ length is
                Data  bs -> fromIntegral $ B.length bs
                Label _  -> 0
              ) :: Word32
            )
            sections
          )
        labels' = foldr
          (\(section, start) labels -> case section of
            Label name -> Map.insert name start labels
            _          -> labels
          )
          Map.empty
          sections'
    in  toLazyByteString $ mconcat $ map
          (\(section, start) -> case section of
            Text is -> mconcat $ zipWith
              (\index instr -> lazyByteString $ opcodes labels' index instr)
              (iterate (+ 1) start)
              is
            Data  bs -> lazyByteString bs
            Label _  -> mempty
          )
          sections'
