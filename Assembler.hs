module Assembler
  ( compile
  )
where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map.Strict               as Map
import           Data.Word

import           Assembly
import           Util

data Fragment = Text (Instruction Register)
              | Data B.ByteString
              | Label LName

getFragments :: Program Register -> [Fragment]
getFragments (Program fns datums) =
  concat
      (flip map fns $ \fn -> concat
        (flip map fn $ \(maybeLabel, instr) -> case maybeLabel of
          Nothing   -> [Text instr]
          Just name -> [Label name, Text instr]
        )
      )
    ++ concat (flip map datums $ \(name, bs) -> [Label name, Data bs])

compileInstr
  :: Map.Map LName Word32 -> Word32 -> Instruction Register -> B.ByteString
compileInstr labels pc instr = case instr of
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
          Just labelOffset ->
            toLazyByteString
              $  word8 0x48
              <> word8 0x8d
              <> word8 regcode
              <> int32LE (fromIntegral labelOffset - fromIntegral pc)
  SYSCALL _ -> toLazyByteString $ word8 0x0f <> word8 0x05

compileFrag :: Map.Map LName Word32 -> Word32 -> Fragment -> B.ByteString
compileFrag labels pc (Text  instr) = compileInstr labels pc instr
compileFrag _      _  (Data  bs   ) = bs
compileFrag _      _  (Label _    ) = B.empty

compile :: Program Register -> B.ByteString
compile program =
  let frags = getFragments program
  in  B.concat $ fixedPoint (replicate (length frags) B.empty) $ \binFrags ->
        let offsets = scanl (+) 0 $ map (fromIntegral . B.length) binFrags
            labels =
                foldr
                    (\(frag, offset) ls -> case frag of
                      Label name -> Map.insert name offset ls
                      _          -> ls
                    )
                    Map.empty
                  $ zip frags offsets
        in  zipWith (compileFrag labels) (tail offsets) frags
