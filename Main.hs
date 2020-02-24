module Main where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           System.Posix.Files

import           Assembler                      ( compile )
import qualified Assembly                      as A
import           Linker                         ( link )

{-# ANN module "HLint: ignore Use tuple-section" #-}

helloWorld :: A.Program A.Register
helloWorld = A.Program
  [ map
      (\instr -> (Nothing, instr))
      [ A.MOV_IR 1 A.RAX
      , A.MOV_IR 1 A.RDI
      , A.LEA_LR (A.LName "message") A.RSI
      , A.MOV_IR 14 A.RDX
      , A.SYSCALL 3
      , A.MOV_IR 60 A.RAX
      , A.MOV_IR 0 A.RDI
      , A.SYSCALL 1
      ]
  ]
  [(A.LName "message", toLazyByteString $ stringUtf8 "Hello, world!\n")]

main :: IO ()
main = do
  B.writeFile "main" (link $ compile helloWorld)
  setFileMode "main" 0o777
