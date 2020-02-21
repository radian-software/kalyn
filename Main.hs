module Main where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           System.Posix.Files

import qualified Assembly                      as A
import qualified ELF

helloWorld :: A.Program
helloWorld = A.Program
  [ A.Label $ A.LName "main"
  , A.Text
    [ A.MOV_IR 1 A.RAX
    , A.MOV_IR 1 A.RDI
    , A.LEA_LR (A.LName "main") A.RSI
    , A.MOV_IR 14 A.RDX
    , A.SYSCALL
    , A.MOV_IR 60 A.RAX
    , A.MOV_IR 0 A.RDI
    , A.SYSCALL
    ]
  , A.Data (toLazyByteString $ stringUtf8 "Hello, world!\n")
  ]

helloWorld' :: B.ByteString
helloWorld' = B.pack
  [ 0x48 -- mov $0x1, %rax
  , 0xc7
  , 0xc0
  , 0x01
  , 0x00
  , 0x00
  , 0x00
  , 0x48 -- mov $1, %rdi
  , 0xc7
  , 0xc7
  , 0x01
  , 0x00
  , 0x00
  , 0x00
  , 0x48 -- lea 21(%rip), %rsi
  , 0x8d
  , 0x35
  , 0x15
  , 0x00
  , 0x00
  , 0x00
  , 0x48 -- mov $14, %rdx
  , 0xc7
  , 0xc2
  , 0x0e
  , 0x00
  , 0x00
  , 0x00
  , 0x0f -- syscall
  , 0x05
  , 0x48 -- mov $0x3c, %rax
  , 0xc7
  , 0xc0
  , 0x3c
  , 0x00
  , 0x00
  , 0x00
  , 0x48 -- xor %rdi, %rdi
  , 0x31
  , 0xff
  , 0x0f -- syscall
  , 0x05
  , 0x48 -- "Hello, world!\n"
  , 0x65
  , 0x6c
  , 0x6c
  , 0x6f
  , 0x2c
  , 0x20
  , 0x77
  , 0x6f
  , 0x72
  , 0x6c
  , 0x64
  , 0x21
  , 0x0a
  ]

main :: IO ()
main = do
  print $ toLazyByteString $ lazyByteStringHex $ A.compile helloWorld
  B.writeFile "main" (ELF.elfData $ A.compile helloWorld)
  setFileMode "main" 0o777
