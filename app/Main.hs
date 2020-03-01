module Main where

import           Control.Exception
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           System.IO.Error
import           System.Posix.Files

import           AST
import           Assembly

-- import in stack order
import           Lexer
import           Reader
import           Parser
import           Assembler
import           Linker

{-# ANN module "HLint: ignore Use tuple-section" #-}

helloWorld :: Program Register
helloWorld = Program
  [ map
      (\instr -> (instr, Nothing))
      [ MOV_IR 1 RAX
      , MOV_IR 1 RDI
      , LEA_LR (Label "message") RSI
      , MOV_IR 14 RDX
      , SYSCALL 3
      , MOV_IR 60 RAX
      , MOV_IR 0  RDI
      , SYSCALL 1
      ]
  ]
  [(Label "message", toLazyByteString $ stringUtf8 "Hello, world!\n")]

printInt :: Program Register
printInt = Program
  [ [ (MOV_IR 42 RDI               , Nothing)
    , (IMUL_IR 42 RDI              , Nothing)
    , (CALL 1 (Label "printInt")   , Nothing)
    , (MOV_IR 1 RAX                , Nothing)
    , (MOV_IR 1 RDI                , Nothing)
    , (LEA_LR (Label "newline") RSI, Nothing)
    , (MOV_IR 1 RDX                , Nothing)
    , (SYSCALL 3                   , Nothing)
    , (MOV_IR 60 RAX               , Nothing)
    , (MOV_IR 0 RDI                , Nothing)
    , (SYSCALL 1                   , Nothing)
    ]
  , [ (CMP_IR 0 RDI                , Just $ Label "printInt")
    , (JGE (Label "printInt1")     , Nothing)
    , (LEA_LR (Label "minus") RSI  , Nothing)
    , (PUSH RDI                    , Nothing)
    , (MOV_IR 1 RAX                , Nothing)
    , (MOV_IR 1 RDX                , Nothing)
    , (MOV_IR 1 RDI                , Nothing)
    , (SYSCALL 3                   , Nothing)
    , (POP RDI                     , Nothing)
    , (IMUL_IR (-1) RDI            , Nothing)
    , (CMP_IR 0 RDI                , Just $ Label "printInt1")
    , (JNE (Label "printInt2")     , Nothing)
    , (MOV_IR 1 RAX                , Nothing)
    , (MOV_IR 1 RDI                , Nothing)
    , (LEA_LR (Label "digits") RSI , Nothing)
    , (MOV_IR 1 RDX                , Nothing)
    , (SYSCALL 3                   , Nothing)
    , (RET                         , Nothing)
    , (CALL 1 (Label "printIntRec"), Just $ Label "printInt2")
    , (RET                         , Nothing)
    ]
  , [ (CMP_IR 0 RDI                , Just $ Label "printIntRec")
    , (JNE (Label "printIntRec1")  , Nothing)
    , (RET                         , Nothing)
    , (MOV_RR RDI RAX              , Just $ Label "printIntRec1")
    , (CQTO                        , Nothing)
    , (MOV_IR 10 RSI               , Nothing)
    , (IDIV RSI                    , Nothing)
    , (PUSH RDX                    , Nothing)
    , (MOV_RR RAX RDI              , Nothing)
    , (CALL 1 (Label "printIntRec"), Nothing)
    , (LEA_LR (Label "digits") RSI , Nothing)
    , (MOV_IR 1 RAX                , Nothing)
    , (POP RDX                     , Nothing)
    , (ADD_RR RDX RSI              , Nothing)
    , (MOV_IR 1 RDI                , Nothing)
    , (MOV_IR 1 RDX                , Nothing)
    , (SYSCALL 3                   , Nothing)
    , (RET                         , Nothing)
    ]
  ]
  [ (Label "digits" , toLazyByteString (stringUtf8 "0123456789"))
  , (Label "minus"  , toLazyByteString (charUtf8 '-'))
  , (Label "newline", toLazyByteString (charUtf8 '\n'))
  ]

ignoringDoesNotExist :: IO () -> IO ()
ignoringDoesNotExist m = do
  res <- try m
  case res of
    Left err | not . isDoesNotExistError $ err -> ioError err
    _ -> return ()

testStack :: String -> IO [Decl]
testStack fname = do
  str <- readFile fname
  pure $ parseModule $ readModule $ tokenize str

main :: IO ()
main = do
  ignoringDoesNotExist $ removeLink "main.S"
  ignoringDoesNotExist $ removeLink "main"
  writeFile "main.S" $ ".globl main\nmain:\n" ++ show printInt
  B.writeFile "main" (link $ compile helloWorld)
  setFileMode "main" 0o755
