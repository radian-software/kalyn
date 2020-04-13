module Main where

import           Control.Exception
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           System.IO.Error
import           System.Posix.Files

import           Assembly

-- import in stack order
import           Lexer
import           Reader
import           Parser
import           Bundler
import           Assembler
import           Linker

{-# ANN module "HLint: ignore Use tuple-section" #-}

helloWorld :: Program Register
helloWorld = Program
  [ map
      (\instr -> (instr, Nothing))
      [ OP MOV (IR 1 RAX)
      , OP MOV (IR 1 RDI)
      , LEAL (Label "message") RSI
      , OP MOV (IR 14 RDX)
      , SYSCALL 3
      , OP MOV (IR 60 RAX)
      , OP MOV (IR 0 RDI)
      , SYSCALL 1
      ]
  ]
  [(Label "message", toLazyByteString $ stringUtf8 "Hello, world!\n")]

printInt :: Program Register
printInt = Program
  [ [ (OP MOV (IR 42 RDI)        , Nothing)
    , (OP IMUL (IR 42 RDI)       , Nothing)
    , (CALL 1 (Label "printInt") , Nothing)
    , (OP MOV (IR 1 RAX)         , Nothing)
    , (OP MOV (IR 1 RDI)         , Nothing)
    , (LEAL (Label "newline") RSI, Nothing)
    , (OP MOV (IR 1 RDX)         , Nothing)
    , (SYSCALL 3                 , Nothing)
    , (OP MOV (IR 60 RAX)        , Nothing)
    , (OP MOV (IR 0 RDI)         , Nothing)
    , (SYSCALL 1                 , Nothing)
    ]
  , [ (OP CMP (IR 0 RDI)           , Just $ Label "printInt")
    , (JGE (Label "printInt1")     , Nothing)
    , (LEAL (Label "minus") RSI    , Nothing)
    , (PUSH RDI                    , Nothing)
    , (OP MOV (IR 1 RAX)           , Nothing)
    , (OP MOV (IR 1 RDX)           , Nothing)
    , (OP MOV (IR 1 RDI)           , Nothing)
    , (SYSCALL 3                   , Nothing)
    , (POP RDI                     , Nothing)
    , (OP IMUL (IR (-1) RDI)       , Nothing)
    , (OP CMP (IR 0 RDI)           , Just $ Label "printInt1")
    , (JNE (Label "printInt2")     , Nothing)
    , (OP MOV (IR 1 RAX)           , Nothing)
    , (OP MOV (IR 1 RDI)           , Nothing)
    , (LEAL (Label "digits") RSI   , Nothing)
    , (OP MOV (IR 1 RDX)           , Nothing)
    , (SYSCALL 3                   , Nothing)
    , (RET                         , Nothing)
    , (CALL 1 (Label "printIntRec"), Just $ Label "printInt2")
    , (RET                         , Nothing)
    ]
  , [ (OP CMP (IR 0 RDI)           , Just $ Label "printIntRec")
    , (JNE (Label "printIntRec1")  , Nothing)
    , (RET                         , Nothing)
    , (OP MOV (RR RDI RAX)         , Just $ Label "printIntRec1")
    , (CQTO                        , Nothing)
    , (OP MOV (IR 10 RSI)          , Nothing)
    , (IDIV RSI                    , Nothing)
    , (PUSH RDX                    , Nothing)
    , (OP MOV (RR RAX RDI)         , Nothing)
    , (CALL 1 (Label "printIntRec"), Nothing)
    , (LEAL (Label "digits") RSI   , Nothing)
    , (OP MOV (IR 1 RAX)           , Nothing)
    , (POP RDX                     , Nothing)
    , (OP ADD (RR RDX RSI)         , Nothing)
    , (OP MOV (IR 1 RDI)           , Nothing)
    , (OP MOV (IR 1 RDX)           , Nothing)
    , (SYSCALL 3                   , Nothing)
    , (RET                         , Nothing)
    ]
  ]
  [ (Label "digits" , toLazyByteString (stringUtf8 "0123456789"))
  , (Label "minus"  , toLazyByteString (charUtf8 '-'))
  , (Label "newline", toLazyByteString (charUtf8 '\n'))
  ]

test :: Program Register
test = Program
  [ function
      [ Right
          [ SHIFT Nothing  SHL R12
          , SHIFT Nothing  SAL R12
          , SHIFT Nothing  SHR R12
          , SHIFT Nothing  SAR R12
          , SHIFT (Just 5) SHL R12
          , SHIFT (Just 5) SAL R12
          , SHIFT (Just 5) SHR R12
          , SHIFT (Just 5) SAR R12
          ]
      ]
  ]
  []

ignoringDoesNotExist :: IO () -> IO ()
ignoringDoesNotExist m = do
  res <- try m
  case res of
    Left err | not . isDoesNotExistError $ err -> ioError err
    _ -> return ()

compileIncrementally :: Program Register -> String -> IO ()
compileIncrementally prog fname = do
  let path = "out/" ++ fname
  mapM_ (ignoringDoesNotExist . removeLink) [path, path ++ ".S", path ++ ".o"]
  writeFile (path ++ ".S") $ show prog
  let obj@(codeB, dataB) = compile prog
  B.writeFile (path ++ ".o") (codeB <> dataB)
  let exec = link obj
  B.writeFile path exec
  setFileMode path 0o755

parseIncrementally :: String -> IO ()
parseIncrementally fname = do
  let inpath  = "src-kalyn/" ++ fname ++ ".kalyn"
  let outpath = "out-kalyn/" ++ fname
  str <- readFile inpath
  mapM_
    (ignoringDoesNotExist . removeLink)
    [outpath ++ "Tokens", outpath ++ "Forms.kalyn", outpath ++ "AST.kalyn"]
  let tokens = tokenize str
  writeFile (outpath ++ "Tokens") $ concatMap (\t -> show t ++ "\n") tokens
  let forms = readModule tokens
  writeFile (outpath ++ "Forms.kalyn") $ concatMap (\f -> show f ++ "\n") forms
  let decls = parseModule forms
  writeFile (outpath ++ "AST.kalyn") $ concatMap (\d -> show d ++ "\n") decls

main :: IO ()
main = do
  compileIncrementally helloWorld "hello"
  compileIncrementally printInt   "print"
  compileIncrementally test       "test"
  parseIncrementally "Linker"
  parseIncrementally "Main"
  parseIncrementally "Stdlib"
  parseIncrementally "Util"
  ignoringDoesNotExist $ removeLink "out-kalyn/MainFullAST.kalyn"
  bundle <- readBundle "src-kalyn/Main.kalyn"
  writeFile "out-kalyn/MainFullAST.kalyn" $ show bundle
