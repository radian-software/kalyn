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
  [ function
      [ Right
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
  ]
  [(Label "message", toLazyByteString $ stringUtf8 "Hello, world!\n")]

printInt :: Program Register
printInt = Program
  [ function
    [ Right
        [ (OP MOV (IR 42 RDI))
        , OP IMUL (IR 42 RDI)
        , CALL 1 (Label "printInt")
        , OP MOV (IR 1 RAX)
        , OP MOV (IR 1 RDI)
        , LEAL (Label "newline") RSI
        , OP MOV (IR 1 RDX)
        , SYSCALL 3
        , OP MOV (IR 60 RAX)
        , OP MOV (IR 0 RDI)
        , SYSCALL 1
        ]
    ]
  , function
    [ Left (Label "printInt")
    , Right
      [ OP CMP (IR 0 RDI)
      , JGE (Label "printInt1")
      , LEAL (Label "minus") RSI
      , PUSH RDI
      , OP MOV (IR 1 RAX)
      , OP MOV (IR 1 RDX)
      , OP MOV (IR 1 RDI)
      , SYSCALL 3
      , POP RDI
      , OP IMUL (IR (-1) RDI)
      ]
    , Left (Label "printInt1")
    , Right
      [ OP CMP (IR 0 RDI)
      , JNE (Label "printInt2")
      , OP MOV (IR 1 RAX)
      , OP MOV (IR 1 RDI)
      , LEAL (Label "digits") RSI
      , OP MOV (IR 1 RDX)
      , SYSCALL 3
      , RET
      ]
    , Left (Label "printInt2")
    , Right [CALL 1 (Label "printIntRec"), RET]
    ]
  , function
    [ Left (Label "printIntRec")
    , Right [OP CMP (IR 0 RDI), JNE (Label "printIntRec1"), RET]
    , Left (Label "printIntRec1")
    , Right
      [ OP MOV (RR RDI RAX)
      , CQTO
      , OP MOV (IR 10 RSI)
      , IDIV RSI
      , PUSH RDX
      , OP MOV (RR RAX RDI)
      , CALL 1 (Label "printIntRec")
      , LEAL (Label "digits") RSI
      , OP MOV (IR 1 RAX)
      , POP RDX
      , OP ADD (RR RDX RSI)
      , OP MOV (IR 1 RDI)
      , OP MOV (IR 1 RDX)
      , SYSCALL 3
      , RET
      ]
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
