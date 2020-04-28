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
  (Function
    [ OP MOV (IR 1 RAX)
    , OP MOV (IR 1 RDI)
    , LEA (memLabel "message") RSI
    , OP MOV (IR 14 RDX)
    , SYSCALL 3 -- write
    , OP MOV (IR 60 RAX)
    , OP MOV (IR 0 RDI)
    , SYSCALL 1
    ]
  )
  []
  [("message", toLazyByteString $ stringUtf8 "Hello, world!\n")]

printInt :: Program Register
printInt = Program
  (Function
    [ OP MOV  (IR 42 RDI)
    , OP IMUL (IR 42 RDI)
    , CALL "printInt"
    , OP MOV (IR 1 RAX)
    , OP MOV (IR 1 RDI)
    , LEA (memLabel "newline") RSI
    , OP MOV (IR 1 RDX)
    , SYSCALL 3 -- write
    , OP MOV (IR 60 RAX)
    , OP MOV (IR 0 RDI)
    , SYSCALL 1
    ]
  )
  [ function
    "printInt"
    [ OP CMP (IR 0 RDI)
    , JGE "printInt1"
    , LEA (memLabel "minus") RSI
    , PUSH RDI
    , OP MOV (IR 1 RAX)
    , OP MOV (IR 1 RDX)
    , OP MOV (IR 1 RDI)
    , SYSCALL 3 -- write
    , POP RDI
    , OP IMUL (IR (-1) RDI)
    , LABEL "printInt1"
    , OP CMP (IR 0 RDI)
    , JNE "printInt2"
    , OP MOV (IR 1 RAX)
    , OP MOV (IR 1 RDI)
    , LEA (memLabel "digits") RSI
    , OP MOV (IR 1 RDX)
    , SYSCALL 3 -- write
    , RET
    , LABEL "printInt2"
    , CALL "printIntRec"
    , RET
    ]
  , function
    "printIntRec"
    [ OP CMP (IR 0 RDI)
    , JNE "printIntRec1"
    , RET
    , LABEL "printIntRec1"
    , OP MOV (RR RDI RAX)
    , CQTO
    , OP MOV (IR 10 RSI)
    , IDIV RSI
    , PUSH RDX
    , OP MOV (RR RAX RDI)
    , CALL "printIntRec"
    , LEA (memLabel "digits") RSI
    , OP MOV (IR 1 RAX)
    , POP RDX
    , OP ADD (RR RDX RSI)
    , OP MOV (IR 1 RDI)
    , OP MOV (IR 1 RDX)
    , SYSCALL 3 -- write
    , RET
    ]
  ]
  [ ("digits" , toLazyByteString (stringUtf8 "0123456789"))
  , ("minus"  , toLazyByteString (charUtf8 '-'))
  , ("newline", toLazyByteString (charUtf8 '\n'))
  ]

test :: Program Register
test = Program
  (Function
    [ SHIFT Nothing  SHL R12
    , SHIFT Nothing  SAL R12
    , SHIFT Nothing  SHR R12
    , SHIFT Nothing  SAR R12
    , SHIFT (Just 5) SHL R12
    , SHIFT (Just 5) SAL R12
    , SHIFT (Just 5) SHR R12
    , SHIFT (Just 5) SAR R12
    ]
  )
  []
  []

ignoringDoesNotExist :: IO () -> IO ()
ignoringDoesNotExist m = do
  res <- try m
  case res of
    Left err | not . isDoesNotExistError $ err -> ioError err
    _ -> return ()

compileIncrementally :: Program Register -> String -> IO ()
compileIncrementally prog fname = do
  putStrLn $ "  compiling " ++ show fname ++ " ..."
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
  putStrLn $ "  parsing " ++ show fname ++ " ..."
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
  putStrLn "Running Kalyn ..."
  compileIncrementally helloWorld "hello"
  compileIncrementally printInt   "print"
  compileIncrementally test       "test"
  parseIncrementally "Linker"
  parseIncrementally "Main"
  parseIncrementally "Stdlib"
  parseIncrementally "Util"
  ignoringDoesNotExist $ removeLink "out-kalyn/MainFullAST.kalyn"
  putStrLn "  bundling ..."
  bundle <- readBundle (parseModule . readModule . tokenize)
                       "src-kalyn/Main.kalyn"
  writeFile "out-kalyn/MainFullAST.kalyn" $ show bundle
