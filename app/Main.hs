module Main where

import           Control.Exception
import qualified Data.ByteString.Lazy          as B
import           System.FilePath
import           System.IO.Error
import           System.Posix.Files

import           AST

-- import in stack order
import           Lexer
import           Reader
import           Parser
import           Bundler
import           Resolver
import           Translator
import           Liveness
import           RegisterAllocator
import           Boilerplate
import           Assembler
import           Linker

{-# ANN module "HLint: ignore Use tuple-section" #-}

ignoringDoesNotExist :: IO () -> IO ()
ignoringDoesNotExist m = do
  res <- try m
  case res of
    Left err | not . isDoesNotExistError $ err -> ioError err
    _ -> return ()

overwriteFile :: FilePath -> String -> IO ()
overwriteFile filename str = do
  ignoringDoesNotExist $ removeLink filename
  writeFile filename str

overwriteBinary :: FilePath -> B.ByteString -> IO ()
overwriteBinary filename bin = do
  ignoringDoesNotExist $ removeLink filename
  B.writeFile filename bin

getPrefix :: String -> String
getPrefix inputFilename = do
  let base = dropExtension . takeFileName $ inputFilename
  let src = takeFileName . takeDirectory $ inputFilename
  let out  = "out"
  let root = takeDirectory . takeDirectory $ inputFilename
  if src /= "src-kalyn-test"
    then error "Kalyn source files outside src-kalyn-test"
    else root </> out </> base

readIncrementally :: String -> IO [Decl]
readIncrementally inputFilename = do
  let prefix = getPrefix inputFilename
  str <- readFile inputFilename
  putStrLn $ "Lexer (" ++ takeFileName inputFilename ++ ")"
  let tokens = tokenize str
  overwriteFile (prefix ++ "Tokens") $ concatMap (\t -> show t ++ "\n") tokens
  putStrLn $ "Reader (" ++ takeFileName inputFilename ++ ")"
  let forms = readModule tokens
  overwriteFile (prefix ++ "Forms.kalyn")
    $ concatMap (\f -> show f ++ "\n") forms
  putStrLn $ "Parser (" ++ takeFileName inputFilename ++ ")"
  let decls = parseModule forms
  overwriteFile (prefix ++ "AST.kalyn") $ concatMap (\d -> show d ++ "\n") decls
  return decls

compileIncrementally :: String -> IO ()
compileIncrementally inputFilename = do
  let prefix = getPrefix inputFilename
  putStrLn "Bundler"
  bundle <- readBundle readIncrementally inputFilename
  overwriteFile (prefix ++ "Bundle.kalyn") $ show bundle
  putStrLn "Resolver"
  let resolver = resolveBundle bundle
  overwriteFile (prefix ++ "Resolver") $ show resolver
  putStrLn "Translator"
  let virtualProgram = translateBundle resolver bundle
  overwriteFile (prefix ++ "Virtual.S") $ show virtualProgram
  putStrLn "Liveness"
  overwriteFile (prefix ++ "Liveness") $ showLiveness virtualProgram
  putStrLn "RegisterAllocator"
  let physicalProgram = allocateProgramRegs virtualProgram
  overwriteFile (prefix ++ "Raw.S") $ show physicalProgram
  putStrLn "Boilerplate"
  let physicalProgram' = addProgramBoilerplate physicalProgram
  overwriteFile (prefix ++ ".S") $ show physicalProgram'
  putStrLn "Assembler"
  let (codeB, dataB) = assemble physicalProgram'
  overwriteBinary (prefix ++ ".o") (codeB <> dataB)
  putStrLn "Linker"
  let binary = link (codeB, dataB)
  overwriteBinary prefix binary
  setFileMode prefix 0o755

main :: IO ()
main = compileIncrementally "src-kalyn-test/Main.kalyn"
