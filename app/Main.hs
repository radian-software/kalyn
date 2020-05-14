module Main where

import           Control.Exception
import qualified Data.ByteString.Lazy          as B
import           Data.List
import           Data.String.Utils
import qualified Data.Text.Lazy                as T
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           System.Posix.Files
import           Text.Pretty.Simple             ( pShowNoColor )

import           AST
import           OS
import           Util

-- import in stack order
import           Lexer
import           Reader
import           Parser
import           Bundler
import           Resolver
import           TypeChecker
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
  createDirectoryIfMissing True . dropFileName $ filename
  ignoringDoesNotExist $ removeLink filename
  writeFile filename str

overwriteBinary :: FilePath -> B.ByteString -> IO ()
overwriteBinary filename bin = do
  createDirectoryIfMissing True . dropFileName $ filename
  ignoringDoesNotExist $ removeLink filename
  B.writeFile filename bin

getPrefix :: String -> String
getPrefix inputFilename = if "/src-kalyn/" `isInfixOf` inputFilename
  then dropExtension . replace "/src-kalyn/" "/out-kalyn/" $ inputFilename
  else error $ "Kalyn source file outside src-kalyn: " ++ inputFilename

readIncrementally :: String -> IO [Decl]
readIncrementally inputFilename = do
  let prefix = getPrefix inputFilename
  str <- readFile inputFilename
  putStrLn $ "Lexer (" ++ takeFileName inputFilename ++ ")"
  let tokens = tokenize str
  overwriteFile (prefix ++ "Tokens") $ concatMap (\t -> show t ++ "\n") tokens
  putStrLn $ "Reader (" ++ takeFileName inputFilename ++ ")"
  let forms = readModule tokens
  overwriteFile (prefix ++ "Forms")
    $ concatMap (\f -> (T.unpack . pShowNoColor $ f) ++ "\n") forms
  overwriteFile (prefix ++ "Forms.kalyn")
    $ concatMap (\f -> pretty f ++ "\n") forms
  putStrLn $ "Parser (" ++ takeFileName inputFilename ++ ")"
  let decls = parseModule forms
  overwriteFile (prefix ++ "AST")
    $ concatMap (\d -> (T.unpack . pShowNoColor $ d) ++ "\n") decls
  overwriteFile (prefix ++ "AST.kalyn")
    $ concatMap (\d -> pretty d ++ "\n") decls
  return decls

compileIncrementally :: String -> IO ()
compileIncrementally inputFilename = do
  let prefix = getPrefix inputFilename
  putStrLn "Bundler"
  bundle <- readBundle readIncrementally inputFilename
  overwriteFile (prefix ++ "Bundle") $ T.unpack . pShowNoColor $ bundle
  overwriteFile (prefix ++ "Bundle.kalyn") $ pretty bundle
  putStrLn "Resolver"
  let resolver = resolveBundle bundle
  overwriteFile (prefix ++ "Resolver") $ pretty resolver
  putStrLn "TypeChecker"
  typeCheckBundle resolver bundle `seq` putStrLn "Translator"
  let virtualProgram = translateBundle resolver bundle
  overwriteFile (prefix ++ "Virtual.S") $ show virtualProgram
  putStrLn "Liveness"
  let liveness = computeProgramLiveness virtualProgram
  overwriteFile (prefix ++ "Liveness.S") $ showLiveness liveness
  putStrLn "RegisterAllocator"
  let (physicalProgram, allocation, spilled) =
        allocateProgramRegs virtualProgram (assertNoFreeVariablesP liveness)
  overwriteFile (prefix ++ "Raw.S") $ show physicalProgram
  overwriteFile (prefix ++ "Allocation") $ showAllocation allocation spilled
  putStrLn "Boilerplate"
  let physicalProgram' = addProgramBoilerplate physicalProgram
  overwriteFile (prefix ++ ".S") $ show physicalProgram'
  putStrLn "Assembler"
  let assembled@(codeB, dataB, _, _) = assemble physicalProgram'
  overwriteBinary
    (prefix ++ ".o")
    (  codeB
    <> B.pack
         (replicate (leftover pageSize (fromIntegral $ B.length codeB)) 0)
    <> dataB
    )
  putStrLn "Linker"
  let binary = link assembled
  overwriteBinary prefix binary
  setFileMode prefix 0o755

main :: IO ()
main = compileIncrementally "./src-kalyn/Main.kalyn"
