module Main where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString.Lazy          as B
import           Data.List
import           Data.Maybe
import qualified Data.Text.Lazy                as T
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Error
import           System.Posix.Files
import           Text.Pretty.Simple             ( pShowNoColor )

import           AST
import           OS
import           Util

import           Assembler
import           Boilerplate
import           Bundler
import           Lexer
import           Linker
import           Liveness
import           Parser
import           Reader
import           RegisterAllocator
import           Resolver
import           Translator
import           TypeChecker

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
  then dropExtension . T.unpack $ T.replace (T.pack "/src-kalyn/")
                                            (T.pack "/out-kalyn/")
                                            (T.pack inputFilename)
  else error $ "Kalyn source file outside src-kalyn: " ++ inputFilename

readIncrementally :: Bool -> String -> IO [Decl]
readIncrementally verbose inputFilename = do
  let prefix = getPrefix inputFilename
  str <- readFile inputFilename
  putStrLn $ "Lexer (" ++ takeFileName inputFilename ++ ")"
  let tokens = tokenize str
  when verbose $ overwriteFile (prefix ++ "Tokens") $ concatMap
    (\t -> show t ++ "\n")
    tokens
  tokens `deepseq` putStrLn $ "Reader (" ++ takeFileName inputFilename ++ ")"
  let forms = readModule tokens
  when verbose $ do
    overwriteFile (prefix ++ "Forms")
      $ concatMap (\f -> (T.unpack . pShowNoColor $ f) ++ "\n") forms
    overwriteFile (prefix ++ "Forms.kalyn")
      $ concatMap (\f -> pretty f ++ "\n") forms
  forms `deepseq` putStrLn $ "Parser (" ++ takeFileName inputFilename ++ ")"
  let decls = parseModule forms
  when verbose $ do
    overwriteFile (prefix ++ "AST")
      $ concatMap (\d -> (T.unpack . pShowNoColor $ d) ++ "\n") decls
    overwriteFile (prefix ++ "AST.kalyn")
      $ concatMap (\d -> pretty d ++ "\n") decls
  decls `deepseq` return decls

compileIncrementally :: Bool -> String -> IO ()
compileIncrementally verbose inputFilename = do
  let prefix = getPrefix inputFilename
  bundle <- readBundle (putStrLn "Bundler")
                       (readIncrementally verbose)
                       inputFilename
  when verbose $ do
    overwriteFile (prefix ++ "Bundle") $ T.unpack . pShowNoColor $ bundle
    overwriteFile (prefix ++ "Bundle.kalyn") $ pretty bundle
  bundle `deepseq` putStrLn "Resolver"
  let resolver = resolveBundle bundle
  when verbose $ overwriteFile (prefix ++ "Resolver") $ pretty resolver
  resolver `deepseq` putStrLn "TypeChecker"
  let typeChecked = typeCheckBundle resolver bundle
  typeChecked `deepseq` putStrLn "Translator"
  let (virtualProgram, translatorState) =
        flip runState 0 $ translateBundle resolver bundle
  when verbose $ overwriteFile (prefix ++ "Virtual.S") $ show virtualProgram
  virtualProgram `deepseq` putStrLn "Liveness"
  let liveness = computeProgramLiveness virtualProgram
  when verbose $ overwriteFile (prefix ++ "Liveness.S") $ showLiveness liveness
  liveness `deepseq` putStrLn "RegisterAllocator"
  let info@(physicalProgram, allocation, spilled) =
        flip evalState translatorState
          $ allocateProgramRegs virtualProgram (assertNoFreeVariablesP liveness)
  when verbose $ do
    overwriteFile (prefix ++ "Raw.S") $ show physicalProgram
    overwriteFile (prefix ++ "Allocation") $ showAllocation allocation spilled
  info `deepseq` putStrLn "Boilerplate"
  let physicalProgram' = addProgramBoilerplate physicalProgram
  when verbose $ overwriteFile (prefix ++ ".S") $ show physicalProgram'
  physicalProgram' `deepseq` putStrLn "Assembler"
  let assembled@(codeB, dataB, _, _) = assemble physicalProgram'
  when verbose $ overwriteBinary
    (prefix ++ ".o")
    (  codeB
    <> B.pack
         (replicate (leftover pageSize (fromIntegral $ B.length codeB)) 0)
    <> dataB
    )
  assembled `deepseq` putStrLn "Linker"
  let binary = link assembled
  overwriteBinary prefix binary
  setFileMode prefix 0o755

main :: IO ()
main = do
  args <- getArgs
  env  <- lookupEnv "VERBOSE"
  let verbose = "-v" `elem` args || "--verbose" `elem` args || isJust
        (not . null <$> env)
  compileIncrementally verbose "./src-kalyn/Main.kalyn"
