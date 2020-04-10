module Bundler where

import           System.Directory
import           System.FilePath

import           AST

-- import in stack order
import           Lexer
import           Reader
import           Parser

extractImports :: [Decl] -> ([String], [Decl])
extractImports [] = ([], [])
extractImports (Import file : decls) =
  let (files, decls') = extractImports decls in (file : files, decls')
extractImports (decl : decls) =
  let (files, decls') = extractImports decls in (files, decl : decls')

readBundle'
  :: [String]
  -> [String]
  -> [(String, [Decl], [String])]
  -> IO [(String, [Decl], [String])]
readBundle' _ [] declsSoFar = return declsSoFar
readBundle' alreadyRead (curToRead : restToRead) declsSoFar =
  if curToRead `elem` alreadyRead
    then readBundle' alreadyRead restToRead declsSoFar
    else do
      str <- readFile curToRead
      let tokens                = tokenize str
          forms                 = readModule tokens
          (newToRead, newDecls) = extractImports $ parseModule forms
      absNewToRead <- withCurrentDirectory (takeDirectory curToRead)
        $ mapM canonicalizePath newToRead
      readBundle' (curToRead : alreadyRead)
                  (absNewToRead ++ restToRead)
                  ((curToRead, newDecls, absNewToRead) : declsSoFar)

readBundle :: FilePath -> IO Bundle
readBundle filename = do
  absFilename <- canonicalizePath filename
  modules     <- readBundle' [] [absFilename] []
  return $ Bundle modules
