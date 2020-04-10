module Bundler where

import           Data.List
import qualified Data.Map.Strict               as Map
import           System.Directory
import           System.FilePath

import           AST

-- import in stack order
import           Lexer
import           Reader
import           Parser

extractImports :: [Decl] -> ([(FilePath, Bool)], [Decl])
extractImports [] = ([], [])
extractImports (Import pub file : decls) =
  let (files, decls') = extractImports decls in ((file, pub) : files, decls')
extractImports (decl : decls) =
  let (files, decls') = extractImports decls in (files, decl : decls')

readBundle'
  :: [FilePath]
  -> [FilePath]
  -> Map.Map FilePath ([Decl], [(FilePath, Bool)])
  -> IO (Map.Map FilePath ([Decl], [(FilePath, Bool)]))
readBundle' _ [] declsSoFar = return declsSoFar
readBundle' alreadyRead (curToRead : restToRead) declsSoFar =
  if curToRead `elem` alreadyRead
    then readBundle' alreadyRead restToRead declsSoFar
    else do
      str <- readFile curToRead
      let tokens                = tokenize str
          forms                 = readModule tokens
          (newToRead, newDecls) = extractImports $ parseModule forms
      absNewToRead <- withCurrentDirectory (takeDirectory curToRead) $ mapM
        (\(path, pub) -> do
          absPath <- canonicalizePath path
          return (absPath, pub)
        )
        newToRead
      readBundle' (curToRead : alreadyRead)
                  (map fst absNewToRead ++ restToRead)
                  (Map.insert curToRead (newDecls, absNewToRead) declsSoFar)

transitiveImports
  :: Map.Map FilePath ([Decl], [(FilePath, Bool)])
  -> [FilePath]
  -> [FilePath]
  -> [FilePath]
  -> [FilePath]
transitiveImports _       _    []           result = result
transitiveImports modules seen (cur : next) result = if cur `elem` seen
  then transitiveImports modules seen next result
  else
    let (_, deps) = modules Map.! cur
        new       = map fst $ filter snd deps
    in  transitiveImports modules (cur : seen) (new ++ next) (cur : result)

readBundle :: FilePath -> IO Bundle
readBundle filename = do
  absFilename <- canonicalizePath filename
  modules     <- readBundle' [] [absFilename] Map.empty
  return $ Bundle $ Map.mapWithKey
    (\name (decls, _) ->
      ( decls
      , nub $ transitiveImports modules
                                [name]
                                (map fst $ snd $ modules Map.! name)
                                []
      )
    )
    modules
