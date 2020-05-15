module Bundler
  ( readBundle
  )
where

import           Data.List
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           System.Directory
import           System.FilePath

import           AST

extractImports :: [Decl] -> ([(FilePath, Bool)], [Decl])
extractImports [] = ([], [])
extractImports (Import pub file : decls) =
  let (files, decls') = extractImports decls in ((file, pub) : files, decls')
extractImports (decl : decls) =
  let (files, decls') = extractImports decls in (files, decl : decls')

readBundle'
  :: (String -> IO [Decl])
  -> [FilePath]
  -> [FilePath]
  -> Map.Map FilePath ([Decl], [(FilePath, Bool)])
  -> IO (Map.Map FilePath ([Decl], [(FilePath, Bool)]))
readBundle' _ _ [] declsSoFar = return declsSoFar
readBundle' readDecls alreadyRead (curToRead : restToRead) declsSoFar =
  if curToRead `elem` alreadyRead
    then readBundle' readDecls alreadyRead restToRead declsSoFar
    else do
      (newToRead, newDecls) <- extractImports <$> readDecls curToRead
      absNewToRead <- withCurrentDirectory (takeDirectory curToRead) $ mapM
        (\(path, pub) -> do
          absPath <- canonicalizePath path
          return (absPath, pub)
        )
        newToRead
      readBundle' readDecls
                  (curToRead : alreadyRead)
                  (map fst absNewToRead ++ restToRead)
                  (Map.insert curToRead (newDecls, absNewToRead) declsSoFar)

transitiveImports
  :: Map.Map FilePath ([Decl], [(FilePath, Bool)])
  -> Set.Set FilePath
  -> [FilePath]
  -> [FilePath]
  -> [FilePath]
transitiveImports _       _    []           result = result
transitiveImports modules seen (cur : next) result = if Set.member cur seen
  then transitiveImports modules seen next result
  else
    let (_, deps) = modules Map.! cur
        new       = map fst $ filter snd deps
    in  transitiveImports modules
                          (Set.insert cur seen)
                          (new ++ next)
                          (cur : result)

readBundle :: IO () -> (String -> IO [Decl]) -> FilePath -> IO Bundle
readBundle onReadFinished readDecls filename = do
  absFilename <- canonicalizePath filename
  modules     <- readBundle' readDecls [] [absFilename] Map.empty
  onReadFinished
  return $ Bundle absFilename $ Map.mapWithKey
    (\name (decls, _) ->
      ( decls
      , nub $ transitiveImports modules
                                (Set.singleton name)
                                (map fst $ snd $ modules Map.! name)
                                []
      )
    )
    modules
