module Resolver where

import           Data.Char
import           Data.Int
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Prelude                 hiding ( mod )
import           System.FilePath

import           AST
import           Util

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Use tuple-section" #-}

mapSymbol :: (String -> String) -> Symbol -> Symbol
mapSymbol f (SymDef name               ) = SymDef (f name)
mapSymbol f (SymData name idx numFields) = SymData (f name) idx numFields

userAllowedChars :: String
userAllowedChars = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

-- important properties: deterministic, stateless, and injective
sanitize :: String -> String
sanitize = concatMap
  $ \c -> if c `elem` userAllowedChars then [c] else "_u" ++ show (ord c)

uniquify :: [String] -> [String]
uniquify = uniquify' Set.empty
 where
  uniquify' _ [] = []
  uniquify' seen (str : strs) =
    let str' = findUnused str seen
    in  (str' : uniquify' (Set.insert str seen) strs)
  findUnused str seen = head $ filter (`Set.notMember` seen) $ str : map
    (\num -> str ++ show num)
    (iterate (+ 1) (1 :: Int))

sanitizeModuleName :: Int -> FilePath -> FilePath
sanitizeModuleName n path = sanitize $ concat (take n $ splitPath path)

sanitizeModuleNames :: [String] -> Map.Map String String
sanitizeModuleNames fullNames =
  let maxComponents = maximum $ (map $ length . splitPath) fullNames
      xforms =
          map (\n names -> map (sanitizeModuleName n) names) [1 .. maxComponents]
            ++ [uniquify . map (sanitizeModuleName maxComponents)]
      bestXForm = head $ filter (\xform -> listUnique $ xform fullNames) xforms
  in  Map.fromList $ zip fullNames (bestXForm fullNames)

-- for now, doesn't handle Derive or Instance
getDeclSymbols :: Decl -> [Symbol]
getDeclSymbols (Alias _ _ _   ) = []
getDeclSymbols (Class _ _ _ _ ) = []
getDeclSymbols (Data _ _ ctors) = zipWith
  (\(name, _) idx -> SymData name idx (length ctors))
  ctors
  (iterate (+ 1) 0)
getDeclSymbols (Def _ name _ _  ) = [SymDef name]
getDeclSymbols (Derive _ _      ) = []
getDeclSymbols (Import _ _) = error "resolver shouldn't be handling imports"
getDeclSymbols (Instance _ _ _ _) = []

resolveBundle :: Bundle -> Resolver
resolveBundle (Bundle mmap) =
  let modNames = sanitizeModuleNames (map fst $ Map.toList mmap)
  in
    Map.mapWithKey
      (\mainMod info ->
        let mods = mainMod : snd info
        in
          Map.fromList $ map
            (\mod ->
              let
                modAbbr = modNames Map.! mod
                syms =
                  map
                      (mapSymbol
                        (\name -> "__" ++ modAbbr ++ "__" ++ sanitize name)
                      )
                    $     concatMap getDeclSymbols
                    $     fst
                    $     mmap
                    Map.! mod
              in
                (mod, syms)
            )
            mods
      )
      mmap
