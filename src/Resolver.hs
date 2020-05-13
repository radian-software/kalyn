module Resolver
  ( resolveBundle
  )
where

import           Data.Char
import           Data.Int
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Prelude                 hiding ( mod )
import           System.FilePath

import           AST
import           Bridge
import           Util

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Use tuple-section" #-}

mapTypeName :: (String -> String) -> String -> String
mapTypeName _ "IO"                     = "IO"
mapTypeName _ "Int"                    = "Int"
mapTypeName _ "Func"                   = "Func"
mapTypeName _ name@(c : _) | isLower c = name
mapTypeName func name                  = func name

mapTypeSpec :: (String -> String) -> TypeSpec -> TypeSpec
mapTypeSpec func (TypeSpec typeName params) =
  TypeSpec (mapTypeName func typeName) params

mapType :: (String -> String) -> Type -> Type
mapType func (Type classSpecs typeName typeArgs) =
  Type classSpecs (mapTypeName func typeName) (map (mapType func) typeArgs)

mapSymbol :: (String -> String) -> (String -> String) -> Symbol -> Symbol
mapSymbol func tfunc (SymDef name t) = SymDef (func name) (mapType tfunc t)
mapSymbol func tfunc (SymData name ctorIdx numFields numCtors boxed typeSpec types)
  = SymData (func name)
            ctorIdx
            numFields
            numCtors
            boxed
            (mapTypeSpec tfunc typeSpec)
            (map (mapType tfunc) types)

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

getComponents :: FilePath -> [String]
getComponents = reverse . tail . splitDirectories

sanitizeModuleName :: Int -> FilePath -> FilePath
sanitizeModuleName n path =
  sanitize $ concat (reverse . take n $ getComponents path)

sanitizeModuleNames :: [String] -> Map.Map String String
sanitizeModuleNames fullNames =
  let maxComponents = maximum $ (map $ length . getComponents) fullNames
      xforms =
          map (\n names -> map (sanitizeModuleName n) names) [1 .. maxComponents]
            ++ [uniquify . map (sanitizeModuleName maxComponents)]
      bestXForm = head $ filter (\xform -> listUnique $ xform fullNames) xforms
  in  Map.fromList $ zip fullNames (bestXForm fullNames)

-- for now, doesn't handle Derive or Instance
getDeclSymbols :: Decl -> [Symbol]
getDeclSymbols (Data _ typeSpec ctors) = zipWith
  (\(name, types) idx -> SymData { sdName      = name
                                 , sdCtorIdx   = idx
                                 , sdNumFields = length types
                                 , sdNumCtors  = length ctors
                                 , sdBoxed     = shouldBox ctors
                                 , sdTypeSpec  = typeSpec
                                 , sdTypes     = types
                                 }
  )
  ctors
  (iterate (+ 1) 0)
getDeclSymbols (Def _ name t _) = [SymDef name t]
getDeclSymbols _                = []

getDeclTypes :: Decl -> [TypeName]
getDeclTypes (Data _ (TypeSpec name _) _) = [name]
getDeclTypes _                            = []

getDeclAliases :: Decl -> [(TypeSpec, Type)]
getDeclAliases (Alias _ typeSpec t) = [(typeSpec, t)]
getDeclAliases _                    = []

mangleWith :: String -> String -> String
mangleWith modAbbr name = "__" ++ modAbbr ++ "__" ++ sanitize name

resolveBundle :: Bundle -> Resolver
resolveBundle (Bundle _ mmap) =
  let
    modNames = sanitizeModuleNames (map fst $ Map.toList mmap)
    gTypeMap = Map.mapWithKey
      (\mainMod info ->
        let mods = mainMod : snd info
        in  Map.fromListWithKey
                (\name _ ->
                  error $ "more than one definition for type " ++ show name
                )
              $ concatMap
                  (\mod ->
                    let modAbbr = modNames Map.! mod
                        decls   = fst . (mmap Map.!) $ mod
                    in  map (\name -> (name, mangleWith modAbbr name))
                          $  concatMap getDeclTypes decls
                          ++ ( map (\(TypeSpec name _, _) -> name)
                             . concatMap getDeclAliases
                             $ decls
                             )
                  )
                  mods
      )
      mmap
    gAliasMap =
      Map.fromListWithKey
          (\name _ -> error $ "more than one alias for " ++ show name)
        . concatMap
            (\(mod, info) ->
              let modAbbr = modNames Map.! mod
              in
                map
                  (\(TypeSpec aliasName params, aliasDefn) ->
                    ( mangleWith modAbbr aliasName
                    , ( params
                      , mapType
                        (\name -> case name `Map.lookup` (gTypeMap Map.! mod) of
                          Nothing ->
                            error $ "no such type in alias: " ++ show name
                          Just name' -> name'
                        )
                        aliasDefn
                      )
                    )
                  )
                . concatMap getDeclAliases
                . fst
                $ info
            )
        . Map.toList
        $ mmap
  in
    Resolver $ Map.mapWithKey
      (\mainMod info ->
        let
          mods = mainMod : snd info
          symbolMap =
            Map.fromListWithKey
                (\name _ ->
                  error $ "more than one definition for symbol " ++ show name
                )
              $  concatMap
                   (\mod ->
                     let modAbbr = modNames Map.! mod
                     in
                       map
                         (\sym ->
                           ( symName sym
                           , mapSymbol
                             (mangleWith modAbbr)
                             (\name ->
                               case name `Map.lookup` (gTypeMap Map.! mod) of
                                 Nothing ->
                                   error $ "no such type: " ++ show name
                                 Just name' -> name'
                             )
                             sym
                           )
                         )
                       . concatMap getDeclSymbols
                       . fst
                       . (mmap Map.!)
                       $ mod
                   )
                   mods
              ++ map
                   (\(publicName, (privateName, _, ty)) ->
                     let sym = SymDef privateName ty
                     in
                       ( publicName
                       , mapSymbol
                         id
                         (\name ->
                           case name `Map.lookup` (gTypeMap Map.! mainMod) of
                             Nothing ->
                               error
                                 $  "no such type "
                                 ++ show name
                                 ++ " in module: "
                                 ++ mainMod
                             Just name' -> name'
                         )
                         sym
                       )
                   )
                   (Map.toList stdlibPublic)
        in
          ( symbolMap
          , Map.restrictKeys
            gAliasMap
            ( Set.fromList
            . concatMap
                (\mod ->
                  let modAbbr = modNames Map.! mod
                  in  ( map (\(TypeSpec name _, _) -> mangleWith modAbbr name)
                      . concatMap getDeclAliases
                      . fst
                      . (mmap Map.!)
                      $ mod
                      )
                )
            $ mods
            )
          )
      )
      mmap
