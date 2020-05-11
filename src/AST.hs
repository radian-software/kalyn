module AST where

import           Codec.Binary.UTF8.String
import           Data.Int
import           Data.List
import qualified Data.Map                      as Map
import           Prelude                 hiding ( mod )

import           Util

{-# ANN module "HLint: ignore Redundant flip" #-}
{-# ANN module "HLint: ignore Use record patterns" #-}

type ClassName = String
type TypeName = String
type VarName = String

data ClassSpec = ClassSpec ClassName TypeName
  deriving (Show)

data TypeSpec = TypeSpec TypeName [TypeName]
  deriving (Show)

data Type = Type [ClassSpec] TypeName [Type]
  deriving (Show)

data Expr = Variable VarName
          | Const Int64
          | Call Expr Expr
          | Case Expr [(Expr, Expr)]
          | Lambda VarName Expr
          | Let VarName Expr Expr
  deriving (Show)

data Decl = Alias Bool TypeSpec Type
          | Class Bool [ClassSpec] ClassSpec [(VarName, Type)]
          | Data Bool TypeSpec [(VarName, [Type])]
          | Def Bool VarName Type Expr
          | Derive Bool ClassSpec
          | Import Bool String
          | Instance Bool [ClassSpec] ClassSpec [(VarName, Expr)]
  deriving (Show)

data Symbol = SymDef String
            | SymData { sdName :: String
                      , sdCtorIdx :: Int
                      , sdNumFields :: Int
                      , sdNumCtors :: Int
                      }

sdHasHeader :: Symbol -> Bool
sdHasHeader SymData { sdNumCtors = numCtors } = numCtors > 1
sdHasHeader _ = error "can only call sdHasHeader on SymDef"

sdBoxed :: Symbol -> Bool
sdBoxed SymData { sdNumFields = numFields, sdNumCtors = numCtors } =
  numFields <= 1 && numCtors <= 1
sdBoxed _ = error "can only call sdBoxed on SymDef"

symName :: Symbol -> String
symName (SymDef name       ) = name
symName (SymData name _ _ _) = name

data Bundle = Bundle String (Map.Map String ([Decl], [String]))
newtype Resolver = Resolver (Map.Map String (Map.Map String Symbol))

instance Pretty ClassSpec where
  pretty (ClassSpec cls typ) = "(" ++ cls ++ " " ++ show typ ++ ")"

instance Pretty TypeSpec where
  pretty (TypeSpec name []) = name
  pretty (TypeSpec name args) =
    "(" ++ name ++ " " ++ unwords (map show args) ++ ")"

instance Pretty Type where
  pretty (Type constraints name args) =
    let inner = if null args
          then name
          else "(" ++ name ++ " " ++ unwords (map pretty args) ++ ")"
    in  if null constraints
          then inner
          else
            "(with " ++ unwords (map pretty constraints) ++ " " ++ inner ++ ")"

instance Pretty Expr where
  pretty (Variable name) = name
  pretty (Const    i   ) = show i
  pretty (Call (Variable "Char") (Const c)) =
    show (toEnum $ fromIntegral c :: Char)
  pretty form@(Call _ _) = case unstringExpr form of
    Just bytes@(_ : _) -> show $ decode (map fromIntegral bytes)
    _                  -> case unlistExpr form of
      Just forms -> "[" ++ unwords (map pretty forms) ++ "]"
      _ -> "(" ++ unwords (map pretty $ reverse $ uncurryExpr form) ++ ")"
   where
    unstringExpr (Variable "Null") = Just []
    unstringExpr (Call (Call (Variable "Cons") (Call (Variable "Char") (Const first))) rest)
      = unstringExpr rest >>= (Just . (first :))
    unstringExpr _ = Nothing
    unlistExpr (Variable "Null") = Just []
    unlistExpr (Call (Call (Variable "Cons") first) rest) =
      unlistExpr rest >>= (Just . (first :))
    unlistExpr _ = Nothing
    uncurryExpr (Call lhs rhs) = rhs : uncurryExpr lhs
    uncurryExpr f              = [f]
  pretty (Case expr branches) =
    "(case "
      ++ pretty expr
      ++ (if null branches
           then ""
           else " " ++ unwords
             ( flip map branches
             $ \(pat, res) -> "(" ++ pretty pat ++ " " ++ pretty res ++ ")"
             )
         )
      ++ ")"
  pretty form@(Lambda _ _) =
    let (body, args) = condenseLambdas form
    in  "(lambda (" ++ unwords (map show args) ++ ") " ++ pretty body ++ ")"
   where
    condenseLambdas (Lambda arg body) = (arg :) <$> condenseLambdas body
    condenseLambdas expr              = (expr, [])
  pretty form@(Let _ _ _) =
    let (body, bindings) = condenseLets form
    in  "(let ("
          ++ unwords
               ( flip map bindings
               $ \(name, expr) -> "(" ++ name ++ " " ++ pretty expr ++ ")"
               )
          ++ ") "
          ++ pretty body
          ++ ")"
   where
    condenseLets (Let name expr body) = ((name, expr) :) <$> condenseLets body
    condenseLets expr                 = (expr, [])

instance Pretty Decl where
  pretty form = case form of
    Alias pub spec typ ->
      "("
        ++ prettyPub pub
        ++ "alias "
        ++ pretty spec
        ++ " "
        ++ pretty typ
        ++ ")"
    Class pub constraints spec members ->
      "("
        ++ prettyPub pub
        ++ "class ("
        ++ (if null constraints
             then pretty spec
             else
               "with ("
               ++ unwords (map pretty constraints)
               ++ ") "
               ++ pretty spec
           )
        ++ ") "
        ++ unwords
             ( flip map members
             $ \(name, typ) -> "(" ++ name ++ " " ++ pretty typ ++ ")"
             )
        ++ ")"
    Data pub spec members ->
      "("
        ++ prettyPub pub
        ++ "data "
        ++ pretty spec
        ++ (if null members
             then ""
             else " " ++ unwords
               (flip map members $ \(name, args) -> if null args
                 then name
                 else name ++ " " ++ unwords (map pretty args)
               )
           )
        ++ ")"
    Def pub name typ form'@(Lambda _ _) ->
      let (body, args) = condenseLambdas form'
      in  "("
            ++ prettyPub pub
            ++ "defn "
            ++ name
            ++ " "
            ++ pretty typ
            ++ " ("
            ++ unwords (map show args)
            ++ ") "
            ++ pretty body
            ++ ")"
     where
      condenseLambdas (Lambda arg body) = (arg :) <$> condenseLambdas body
      condenseLambdas expr              = (expr, [])
    Def pub name typ expr ->
      "("
        ++ prettyPub pub
        ++ "def "
        ++ name
        ++ " "
        ++ pretty typ
        ++ " "
        ++ pretty expr
        ++ ")"
    Derive pub spec -> "(" ++ prettyPub pub ++ "derive " ++ pretty spec ++ ")"
    Import pub str  -> "(" ++ prettyPub pub ++ "import " ++ show str ++ ")"
    Instance pub constraints spec members ->
      "("
        ++ prettyPub pub
        ++ "instance ("
        ++ (if null constraints
             then pretty spec
             else
               "with ("
               ++ unwords (map pretty constraints)
               ++ ") "
               ++ pretty spec
           )
        ++ ") "
        ++ unwords
             ( flip map members
             $ \(name, expr) -> "(" ++ name ++ " " ++ pretty expr ++ ")"
             )
        ++ ")"
   where
    prettyPub False = ""
    prettyPub True  = "public "

instance Pretty Symbol where
  pretty (SymDef name) = "regular symbol " ++ name
  pretty sd@(SymData _ _ _ _) =
    "data constructor "
      ++ sdName sd
      ++ " with index "
      ++ show (sdCtorIdx sd)
      ++ " out of "
      ++ show (sdNumCtors sd)
      ++ " and "
      ++ show (sdNumFields sd)
      ++ " field"
      ++ (if sdNumFields sd == 1 then "" else "s")
      ++ " ("
      ++ (if sdBoxed sd then "boxed" else "unboxed")
      ++ ", "
      ++ (if sdHasHeader sd then "with" else "no")
      ++ " header word)"

instance Pretty Bundle where
  pretty (Bundle main modules) =
    ";; main module: " ++ show main ++ "\n\n" ++ intercalate
      "\n"
      (flip map (Map.toList modules) $ \(name, (decls, deps)) ->
        replicate 80 ';'
          ++ "\n;; module "
          ++ show name
          ++ "\n"
          ++ (if null deps
               then ""
               else "\n"
                 ++ flip concatMap deps (\d -> "(import " ++ show d ++ ")\n")
             )
          ++ (if null decls
               then ""
               else "\n" ++ flip concatMap decls (\d -> pretty d ++ "\n")
             )
      )

instance Pretty Resolver where
  pretty (Resolver resolver) =
    concatMap
        (\(mod, syms) -> "module " ++ show mod ++ "\n" ++ concatMap
          (\(name, sym) -> "  " ++ name ++ " -> " ++ pretty sym ++ "\n")
          (Map.toList syms)
        )
      $ Map.toList resolver
