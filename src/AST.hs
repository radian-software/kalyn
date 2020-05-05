module AST where

import           Codec.Binary.UTF8.String
import           Data.Int
import           Data.List
import qualified Data.Map                      as Map
import           Prelude                 hiding ( mod )

{-# ANN module "HLint: ignore Redundant flip" #-}
{-# ANN module "HLint: ignore Use record patterns" #-}

type ClassName = String
type TypeName = String
type VarName = String

data ClassSpec = ClassSpec ClassName TypeName
data TypeSpec = TypeSpec TypeName [TypeName]
data Type = Type [ClassSpec] TypeName [Type]

data Expr = Variable VarName
          | Const Int64
          | Call Expr Expr
          | Case Expr [(Expr, Expr)]
          | Lambda VarName Expr
          | Let VarName Expr Expr

data Decl = Alias Bool TypeSpec Type
          | Class Bool [ClassSpec] ClassSpec [(VarName, Type)]
          | Data Bool TypeSpec [(VarName, [Type])]
          | Def Bool VarName Type Expr
          | Derive Bool ClassSpec
          | Import Bool String
          | Instance Bool [ClassSpec] ClassSpec [(VarName, Expr)]

-- SymData (name) (ctor index) (num fields)
data Symbol = SymDef String | SymData String Int Int

symName :: Symbol -> String
symName (SymDef name     ) = name
symName (SymData name _ _) = name

data Bundle = Bundle String (Map.Map String ([Decl], [String]))
newtype Resolver = Resolver (Map.Map String (Map.Map String Symbol))

instance Show ClassSpec where
  show (ClassSpec cls typ) = "(" ++ show cls ++ " " ++ show typ ++ ")"

instance Show TypeSpec where
  show (TypeSpec name []) = show name
  show (TypeSpec name args) =
    "(" ++ show name ++ " " ++ unwords (map show args) ++ ")"

instance Show Type where
  show (Type constraints name args) =
    let inner = if null args
          then show name
          else "(" ++ show name ++ " " ++ unwords (map show args) ++ ")"
    in  if null constraints
          then inner
          else "(with " ++ unwords (map show constraints) ++ " " ++ inner ++ ")"

instance Show Expr where
  show (Variable name) = show name
  show (Const    i   ) = show i
  show (Call (Variable "Char") (Const c)) =
    show (toEnum $ fromIntegral c :: Char)
  show form@(Call _ _) = case unstringExpr form of
    Just bytes@(_ : _) -> show $ decode (map fromIntegral bytes)
    _                  -> case unlistExpr form of
      Just forms -> "[" ++ unwords (map show forms) ++ "]"
      _ -> "(" ++ unwords (map show $ reverse $ uncurryExpr form) ++ ")"
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
  show (Case expr branches) =
    "(case "
      ++ show expr
      ++ (if null branches
           then ""
           else " " ++ unwords
             ( flip map branches
             $ \(pat, res) -> "(" ++ show pat ++ " " ++ show res ++ ")"
             )
         )
      ++ ")"
  show form@(Lambda _ _) =
    let (body, args) = condenseLambdas form
    in  "(lambda (" ++ unwords (map show args) ++ ") " ++ show body ++ ")"
   where
    condenseLambdas (Lambda arg body) = (arg :) <$> condenseLambdas body
    condenseLambdas expr              = (expr, [])
  show form@(Let _ _ _) =
    let (body, bindings) = condenseLets form
    in  "(let ("
          ++ unwords
               (flip map bindings $ \(name, expr) ->
                 "(" ++ show name ++ " " ++ show expr ++ ")"
               )
          ++ ") "
          ++ show body
          ++ ")"
   where
    condenseLets (Let name expr body) = ((name, expr) :) <$> condenseLets body
    condenseLets expr                 = (expr, [])

instance Show Decl where
  show form = case form of
    Alias pub spec typ ->
      "(" ++ showPub pub ++ "alias " ++ show spec ++ " " ++ show typ ++ ")"
    Class pub constraints spec members ->
      "("
        ++ showPub pub
        ++ "class ("
        ++ (if null constraints
             then show spec
             else
               "with (" ++ unwords (map show constraints) ++ ") " ++ show spec
           )
        ++ ") "
        ++ unwords
             ( flip map members
             $ \(name, typ) -> "(" ++ show name ++ " " ++ show typ ++ ")"
             )
        ++ ")"
    Data pub spec members ->
      "("
        ++ showPub pub
        ++ "data "
        ++ show spec
        ++ (if null members
             then ""
             else " " ++ unwords
               (flip map members $ \(name, args) -> if null args
                 then show name
                 else show name ++ " " ++ unwords (map show args)
               )
           )
        ++ ")"
    Def pub name typ form'@(Lambda _ _) ->
      let (body, args) = condenseLambdas form'
      in  "("
            ++ showPub pub
            ++ "defn "
            ++ show name
            ++ " "
            ++ show typ
            ++ " ("
            ++ unwords (map show args)
            ++ ") "
            ++ show body
            ++ ")"
     where
      condenseLambdas (Lambda arg body) = (arg :) <$> condenseLambdas body
      condenseLambdas expr              = (expr, [])
    Def pub name typ expr ->
      "("
        ++ showPub pub
        ++ "def "
        ++ show name
        ++ " "
        ++ show typ
        ++ " "
        ++ show expr
        ++ ")"
    Derive pub spec -> "(" ++ showPub pub ++ "derive " ++ show spec ++ ")"
    Import pub str  -> "(" ++ showPub pub ++ "import " ++ show str ++ ")"
    Instance pub constraints spec members ->
      "("
        ++ showPub pub
        ++ "instance ("
        ++ (if null constraints
             then show spec
             else
               "with (" ++ unwords (map show constraints) ++ ") " ++ show spec
           )
        ++ ") "
        ++ unwords
             ( flip map members
             $ \(name, expr) -> "(" ++ show name ++ " " ++ show expr ++ ")"
             )
        ++ ")"
   where
    showPub False = ""
    showPub True  = "public "

instance Show Symbol where
  show (SymDef name) = "regular symbol " ++ name
  show (SymData name ctorIdx numFields) =
    "data constructor "
      ++ name
      ++ " with index "
      ++ show ctorIdx
      ++ " and "
      ++ show numFields
      ++ " fields"

instance Show Bundle where
  show (Bundle main modules) =
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
               else "\n" ++ flip concatMap decls (\d -> show d ++ "\n")
             )
      )

instance Show Resolver where
  show (Resolver resolver) =
    concatMap
        (\(mod, syms) -> "module " ++ show mod ++ "\n" ++ concatMap
          (\(name, sym) -> "  " ++ name ++ " -> " ++ show sym ++ "\n")
          (Map.toList syms)
        )
      $ Map.toList resolver
