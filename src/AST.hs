module AST where

import           Data.Int

newtype ClassName = ClassName String
newtype TypeName = TypeName String
newtype VarName = VarName String

data ClassSpec = ClassSpec ClassName TypeName
data TypeSpec = TypeSpec TypeName [TypeName]
data Type = Type [ClassSpec] TypeName [Type]

data Expr = Variable String
          | Const Int64
          | Call Expr Expr
          | Case Expr [(Expr, Expr)]
          | Lambda [VarName] Expr
          | Let [(VarName, Expr)] Expr

data Decl = Alias Bool TypeSpec Type
          | Class Bool [ClassSpec] ClassSpec [(VarName, Type)]
          | Data Bool TypeSpec [(VarName, [TypeName])]
          | Def Bool VarName Type Expr
          | Derive ClassSpec
          | Import String
          | Instance [ClassSpec] ClassSpec [(VarName, Expr)]

instance Show ClassName where
  show (ClassName name) = name

instance Show TypeName where
  show (TypeName name) = name

instance Show VarName where
  show (VarName name) = name

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
  show (  Variable name) = name
  show (  Const    i   ) = show i
  show f@(Call _ _     ) = "(" ++ unwords (map show $ uncurryExpr f) ++ ")"
   where
    uncurryExpr (Call lhs rhs) = lhs : uncurryExpr rhs
    uncurryExpr form           = [form]
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
  show (Lambda args body) =
    "(lambda (" ++ unwords (map show args) ++ ") " ++ show body ++ ")"
  show (Let bindings body) =
    "(let ("
      ++ unwords
           ( flip map bindings
           $ \(name, expr) -> "(" ++ show name ++ " " ++ show expr ++ ")"
           )
      ++ ") "
      ++ show body
      ++ ")"

showPub :: Bool -> String
showPub False = ""
showPub True  = "pub "

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
             else unwords
               (flip map members $ \(name, args) -> if null args
                 then show name
                 else show name ++ " " ++ unwords (map show args)
               )
           )
        ++ ")"
    Def pub name typ (Lambda args body) ->
      "("
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
    Derive spec -> "(derive " ++ show spec ++ ")"
    Import str  -> "(import " ++ show str ++ ")"
    Instance constraints spec members ->
      "(instance ("
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
