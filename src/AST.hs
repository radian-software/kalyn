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
