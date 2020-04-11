module Parser
  ( parseModule
  )
where

import           AST
import           Codec.Binary.UTF8.String
import           Tokens

withConstraints :: (Form -> a) -> Form -> ([ClassSpec], a)
withConstraints parseBody (RoundList [Symbol "with", RoundList specs, body]) =
  (map parseClassSpec specs, parseBody body)
withConstraints _ form@(RoundList (Symbol "with" : _)) =
  error $ "failed to parse constraint list: " ++ show form
withConstraints parseBody form = ([], parseBody form)

parseTypeSpec :: Form -> TypeSpec
parseTypeSpec (RoundList ((Symbol name) : args)) = TypeSpec
  (TypeName name)
  (flip map args $ \arg -> case arg of
    Symbol argName -> TypeName argName
    _              -> error $ "failed to parse type spec argument: " ++ show arg
  )
parseTypeSpec (Symbol name) = TypeSpec (TypeName name) []
parseTypeSpec form          = error $ "failed to parse type spec: " ++ show form

parseType :: Form -> Type
parseType form@(RoundList (Symbol "with" : _)) =
  let (specs, Type moreSpecs name args) = withConstraints parseType form
  in  Type (specs ++ moreSpecs) name args
parseType (RoundList (Symbol name : args)) =
  Type [] (TypeName name) (map parseType args)
parseType (Symbol name) = Type [] (TypeName name) []
parseType form          = error $ "failed to parse type: " ++ show form

parseClassSpec :: Form -> ClassSpec
parseClassSpec (RoundList [Symbol name, Symbol typ]) =
  ClassSpec (ClassName name) (TypeName typ)
parseClassSpec form = error $ "failed to parse class spec: " ++ show form

parseExpr :: Form -> Expr
parseExpr (RoundList []) = error "round list can't be empty"
parseExpr (RoundList (Symbol "case" : expr : branches)) = Case
  (parseExpr expr)
  (flip map branches $ \br -> case br of
    RoundList [pat, res] -> (parseExpr pat, parseExpr res)
    _                    -> error $ "failed to parse case branch: " ++ show br
  )
parseExpr (RoundList [Symbol "lambda", RoundList args, body]) = foldr
  Lambda
  (parseExpr body)
  (flip map args $ \arg -> case arg of
    Symbol name -> VarName name
    _           -> error $ "failed to parse lambda argument: " ++ show arg
  )
parseExpr (RoundList [Symbol "let", RoundList bindings, body]) = foldr
  (uncurry Let)
  (parseExpr body)
  (flip map bindings $ \binding -> case binding of
    RoundList [Symbol name, val] -> (VarName name, parseExpr val)
    _ -> error $ "failed to parse let binding: " ++ show binding
  )
parseExpr (RoundList  elts) = foldl1 Call (map parseExpr elts)
parseExpr (SquareList elts) = parseExpr $ foldr
  (\char rest -> RoundList [Symbol "Cons", char, rest])
  (Symbol "Null")
  elts
parseExpr (Symbol   name) = Variable $ VarName name
parseExpr (IntAtom  i   ) = Const i
parseExpr (CharAtom c   ) = parseExpr $ StrAtom [c]
parseExpr (StrAtom  s   ) = parseExpr $ SquareList
  (map (\c -> RoundList [Symbol "Char", IntAtom (fromIntegral c)]) $ encode s)

parseDecl :: Form -> Decl
parseDecl form = case form of
  (RoundList (Symbol "public" : rest)) -> parseDecl' True (RoundList rest)
  _ -> parseDecl' False form
 where
  parseDecl' pub (RoundList [Symbol "alias", spec, typ]) =
    Alias pub (parseTypeSpec spec) (parseType typ)
  parseDecl' pub (RoundList (Symbol "class" : spec : members)) =
    let (constraints, innerSpec) = withConstraints parseClassSpec spec
    in  Class
          pub
          constraints
          innerSpec
          (flip map members $ \m -> case m of
            RoundList [Symbol name, typ] -> (VarName name, parseType typ)
            _ -> error $ "failed to parse class member: " ++ show m
          )
  parseDecl' pub (RoundList (Symbol "data" : spec : members)) = Data
    pub
    (parseTypeSpec spec)
    (flip map members $ \m -> case m of
      Symbol name -> (VarName name, [])
      RoundList (Symbol name : args) -> (VarName name, map parseType args)
      _ -> error $ "failed to parse data constructor: " ++ show m
    )
  parseDecl' pub (RoundList [Symbol "def", Symbol name, typ, expr]) =
    Def pub (VarName name) (parseType typ) (parseExpr expr)
  parseDecl' pub (RoundList [Symbol "defn", name, typ, args, body]) =
    parseDecl'
      pub
      (RoundList
        [Symbol "def", name, typ, RoundList [Symbol "lambda", args, body]]
      )
  parseDecl' pub (RoundList [Symbol "derive", spec]) =
    Derive pub (parseClassSpec spec)
  parseDecl' pub (RoundList [Symbol "import", StrAtom file]) = Import pub file
  parseDecl' pub (RoundList (Symbol "instance" : spec : members)) =
    let (constraints, innerSpec) = withConstraints parseClassSpec spec
    in  Instance
          pub
          constraints
          innerSpec
          (flip map members $ \m -> case m of
            RoundList [Symbol name, expr] -> (VarName name, parseExpr expr)
            _ -> error $ "failed to parse instance member: " ++ show m
          )
  parseDecl' _ _ = error $ "failed to parse declaration: " ++ show form

parseModule :: [Form] -> [Decl]
parseModule = map parseDecl
