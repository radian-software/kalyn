module Parser
  ( parseModule
  )
where

import           AST
import           Tokens

parseTypeSpec :: Form -> TypeSpec
parseTypeSpec = undefined

parseType :: Form -> Type
parseType = undefined

parseClassSpec :: Form -> ClassSpec
parseClassSpec = undefined

parseExpr :: Form -> Expr
parseExpr = undefined

withConstraints :: (Form -> a) -> Form -> ([ClassSpec], a)
withConstraints = undefined

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
      RoundList (Symbol name : args) ->
        ( VarName name
        , flip map args $ \a -> case a of
          Symbol argName -> TypeName argName
          _ -> error $ "failed to parse data constructor argument: " ++ show a
        )
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
  parseDecl' pub (RoundList [Symbol "derive", spec]) = if pub
    then error $ "cannot make public: " ++ show form
    else Derive (parseClassSpec spec)
  parseDecl' pub (RoundList [Symbol "import", StrAtom file]) =
    if pub then error $ "cannot make public: " ++ show form else Import file
  parseDecl' pub (RoundList (Symbol "instance" : spec : members)) = if pub
    then error $ "cannot make public: " ++ show form
    else
      let (constraints, innerSpec) = withConstraints parseClassSpec spec
      in  Instance
            constraints
            innerSpec
            (flip map members $ \m -> case m of
              RoundList [Symbol name, expr] -> (VarName name, parseExpr expr)
              _ -> error $ "failed to parse instance member: " ++ show m
            )
  parseDecl' _ _ = error $ "failed to parse declaration: " ++ show form

parseModule :: [Form] -> [Decl]
parseModule = map parseDecl
