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
withConstraints _ form =
  error $ "failed to parse constraint list: " ++ show form

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
parseExpr (RoundList  []  ) = error "round list can't be empty"
parseExpr (RoundList  elts) = foldl1 Call (map parseExpr elts)
parseExpr (SquareList elts) = parseExpr $ foldr
  (\rest char -> RoundList [Symbol "Cons", char, rest])
  (Symbol "Null")
  elts
parseExpr (Symbol  name) = Variable name
parseExpr (IntAtom i   ) = Const i
parseExpr (StrAtom s) =
  parseExpr $ SquareList (map (IntAtom . fromIntegral) $ encode s)

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
