module Parser
  ( parseModule
  )
where

import           Codec.Binary.UTF8.String

import           AST
import           Tokens
import           Util

withConstraints :: (Form -> a) -> Form -> ([ClassSpec], a)
withConstraints parseBody (RoundList [Symbol "with", RoundList specs, body]) =
  (map parseClassSpec specs, parseBody body)
withConstraints _ form@(RoundList (Symbol "with" : _)) =
  error $ "failed to parse constraint list: " ++ pretty form
withConstraints parseBody form = ([], parseBody form)

parseTypeSpec :: Form -> TypeSpec
parseTypeSpec (RoundList ((Symbol name) : args)) = TypeSpec
  name
  (flip map args $ \arg -> case arg of
    Symbol argName -> argName
    _ -> error $ "failed to parse type spec argument: " ++ pretty arg
  )
parseTypeSpec (Symbol name) = TypeSpec name []
parseTypeSpec form = error $ "failed to parse type spec: " ++ pretty form

parseType :: Form -> Type
parseType form@(RoundList (Symbol "with" : _)) =
  let (specs, Type moreSpecs name args) = withConstraints parseType form
  in  Type (specs ++ moreSpecs) name args
parseType (RoundList (Symbol name : args)) = Type [] name (map parseType args)
parseType (Symbol name) = Type [] name []
parseType form = error $ "failed to parse type: " ++ pretty form

parseClassSpec :: Form -> ClassSpec
parseClassSpec (RoundList [Symbol name, Symbol typ]) = ClassSpec name typ
parseClassSpec form = error $ "failed to parse class spec: " ++ pretty form

parseExpr :: Form -> Expr
parseExpr (RoundList []) = error "round list can't be empty"
parseExpr (RoundList (Symbol "case" : expr : branches)) = Case
  (parseExpr expr)
  (flip map branches $ \br -> case br of
    RoundList [pat, res] -> (parseExpr pat, parseExpr res)
    _                    -> error $ "failed to parse case branch: " ++ pretty br
  )
parseExpr (RoundList [Symbol "if", cond, true, false]) = Case
  (parseExpr cond)
  [(Variable "True", parseExpr true), (Variable "False", parseExpr false)]
parseExpr (RoundList [Symbol "lambda", RoundList args, body]) = foldr
  Lambda
  (parseExpr body)
  (flip map args $ \arg -> case arg of
    Symbol name -> name
    _           -> error $ "failed to parse lambda argument: " ++ pretty arg
  )
parseExpr (RoundList [Symbol "let", RoundList bindings, body]) = foldr
  (uncurry Let)
  (parseExpr body)
  (flip map bindings $ \binding -> case binding of
    RoundList [Symbol name, val] -> (name, parseExpr val)
    _ -> error $ "failed to parse let binding: " ++ pretty binding
  )
parseExpr (RoundList  elts) = foldl1 Call (map parseExpr elts)
parseExpr (SquareList elts) = parseExpr $ foldr
  (\char rest -> RoundList [Symbol "Cons", char, rest])
  (Symbol "Null")
  elts
parseExpr (Symbol   name) = Variable name
parseExpr (IntAtom  i   ) = Const i
parseExpr (CharAtom c   ) = case encodeChar c of
  [b] -> parseExpr $ RoundList [Symbol "Char", IntAtom (fromIntegral b)]
  _   -> error "multibyte character literals are not supported"
parseExpr (StrAtom s) = parseExpr $ SquareList
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
            RoundList [Symbol name, typ] -> (name, parseType typ)
            _ -> error $ "failed to parse class member: " ++ pretty m
          )
  parseDecl' pub (RoundList (Symbol "data" : spec : members)) = Data
    pub
    (parseTypeSpec spec)
    (flip map members $ \m -> case m of
      Symbol name -> (name, [])
      RoundList (Symbol name : args) -> (name, map parseType args)
      _ -> error $ "failed to parse data constructor: " ++ pretty m
    )
  parseDecl' pub (RoundList [Symbol "def", Symbol name, typ, expr]) =
    Def pub name (parseType typ) (parseExpr expr)
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
            RoundList [Symbol name, expr] -> (name, parseExpr expr)
            _ -> error $ "failed to parse instance member: " ++ pretty m
          )
  parseDecl' _ _ = error $ "failed to parse declaration: " ++ pretty form

parseModule :: [Form] -> [Decl]
parseModule = map parseDecl
