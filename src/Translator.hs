module Translator
  ( translateBundle
  )
where

import           Control.Applicative     hiding ( Const )
import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import           Data.List
import qualified Data.Map.Strict               as Map
import           Prelude                 hiding ( mod )

import           AST
import           Assembly
import           Bridge
import           Subroutines

{-# ANN module "HLint: ignore Use record patterns" #-}

type Bindings = Map.Map String (Either Symbol VirtualRegister)

type TopLevelBindings = Map.Map String Symbol

data Context = Context
  { bindings :: Bindings
  , fnName :: String
  }

withBinding :: String -> VirtualRegister -> Context -> Context
withBinding name temp ctx =
  Context (Map.insert name (Right temp) (bindings ctx)) (fnName ctx)

freeVariables :: Expr -> [String]
freeVariables (Variable name) = [name]
freeVariables (Const    _   ) = []
freeVariables (Call lhs rhs ) = freeVariables lhs ++ freeVariables rhs
freeVariables (Case arg branches) =
  freeVariables arg
    ++ concatMap (\(pat, expr) -> freeVariables expr \\ freeVariables pat)
                 branches
freeVariables (Lambda arg body) = freeVariables body \\ [arg]
freeVariables (Let name val body) =
  freeVariables val ++ (freeVariables body \\ [name])

translateVar :: Context -> VirtualRegister -> String -> [VirtualInstruction]
translateVar ctx dst name = case Map.lookup name (bindings ctx) of
  Just (Left  sym) -> [JUMP CALL (symName sym), OP MOV $ RR rax dst]
  Just (Right reg) -> [OP MOV $ RR reg dst]
  Nothing          -> case Map.lookup name stdlibPublic of
    Just (internalName, _) -> [JUMP CALL internalName, OP MOV $ RR rax dst]
    Nothing -> error $ "reference to free variable: " ++ show name

translatePattern
  :: Context
  -> Label
  -> VirtualRegister
  -> Expr
  -> Stateful ([VirtualInstruction], Map.Map String VirtualRegister)
translatePattern ctx _ temp (Variable name) =
  case Map.lookup name (bindings ctx) of
    Just (Left (SymData _ numFields _)) -> case numFields of
      0 -> return ([], Map.empty)
      _ ->
        error
          $  "data constructor "
          ++ name
          ++ " used with too many fields in case pattern"
    _ -> return ([], Map.fromList [(name, temp)])
translatePattern _ nextBranch temp (Const val) = do
  imm <- newTemp
  return ([MOV64 val imm, OP CMP $ RR imm temp, JUMP JNE nextBranch], Map.empty)
translatePattern ctx nextBranch obj expr@(Call _ _) =
  let (ctor : args) = reverse $ uncurryExpr expr
  in
    case ctor of
      Variable name -> case Map.lookup name (bindings ctx) of
        Just (Left (SymData _ numFields ctorIdx)) ->
          if numFields /= length args
            then
              error
              $  "data constructor "
              ++ name
              ++ " used with "
              ++ show (length args)
              ++ " fields but needs "
              ++ show numFields
            else do
              fieldTemps <- replicateM numFields newTemp
              let extractCodes = zipWith
                    (\temp idx -> [OP MOV $ MR (getField (idx + 1) obj) temp])
                    fieldTemps
                    (iterate (+ 1) 0)
              let mainCheck =
                    [ OP CMP $ IM (fromIntegral ctorIdx) (getField 0 obj)
                    , JUMP JNE nextBranch
                    ]
              fieldChecks <- zipWithM (translatePattern ctx nextBranch)
                                      fieldTemps
                                      args
              return
                ( concat extractCodes ++ mainCheck ++ concatMap fst fieldChecks
                , foldr
                  ( Map.unionWithKey
                      (\var _ _ ->
                        error
                          $  "two bindings for "
                          ++ show var
                          ++ " in same case pattern"
                      )
                  . snd
                  )
                  Map.empty
                  fieldChecks
                )
        _ -> error $ show name ++ " is not a data constructor"
      _ -> error "malformed data list in case pattern"
 where
  uncurryExpr (Call lhs rhs) = rhs : uncurryExpr lhs
  uncurryExpr f              = [f]
translatePattern _ _ _ (Case   _ _) = error "can't use case in case pattern"
translatePattern _ _ _ (Lambda _ _) = error "can't use lambda in case pattern"
translatePattern _ _ _ (Let _ _ _ ) = error "can't use let in case pattern"

translateExpr
  :: Context
  -> VirtualRegister
  -> Expr
  -> Stateful ([VirtualInstruction], [VirtualFunction])
translateExpr ctx dst (Variable name) = return (translateVar ctx dst name, [])
translateExpr _   dst (Const    val ) = return ([MOV64 val dst], [])
translateExpr ctx dst (Call lhs rhs ) = do
  lhsTemp           <- newTemp
  rhsTemp           <- newTemp
  (lhsCode, lhsFns) <- translateExpr ctx lhsTemp lhs
  (rhsCode, rhsFns) <- translateExpr ctx rhsTemp rhs
  callCode          <- translateCall lhsTemp (Just rhsTemp)
  return
    (lhsCode ++ rhsCode ++ callCode ++ [OP MOV $ RR rax dst], lhsFns ++ rhsFns)
translateExpr ctx dst (Case arg branches) = do
  argTemp           <- newTemp
  (argCode, argFns) <- translateExpr ctx argTemp arg
  nextBranches      <- replicateM (length branches) newLabel
  branchCodes       <- zipWithM (\label -> translatePattern ctx label argTemp)
                                nextBranches
                                (map fst branches)
  endLabel  <- newLabel
  exprCodes <- zipWithM
    (\branch ctx' -> do
      (code, fns) <- translateExpr ctx' dst branch
      return (code ++ [JUMP JMP endLabel], fns)
    )
    (map snd branches)
    (map (foldr (uncurry withBinding) ctx . Map.toList . snd) branchCodes)
  return
    ( argCode
    ++ concatMap fst branchCodes
    ++ concatMap fst exprCodes
    ++ [LABEL endLabel]
    , argFns ++ concatMap snd exprCodes
    )
translateExpr ctx dst form@(Lambda name body) = do
  fnPtr      <- newTemp
  temp       <- newTemp
  lambdaName <- newLambda (fnName ctx)
  let vars = nub (freeVariables form)
  varTemps <- replicateM (length vars) newTemp
  let varCodes = zipWith (translateVar ctx) varTemps vars
  argTemps <- replicateM (length vars + 1) newTemp
  let argNames = vars ++ [name]
  let bodyCtx = foldr (uncurry withBinding) ctx (zip argNames argTemps)
  bodyDst             <- newTemp
  (bodyCode, bodyFns) <- translateExpr bodyCtx bodyDst body
  return
    ( [ PUSHI (fromIntegral $ (length vars + 2) * 8)
      , JUMP CALL "memoryAlloc"
      , unpush 1
      , OP MOV $ RR rax fnPtr
      , LEA (memLabel lambdaName) temp
      , OP MOV $ RM temp (getField 0 fnPtr)
      , OP MOV $ IM (fromIntegral $ length vars) (getField 1 fnPtr)
      ]
    ++ concat varCodes
    ++ zipWith
         (\varTemp idx -> OP MOV $ RM varTemp (getField (idx + 2) fnPtr))
         varTemps
         (iterate (+ 1) 0)
    ++ [OP MOV $ RR fnPtr dst]
    , Function lambdaName (bodyCode ++ [OP MOV $ RR bodyDst rax]) : bodyFns
    )
translateExpr ctx dst (Let name val body) = do
  temp                <- newTemp
  (letCode , letFns ) <- translateExpr ctx temp val
  (bodyCode, bodyFns) <- translateExpr (withBinding name temp ctx) dst body
  return (letCode ++ bodyCode, letFns ++ bodyFns)

-- don't handle Derive or Instance for now
translateDecl :: TopLevelBindings -> Decl -> Stateful [VirtualFunction]
translateDecl _     (Alias _ _ _   ) = return []
translateDecl _     (Class _ _ _ _ ) = return []
translateDecl binds (Data _ _ ctors) = concat <$> zipWithM
  (\(ctor, types) ctorIdx -> do
    arg <- newTemp
    let mainName =
          symName (binds Map.! ctor)
            ++ (if length types >= 2 then "__uncurried" else "")
    let mainFn = Function
          mainName
          (if length types == 1
            then [OP MOV $ MR (getArg 1) rax, RET]
            else
              [ PUSHI (fromIntegral $ (length ctor + 1) * 8)
              , JUMP CALL "memoryAlloc"
              , unpush 1
              , OP MOV $ IM ctorIdx (getField 0 rax)
              ]
              ++ concatMap
                   (\argIdx ->
                     [ OP MOV $ MR (getArg argIdx) arg
                     , OP MOV $ RM arg (getField (argIdx + 1) rax)
                     ]
                   )
                   [1 .. length types]
              ++ [RET]
          )
    return [mainFn]
  )
  ctors
  (iterate (+ 1) 0)
translateDecl binds (Def _ name _ value) = do
  dst           <- newTemp
  (instrs, fns) <- translateExpr (Context (Map.map Left binds) name) dst value
  return
    $ Function (symName $ binds Map.! name)
               (instrs ++ [OP MOV $ RR dst rax, RET])
    : fns
translateDecl _ (Derive _ _) = return []
translateDecl _ (Import _ _) = error "translator shouldn't be handling imports"
translateDecl _ (Instance _ _ _ _) = return []

translateBundle' :: Resolver -> Bundle -> Stateful (Program VirtualRegister)
translateBundle' (Resolver resolver) (Bundle mmod mmap) = do
  let isMain decl = case decl of
        Def _ name _ _ | name == "main" -> True
        _                               -> False
  let mainDecl = case filter isMain (fst $ mmap Map.! mmod) of
        []     -> error "no main function declared"
        [main] -> main
        _      -> error "more than one main function declared"
  let mmap' = Map.adjust (first (filter (not . isMain))) mmod mmap
  fns <- concat <$> mapM
    (\(mod, (decls, _)) ->
      concat <$> mapM (translateDecl (resolver Map.! mod)) decls
    )
    (Map.toList mmap')
  mainFns <- translateDecl (resolver Map.! mmod) mainDecl
  mainFn  <-
    let Function name instrs = head mainFns
    in  do
          let memInitCode = [JUMP CALL "memoryInit"]
          callCode <- translateCall rax Nothing
          let exitCode =
                [ OP MOV $ IR 60 rax
                , OP MOV $ IR 0 rdi
                , SYSCALL 1 -- exit
                ]
          return $ Function name $ memInitCode ++ instrs ++ callCode ++ exitCode
  let extraMainFns = tail mainFns
  let userFns      = extraMainFns ++ fns
  stdlib <- stdlibFns (mainFn : userFns)
  return $ Program mainFn (userFns ++ stdlib) stdlibData

translateBundle :: Resolver -> Bundle -> Program VirtualRegister
translateBundle resolver bundle =
  evalState (translateBundle' resolver bundle) 0
