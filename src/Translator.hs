module Translator
  ( translateBundle
  )
where

import           Control.Applicative     hiding ( Const )
import           Control.Monad
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import qualified Data.Set                      as Set
import           Prelude                 hiding ( mod )

import           AST
import           Assembly
import           Bridge
import           Subroutines
import           Util

{-# ANN module "HLint: ignore Use record patterns" #-}

type Bindings = Map.Map String (Either Symbol VirtualRegister)

type TopLevelBindings = Map.Map String Symbol

data Context = Context
  { bindings :: Bindings
  , fnName :: String
  , sublambdaCount :: Int
  , sublambdasLeft :: Int
  }

withBinding :: String -> VirtualRegister -> Context -> Context
withBinding name temp ctx = Context
  (Map.insert name (Right temp) (bindings ctx))
  (fnName ctx)
  (sublambdaCount ctx)
  (sublambdasLeft ctx)

lessOneSublambda :: Context -> Context
lessOneSublambda ctx = Context (bindings ctx)
                               (fnName ctx)
                               (sublambdaCount ctx)
                               (sublambdasLeft ctx - 1)

freeVariables :: Expr -> Set.Set String
freeVariables (Variable name) = Set.singleton name
freeVariables (Const    _   ) = Set.empty
freeVariables (Call lhs rhs ) = freeVariables lhs `Set.union` freeVariables rhs
freeVariables (Case arg branches) =
  Set.unions
    $ freeVariables arg
    : map (\(pat, expr) -> freeVariables expr Set.\\ freeVariables pat) branches
freeVariables (Lambda arg body) = Set.delete arg (freeVariables body)
freeVariables (Let name val body) =
  freeVariables val `Set.union` Set.delete name (freeVariables body)
freeVariables (As name expr) = Set.delete name (freeVariables expr)

translateVar :: Context -> VirtualRegister -> String -> [VirtualInstruction]
translateVar ctx dst name = case Map.lookup name (bindings ctx) of
  Just (Left  sym) -> [JUMP CALL (symName sym), OP MOV $ RR rax dst]
  Just (Right reg) -> [OP MOV $ RR reg dst]
  Nothing          -> error $ "reference to free variable: " ++ show name

translatePattern
  :: Context
  -> Label
  -> VirtualRegister
  -> Expr
  -> Stateful ([VirtualInstruction], Map.Map String VirtualRegister)
translatePattern ctx nextBranch temp (Variable name) =
  case Map.lookup name (bindings ctx) of
    Just (Left sd@(SymData _ _ _ _ _ _ _)) -> case sdNumFields sd of
      0 -> case sdNumCtors sd of
        0 -> error "somehow a nonexistent data constructor has appeared"
        1 -> return ([], Map.empty)
        _ -> return
          ( [ OP CMP
              $ (if sdBoxed sd then flip IM $ deref temp else flip IR temp)
                  (fromIntegral $ sdCtorIdx sd)
            , JUMP JNE nextBranch
            ]
          , Map.empty
          )
      _ ->
        error
          $  "data constructor "
          ++ name
          ++ " used with no fields in case pattern (needs "
          ++ show (sdNumFields sd)
          ++ ")"
    _ -> return ([], Map.fromList [(name, temp)])
translatePattern _ nextBranch temp (Const val) = do
  imm <- newTemp
  return ([MOV64 val imm, OP CMP $ RR imm temp, JUMP JNE nextBranch], Map.empty)
translatePattern ctx nextBranch obj expr@(Call _ _) =
  let (ctor : args) = reverse $ uncurryExpr expr
  in
    case ctor of
      Variable name -> case Map.lookup name (bindings ctx) of
        Just (Left sd@(SymData _ _ _ _ _ _ _)) ->
          if sdNumFields sd /= length args
            then
              error
              $  "data constructor "
              ++ name
              ++ " used with "
              ++ show (length args)
              ++ " field"
              ++ (if length args == 1 then "" else "s")
              ++ " but needs "
              ++ show (sdNumFields sd)
            else do
              fieldTemps <- replicateM (sdNumFields sd) newTemp
              let
                extractCode = if sdBoxed sd
                  then concat $ zipWith
                    (\temp idx ->
                      [ OP MOV $ MR
                          (getField (idx + (if sdHasHeader sd then 1 else 0))
                                    obj
                          )
                          temp
                      ]
                    )
                    fieldTemps
                    (iterate (+ 1) 0)
                  -- only one fieldTemp in this case
                  else [OP MOV $ RR obj (head fieldTemps)]
              let mainCheck = if sdHasHeader sd
                    then
                      [ OP CMP
                        $ IM (fromIntegral $ sdCtorIdx sd) (getField 0 obj)
                      , JUMP JNE nextBranch
                      ]
                    else []
              fieldChecks <- zipWithM (translatePattern ctx nextBranch)
                                      fieldTemps
                                      args
              return
                ( extractCode ++ mainCheck ++ concatMap fst fieldChecks
                , foldr
                  ( Map.unionWithKey
                      (\var k1 _ -> if var == "_"
                        then k1
                        else
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
translatePattern _ _ _ (Case _ _) = error "can't use case in case pattern"
translatePattern _ _ _ (Lambda _ _) = error "can't use lambda in case pattern"
translatePattern _ _ _ (Let _ _ _) = error "can't use let in case pattern"
translatePattern ctx nextBranch temp (As name pat) = do
  (instrs, binds) <- translatePattern ctx nextBranch temp pat
  return
    ( instrs
    , Map.unionWithKey
      (\var _ _ ->
        error $ "two bindings for " ++ show var ++ " in same case pattern"
      )
      binds
      (Map.fromList [(name, temp)])
    )

translateIndirectCall
  :: Context
  -> VirtualRegister
  -> Expr
  -> Expr
  -> Stateful ([VirtualInstruction], [VirtualFunction])
translateIndirectCall ctx dst lhs rhs = do
  lhsTemp           <- newTemp
  rhsTemp           <- newTemp
  (lhsCode, lhsFns) <- translateExpr ctx lhsTemp lhs
  (rhsCode, rhsFns) <- translateExpr ctx rhsTemp rhs
  callCode          <- translateCall lhsTemp (Just rhsTemp)
  return
    (lhsCode ++ rhsCode ++ callCode ++ [OP MOV $ RR rax dst], lhsFns ++ rhsFns)

translateExpr
  :: Context
  -> VirtualRegister
  -> Expr
  -> Stateful ([VirtualInstruction], [VirtualFunction])
translateExpr ctx dst (Variable name) = return (translateVar ctx dst name, [])
translateExpr _   dst (Const    val ) = return ([MOV64 val dst], [])
translateExpr ctx dst expr@(Call lhs rhs) =
  let (lhs', args) = reverse <$> uncurryArgs expr
  in
    case lhs' of
      Variable name -> case Map.lookup name (bindings ctx) of
        Just (Left (SymDef mangledName _ numSublambdas)) -> do
          let (directArgs, indirectArgs) = splitAt numSublambdas args
          let directName
                | null directArgs
                = mangledName
                | length directArgs == numSublambdas
                = mangledName ++ "__uncurried"
                | otherwise
                = mangledName ++ "__curried" ++ show (length directArgs - 1)
          directArgsTemps <- replicateM (length directArgs) newTemp
          (directArgsCode, directArgsFns) <-
            unzip
              <$> zipWithM
                    (\arg temp -> do
                      (evalCode, evalFns) <- translateExpr ctx temp arg
                      return (evalCode ++ [UN PUSH (R temp)], evalFns)
                    )
                    directArgs
                    directArgsTemps
          directCallTemp <- newTemp
          let directCallCode =
                [JUMP CALL directName]
                  ++ [ unpush (fromIntegral $ length directArgs)
                     | not . null $ directArgs
                     ]
                  ++ [OP MOV $ RR rax directCallTemp]
          let indirectCallExpr = foldl' Call (Variable "gensym") indirectArgs
          (indirectCallCode, indirectCallFns) <- translateExpr
            (withBinding "gensym" directCallTemp ctx)
            dst
            indirectCallExpr
          return
            ( concat directArgsCode ++ directCallCode ++ indirectCallCode
            , concat directArgsFns ++ indirectCallFns
            )
        Just (Left (SymData _ _ _ _ _ _ _)) ->
          translateIndirectCall ctx dst lhs rhs -- FIXME
        _ -> translateIndirectCall ctx dst lhs rhs
      _ -> translateIndirectCall ctx dst lhs rhs
 where
  uncurryArgs (Call lhs' rhs') =
    let (lhs'', args) = uncurryArgs lhs' in (lhs'', rhs' : args)
  uncurryArgs lhs' = (lhs', [])
translateExpr ctx dst (Case arg branches) = do
  argTemp           <- newTemp
  (argCode, argFns) <- translateExpr ctx argTemp arg
  endLabel          <- newLabel
  nextBranches      <- replicateM (length branches) newLabel
  branchCodes       <- zipWithM (\label -> translatePattern ctx label argTemp)
                                nextBranches
                                (map fst branches)
  exprCodes <- zipWithM
    (\branch ctx' -> translateExpr ctx' dst branch)
    (map snd branches)
    (map (foldr (uncurry withBinding) ctx . Map.toList . snd) branchCodes)
  msg <- newTemp
  return
    ( argCode
    ++ concat
         (zipWith3
           (\(branchCode, _) (exprCode, _) nextBranch ->
             branchCode ++ exprCode ++ [JUMP JMP endLabel, LABEL nextBranch]
           )
           branchCodes
           exprCodes
           nextBranches
         )
    ++ [ LEA (memLabel "msgPatternMatchFailed") msg
       , UN PUSH $ R msg
       , JUMP CALL "crash"
       , LABEL endLabel
       ]
    , argFns ++ concatMap snd exprCodes
    )
translateExpr ctx dst form@(Lambda name body) = do
  temp <- newTemp
  let possibleVars = Set.toList . freeVariables $ form
  let vars = mapMaybe
        (\var -> case (var, Map.lookup var (bindings ctx)) of
          (_, Just (Right reg)) | var /= name -> Just (var, reg)
          _ -> Nothing
        )
        possibleVars
  let argNames = map fst vars ++ [name]
  lambdaName <- if sublambdasLeft ctx <= 0
    then (++ "__" ++ intercalate "_" (map sanitize argNames))
      <$> newLambda (fnName ctx)
    else if sublambdasLeft ctx == 1
      then return (fnName ctx ++ "__uncurried")
      else return
        (fnName ctx ++ "__curried" ++ show
          (sublambdaCount ctx - sublambdasLeft ctx)
        )
  argTemps <- replicateM (length vars + 1) newTemp
  let bodyCtx = foldr (uncurry withBinding)
                      (lessOneSublambda ctx)
                      (zip argNames argTemps)
  let argsCode = zipWith
        (\argTemp argIdx -> OP MOV $ MR (getArg argIdx) argTemp)
        argTemps
        (iterate (\i -> i - 1) (length argNames))
  bodyDst             <- newTemp
  (bodyCode, bodyFns) <- translateExpr bodyCtx bodyDst body
  -- work directly on dst because we want to allow for recursive
  -- lambda let-bindings
  return
    ( [ PUSHI (fromIntegral $ (length vars + 2) * 8)
      , JUMP CALL "memoryAlloc"
      , unpush 1
      , OP MOV $ RR rax dst
      , LEA (memLabel lambdaName) temp
      , OP MOV $ RM temp (getField 0 dst)
      , OP MOV $ IM (fromIntegral $ length vars) (getField 1 dst)
      ]
      ++ zipWith
           (\varTemp idx -> OP MOV $ RM varTemp (getField (idx + 2) dst))
           (map snd vars)
           (iterate (+ 1) 0)
    , function lambdaName
               (argsCode ++ bodyCode ++ [OP MOV $ RR bodyDst rax, RET])
      : bodyFns
    )
translateExpr ctx dst (Let name val body) = do
  temp <- newTemp
  let ctx' = withBinding name temp ctx
  (letCode , letFns ) <- translateExpr ctx' temp val
  (bodyCode, bodyFns) <- translateExpr ctx' dst body
  return (letCode ++ bodyCode, letFns ++ bodyFns)
translateExpr ctx dst (As _ expr) = translateExpr ctx dst expr

-- don't handle Derive or Instance for now
translateDecl :: TopLevelBindings -> Decl -> Stateful [VirtualFunction]
translateDecl _     (Alias _ _ _          ) = return []
translateDecl _     (Class _ _ _ _        ) = return []
translateDecl binds (Data _ typeSpec ctors) = concat <$> zipWithM
  (\(ctor, types) ctorIdx -> do
    arg <- newTemp
    let baseName = symName (binds Map.! ctor)
    let mainName = if null types then baseName else baseName ++ "__uncurried"
    let
      sd = SymData { sdName      = baseName
                   , sdCtorIdx   = ctorIdx
                   , sdNumFields = length types
                   , sdNumCtors  = length ctors
                   , sdBoxed     = shouldBox ctors
                   , sdTypeSpec  = typeSpec
                   , sdTypes     = types
                   }
      mainFn = function
        mainName
        (if not . sdBoxed $ sd
          then if sdHasHeader sd
            then [OP MOV $ IR (fromIntegral ctorIdx) rax, RET]
            else if null types
              then [OP MOV $ IR 0 rax, RET]
              else [OP MOV $ MR (getArg 1) rax, RET]
          else
            [ PUSHI (fromIntegral $ (length types + 1) * 8)
            , JUMP CALL "memoryAlloc"
            , unpush 1
            ]
            ++ [ OP MOV $ IM (fromIntegral ctorIdx) (getField 0 rax)
               | sdHasHeader sd
               ]
            ++ concatMap
                 (\argIdx ->
                   [ OP MOV $ MR (getArg $ length types - argIdx) arg
                   , OP MOV $ RM
                     arg
                     (getField (argIdx + (if sdHasHeader sd then 1 else 0)) rax)
                   ]
                 )
                 [0 .. length types - 1]
            ++ [RET]
        )
    extraFns <- if null types
      then return []
      else curryify (length types) baseName
    return $ mainFn : extraFns
  )
  ctors
  (iterate (+ 1) 0)
translateDecl binds (Def _ name _ value) = do
  let SymDef mangledName _ numSublambdas = binds Map.! name
  dst           <- newTemp
  (instrs, fns) <- translateExpr
    (Context (Map.map Left binds) mangledName numSublambdas numSublambdas)
    dst
    value
  return $ function mangledName (instrs ++ [OP MOV $ RR dst rax, RET]) : fns
translateDecl _ (Derive _ _) = return []
translateDecl _ (Import _ _) = error "translator shouldn't be handling imports"
translateDecl _ (Instance _ _ _ _) = return []

translateBundle' :: Resolver -> Bundle -> Stateful (Program VirtualRegister)
translateBundle' (Resolver resolver) (Bundle mmod mmap) = do
  let mainName = symName $ fst (resolver Map.! mmod) Map.! "main"
  fns <- concat <$> mapM
    (\(mod, (decls, _)) ->
      concat <$> mapM (translateDecl $ fst (resolver Map.! mod)) decls
    )
    (Map.toList mmap)
  mainFn <- do
    fnPtr <- newTemp
    let setupCode =
          [JUMP CALL "memoryInit", JUMP CALL mainName, OP MOV $ RR rax fnPtr]
    callCode <- translateCall fnPtr Nothing
    let teardownCode =
          [ OP MOV $ IR 60 rax
          , OP MOV $ IR 0 rdi
          , SYSCALL 1 -- exit
          ]
    return $ function "main" $ setupCode ++ callCode ++ teardownCode
  stdlib <- stdlibFns fns
  return $ Program mainFn (fns ++ stdlib) stdlibData

translateBundle :: Resolver -> Bundle -> Program VirtualRegister
translateBundle resolver bundle =
  evalState (translateBundle' resolver bundle) 0
