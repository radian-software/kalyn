module TypeChecker
  ( typeCheckBundle
  )
where

import           Control.Monad.State
import           Data.Char
import           Data.List
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import qualified Data.Set                      as Set
import           Prelude                 hiding ( mod )

import           AST
import           Util

{-# ANN module "HLint: ignore Use lambda-case" #-}
{-# ANN module "HLint: ignore Use record patterns" #-}

type Stateful = State Int

type Bindings = Map.Map String (Either Symbol Int)

data ConsE = ConsT TypeName [ConsE]
           | ConsV Int
  deriving (Show)

type Constraints = [(ConsE, ConsE)]

newVar :: Stateful Int
newVar = do
  count <- get
  put $ count + 1
  return count

numberType :: (String -> Maybe Int) -> Type -> ConsE
numberType func ty@(Type _ typeName typeArgs) = case func typeName of
  Just var -> case typeArgs of
    [] -> ConsV var
    _ ->
      error
        $  "can't use metavariable as data constructor in type: "
        ++ pretty ty
  Nothing -> ConsT typeName (map (numberType func) typeArgs)

unnumberType :: (Int -> Maybe ConsE) -> ConsE -> ConsE
unnumberType func (ConsV var) = case func var of
  Nothing      -> ConsV var
  Just newCons -> newCons
unnumberType func (ConsT typeName typeArgs) =
  ConsT typeName (map (unnumberType func) typeArgs)

getTypeParams :: Type -> [TypeName]
getTypeParams =
  filter
      (\typeName -> case typeName of
        (c : _) | isLower c -> True
        _                   -> False
      )
    . getTypeParams'
 where
  getTypeParams' (Type _ typeName typeArgs) =
    typeName : concatMap getTypeParams' typeArgs

autoNumberType :: Type -> Stateful (ConsE, Map.Map TypeName Int)
autoNumberType ty = do
  let params = nub . getTypeParams $ ty
  vars <- replicateM (length params) newVar
  let paramMap = Map.fromList $ zip params vars
  return (numberType (`Map.lookup` paramMap) ty, paramMap)

analyzePattern
  :: Bindings -> Int -> Expr -> Stateful (Constraints, Map.Map VarName Int)
analyzePattern _ var (Variable name) = return ([], Map.singleton name var)
analyzePattern _ var (Const _) =
  return ([(ConsV var, ConsT "Int" [])], Map.empty)
analyzePattern ctx var expr@(Call _ _) =
  let (ctor : args) = reverse $ uncurryExpr expr
  in
    case ctor of
      Variable ctorName -> case ctorName `Map.lookup` ctx of
        Just (Left sd@(SymData _ _ _ _ _ _ _)) ->
          if sdNumFields sd /= length args
            then
              error
              $  "data constructor "
              ++ ctorName
              ++ " used with "
              ++ show (length args)
              ++ " field"
              ++ (if length args == 1 then "" else "s")
              ++ " but needs "
              ++ show (sdNumFields sd)
            else do
              fieldVars <- replicateM (sdNumFields sd) newVar
              let TypeSpec origName origParams = sdTypeSpec sd
              paramVars <- replicateM (length origParams) newVar
              let paramMap = Map.fromList $ zip origParams paramVars
              let fieldConsTypes =
                    map (numberType (`Map.lookup` paramMap)) (sdTypes sd)
              (fieldConses, fieldCtxs) <- mapAndUnzipM
                (uncurry $ analyzePattern ctx)
                (zip fieldVars args)
              let patternCons =
                    (ConsV var, ConsT origName fieldConsTypes)
                      : zipWith
                          (\fieldVar fieldConsType ->
                            (ConsV fieldVar, fieldConsType)
                          )
                          fieldVars
                          fieldConsTypes
              return
                ( patternCons ++ concat fieldConses
                , mapUnionsWithKey
                  (\name _ _ ->
                    error
                      $  "more than one binding for "
                      ++ show name
                      ++ " in case pattern"
                  )
                  fieldCtxs
                )
        _ -> error $ show ctorName ++ " is not a data constructor"
      _ -> error "malformed data list in case pattern"
 where
  uncurryExpr (Call lhs rhs) = rhs : uncurryExpr lhs
  uncurryExpr f              = [f]
analyzePattern _   _   (Case   _ _ ) = error "can't use case in case pattern"
analyzePattern _   _   (Lambda _ _ ) = error "can't use lambdax in case pattern"
analyzePattern _   _   (Let _ _ _  ) = error "can't use let in case pattern"
analyzePattern ctx var (As name pat) = do
  (cons, binds) <- analyzePattern ctx var pat
  return
    ( cons
    , Map.insertWith
      (\_ _ ->
        error $ "two bindings for " ++ show name ++ " in same case pattern"
      )
      name
      var
      binds
    )

analyzeExpr :: Bindings -> Int -> Expr -> Stateful Constraints
analyzeExpr ctx var (Variable name) = case name `Map.lookup` ctx of
  Nothing -> error $ "type checker found free variable " ++ show name
  Just (Left (SymDef _ ty)) -> do
    (labeledType, _) <- autoNumberType ty
    return [(ConsV var, labeledType)]
  Just (Left sd@(SymData _ _ _ _ _ _ _)) -> do
    let TypeSpec typeName params = sdTypeSpec sd
    let ctorType = foldr
          (\lhs rhs -> Type [] "Func" [lhs, rhs])
          (Type [] typeName (map (\param -> Type [] param []) params))
          (sdTypes sd)
    (labeledType, _) <- autoNumberType ctorType
    return [(ConsV var, labeledType)]
  Just (Right existingVar) -> return [(ConsV var, ConsV existingVar)]
analyzeExpr _   var (Const _     ) = return [(ConsV var, ConsT "Int" [])]
analyzeExpr ctx var (Call lhs rhs) = do
  lhsVar  <- newVar
  rhsVar  <- newVar
  lhsCons <- analyzeExpr ctx lhsVar lhs
  rhsCons <- analyzeExpr ctx rhsVar rhs
  return
    $  (ConsV lhsVar, ConsT "Func" [ConsV rhsVar, ConsV var])
    :  lhsCons
    ++ rhsCons
analyzeExpr ctx var (Case expr branches) = do
  exprVar                     <- newVar
  exprCons                    <- analyzeExpr ctx exprVar expr
  (branchConses, branchBinds) <- mapAndUnzipM (analyzePattern ctx exprVar)
                                              (map fst branches)
  resultConses <- zipWithM
    (\binds result ->
      let
        ctx' = foldr (\(name, rv) -> Map.insert name (Right rv))
                     ctx
                     (Map.toList binds)
      in  analyzeExpr ctx' var result
    )
    branchBinds
    (map snd branches)
  return $ exprCons ++ concat branchConses ++ concat resultConses
analyzeExpr ctx var (Lambda arg body) = do
  argVar   <- newVar
  bodyVar  <- newVar
  bodyCons <- analyzeExpr (Map.insert arg (Right argVar) ctx) bodyVar body
  return $ (ConsV var, ConsT "Func" [ConsV argVar, ConsV bodyVar]) : bodyCons
analyzeExpr ctx var (Let name val body) = do
  nameVar <- newVar
  bodyVar <- newVar
  let ctx' = Map.insert name (Right nameVar) ctx
  valCons  <- analyzeExpr ctx' nameVar val
  bodyCons <- analyzeExpr ctx' bodyVar body
  return $ (ConsV var, ConsV bodyVar) : valCons ++ bodyCons
analyzeExpr ctx var (As _ expr) = analyzeExpr ctx var expr

expand :: ModAliasResolver -> ConsE -> Stateful ConsE
expand _        (ConsV var      ) = return $ ConsV var
expand resolver (ConsT name args) = case name `Map.lookup` resolver of
  Nothing             -> ConsT name <$> mapM (expand resolver) args
  Just (params, defn) -> if length args < length params
    then
      error
      $  "not enough arguments given for type alias "
      ++ show name
      ++ " ("
      ++ show (length args)
      ++ " given, "
      ++ show (length params)
      ++ " needed)"
    else do
      (defnCons, mapping) <- autoNumberType defn
      let (appliedArgs, remainingArgs) = splitAt (length params) args
      let paramMap =
            Map.fromList (zip (map (mapping Map.!) params) appliedArgs)
      let (ConsT typeName typeArgs) =
            unnumberType (`Map.lookup` paramMap) defnCons
      return $ ConsT typeName (typeArgs ++ remainingArgs)

unify
  :: VarName
  -> Set.Set Int
  -> Map.Map Int ConsE
  -> ConsE
  -> ConsE
  -> Stateful (Map.Map Int ConsE)
unify name _ mappings (ConsT t1 args1) (ConsT t2 args2)
  | t1 /= t2
  = error
    $  "in function "
    ++ show name
    ++ ": can't unify "
    ++ t1
    ++ " and "
    ++ t2
  | length args1 /= length args2
  = error
    $  "in function "
    ++ show name
    ++ ": can't unify instances of "
    ++ t1
    ++ " with "
    ++ show (length args1)
    ++ " and "
    ++ show (length args2)
    ++ " arguments"
  | otherwise
  = foldr (\(arg1, arg2) mm -> mm >>= \m -> unify name Set.empty m arg1 arg2)
          (return mappings)
          (zip args1 args2)
unify name _ mappings c1@(ConsT _ _) c2@(ConsV _) =
  unify name Set.empty mappings c2 c1
unify name seen mappings (ConsV var) rhs = case var `Map.lookup` mappings of
  Nothing -> return . Map.insert var rhs $ mappings
  Just (ConsV existing)
    | existing `Set.member` seen -> return . Map.insert existing rhs $ mappings
    | otherwise -> unify name
                         (Set.insert var seen)
                         mappings
                         (ConsV existing)
                         rhs
  Just existing@(ConsT _ _) -> unify name Set.empty mappings existing rhs

solveConstraints :: VarName -> Constraints -> ()
solveConstraints name constraints =
  foldM (uncurry . unify name Set.empty) Map.empty (reverse constraints)
    `seq` ()

typeCheckDecl :: ModResolver -> Decl -> ()
typeCheckDecl resolver (Def _ name _ expr) =
  let constraints = flip evalState 0 $ do
        tlVar <- newVar
        let SymDef _ ty = fst resolver Map.! name
        (tlConsType, _) <- autoNumberType ty
        let tlCons = (ConsV tlVar, tlConsType)
        exprConses <- analyzeExpr (Map.map Left (fst resolver)) tlVar expr
        let expander = expand (snd resolver)
        mapM (\(lhs, rhs) -> (,) <$> expander lhs <*> expander rhs)
          $ tlCons
          : exprConses
  in  solveConstraints name constraints
typeCheckDecl _ _ = ()

typeCheckBundle :: Resolver -> Bundle -> ()
typeCheckBundle (Resolver resolver) (Bundle _ mmap) =
  Map.mapWithKey
      (\mod (decls, deps) ->
        (map (typeCheckDecl (resolver Map.! mod)) decls, deps)
      )
      mmap
    `seq` ()
