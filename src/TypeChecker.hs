module TypeChecker
  ( typeCheckBundle
  ) where

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

numberType :: VarName -> (String -> Maybe Int) -> Type -> ConsE
numberType funcName func ty@(Type _ typeName typeArgs) = case func typeName of
  Just var -> case typeArgs of
    [] -> ConsV var
    _ ->
      error
        $  "in function "
        ++ show funcName
        ++ ": can't use metavariable as data constructor in type: "
        ++ pretty ty
  Nothing -> ConsT typeName (map (numberType funcName func) typeArgs)

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

autoNumberType :: VarName -> Type -> Stateful (ConsE, Map.Map TypeName Int)
autoNumberType fnName ty = do
  let params = nub . getTypeParams $ ty
  vars <- replicateM (length params) newVar
  let paramMap = Map.fromList $ zip params vars
  return (numberType fnName (`Map.lookup` paramMap) ty, paramMap)

analyzePattern
  :: VarName
  -> Bindings
  -> Int
  -> Expr
  -> Stateful (Constraints, Map.Map VarName Int)
analyzePattern fnName ctx var (Variable name) = case name `Map.lookup` ctx of
  Just (Left sd@(SymData _ _ _ _ _ _ _)) -> if sdNumFields sd /= 0
    then
      error
      $  "in function "
      ++ show fnName
      ++ ": data constructor "
      ++ name
      ++ " used with no fields but needs "
      ++ show (sdNumFields sd)
    else do
      let TypeSpec origName origParams = sdTypeSpec sd
      paramVars <- replicateM (length origParams) newVar
      return ([(ConsV var, ConsT origName (map ConsV paramVars))], Map.empty)
  _ -> return ([], Map.singleton name var)
analyzePattern _ _ var (Const _) =
  return ([(ConsV var, ConsT "Int" [])], Map.empty)
analyzePattern fnName ctx var expr@(Call _ _) =
  let (ctor : args) = reverse $ uncurryExpr expr
  in
    case ctor of
      Variable ctorName -> case ctorName `Map.lookup` ctx of
        Just (Left sd@(SymData _ _ _ _ _ _ _)) ->
          if sdNumFields sd /= length args
            then
              error
              $  "in function "
              ++ show fnName
              ++ ": data constructor "
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
              let fieldConsTypes = map
                    (numberType fnName (`Map.lookup` paramMap))
                    (sdTypes sd)
              (fieldConses, fieldCtxs) <- mapAndUnzipM
                (uncurry $ analyzePattern fnName ctx)
                (zip fieldVars args)
              let patternCons =
                    (ConsV var, ConsT origName (map ConsV paramVars))
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
        _ ->
          error
            $  "in function "
            ++ show fnName
            ++ ": "
            ++ show ctorName
            ++ " is not a data constructor"
      _ -> error "malformed data list in case pattern"
 where
  uncurryExpr (Call lhs rhs) = rhs : uncurryExpr lhs
  uncurryExpr f              = [f]
analyzePattern _ _ _ (Case _ _) = error "can't use case in case pattern"
analyzePattern _ _ _ (Lambda _ _) = error "can't use lambdax in case pattern"
analyzePattern _ _ _ (Let _ _ _) = error "can't use let in case pattern"
analyzePattern fnName ctx var (As name pat) = do
  (cons, binds) <- analyzePattern fnName ctx var pat
  return
    ( cons
    , Map.insertWith
      (\_ _ ->
        error
          $  "in function "
          ++ show fnName
          ++ ": two bindings for "
          ++ show name
          ++ " in same case pattern"
      )
      name
      var
      binds
    )

analyzeExpr :: VarName -> Bindings -> Int -> Expr -> Stateful Constraints
analyzeExpr fnName ctx var (Variable name) = case name `Map.lookup` ctx of
  Nothing ->
    error
      $  "in function "
      ++ show fnName
      ++ ": type checker found free variable "
      ++ show name
  Just (Left (SymDef _ ty _)) -> do
    (labeledType, _) <- autoNumberType fnName ty
    return [(ConsV var, labeledType)]
  Just (Left sd@(SymData _ _ _ _ _ _ _)) -> do
    let TypeSpec typeName params = sdTypeSpec sd
    let ctorType = foldr
          (\lhs rhs -> Type [] "Func" [lhs, rhs])
          (Type [] typeName (map (\param -> Type [] param []) params))
          (sdTypes sd)
    (labeledType, _) <- autoNumberType fnName ctorType
    return [(ConsV var, labeledType)]
  Just (Right existingVar) -> return [(ConsV var, ConsV existingVar)]
analyzeExpr _      _   var (Const _     ) = return [(ConsV var, ConsT "Int" [])]
analyzeExpr fnName ctx var (Call lhs rhs) = do
  lhsVar  <- newVar
  rhsVar  <- newVar
  lhsCons <- analyzeExpr fnName ctx lhsVar lhs
  rhsCons <- analyzeExpr fnName ctx rhsVar rhs
  return
    $  (ConsV lhsVar, ConsT "Func" [ConsV rhsVar, ConsV var])
    :  lhsCons
    ++ rhsCons
analyzeExpr fnName ctx var (Case expr branches) = do
  exprVar                     <- newVar
  exprCons                    <- analyzeExpr fnName ctx exprVar expr
  (branchConses, branchBinds) <- mapAndUnzipM
    (analyzePattern fnName ctx exprVar)
    (map fst branches)
  resultConses <- zipWithM
    (\binds result ->
      let
        ctx' = foldr (\(name, rv) -> Map.insert name (Right rv))
                     ctx
                     (Map.toList binds)
      in  analyzeExpr fnName ctx' var result
    )
    branchBinds
    (map snd branches)
  return $ exprCons ++ concat branchConses ++ concat resultConses
analyzeExpr fnName ctx var (Lambda arg body) = do
  argVar   <- newVar
  bodyVar  <- newVar
  bodyCons <- analyzeExpr fnName
                          (Map.insert arg (Right argVar) ctx)
                          bodyVar
                          body
  return $ (ConsV var, ConsT "Func" [ConsV argVar, ConsV bodyVar]) : bodyCons
analyzeExpr fnName ctx var (Let name val body) = do
  nameVar <- newVar
  bodyVar <- newVar
  let ctx' = Map.insert name (Right nameVar) ctx
  valCons  <- analyzeExpr fnName ctx' nameVar val
  bodyCons <- analyzeExpr fnName ctx' bodyVar body
  return $ (ConsV var, ConsV bodyVar) : valCons ++ bodyCons
analyzeExpr fnName ctx var (As _ expr) = analyzeExpr fnName ctx var expr

expand :: VarName -> ModAliasResolver -> ConsE -> Stateful ConsE
expand _      _        (ConsV var      ) = return $ ConsV var
expand fnName resolver (ConsT name args) = case name `Map.lookup` resolver of
  Nothing             -> ConsT name <$> mapM (expand fnName resolver) args
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
      (defnCons, mapping) <- autoNumberType fnName defn
      let (appliedArgs, remainingArgs) = splitAt (length params) args
      let paramMap =
            Map.fromList (zip (map (mapping Map.!) params) appliedArgs)
      let (ConsT typeName typeArgs) =
            unnumberType (`Map.lookup` paramMap) defnCons
      expand fnName resolver . ConsT typeName $ typeArgs ++ remainingArgs

unify
  :: VarName
  -> Set.Set Int
  -> Set.Set Int
  -> Map.Map Int ConsE
  -> ConsE
  -> ConsE
  -> Stateful (Map.Map Int ConsE)
unify _ _ _ mappings (ConsV v1) (ConsV v2) | v1 == v2 = return mappings
unify name fixed _ mappings (ConsT t1 args1) (ConsT t2 args2)
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
  = foldr
    (\(arg1, arg2) mm -> mm >>= \m -> unify name fixed Set.empty m arg1 arg2)
    (return mappings)
    (zip args1 args2)
unify name fixed seen mappings c1@(ConsT _ _) c2@(ConsV _) =
  unify name fixed seen mappings c2 c1
unify name fixed seen mappings (ConsV var) rhs = case rhs of
  (ConsT _ _) | var `Set.member` fixed ->
    error
      $  "in function "
      ++ show name
      ++ ": can't unify free type parameter "
      ++ show var
      ++ " with "
      ++ show rhs
  (ConsV other)
    | var /= other && var `Set.member` fixed && other `Set.member` fixed
    -> error
      $  "in function "
      ++ show name
      ++ ": can't unify distinct free type parameters"
  _ -> case var `Map.lookup` mappings of
    Nothing -> return . Map.insert var rhs $ mappings
    Just (ConsV existing)
      | existing `Set.member` seen -> return
      .  Map.insert existing rhs
      $  mappings
      | otherwise -> unify name
                           fixed
                           (Set.insert var seen)
                           mappings
                           (ConsV existing)
                           rhs
    Just existing@(ConsT _ _) ->
      unify name fixed Set.empty mappings existing rhs

solveConstraints :: VarName -> Set.Set Int -> Constraints -> Map.Map Int ConsE
solveConstraints name fixed =
  flip evalState 0
    . foldM (uncurry . unify name fixed Set.empty) Map.empty
    . reverse

collectTypes
  :: Map.Map Int ConsE -> Set.Set Int -> Bool -> ConsE -> Set.Set Int
collectTypes mappings seen topLevel (ConsV var) = if Set.member var seen
  then Set.empty
  else
    let more = case var `Map.lookup` mappings of
          Nothing -> Set.empty
          Just cons ->
            collectTypes mappings (Set.insert var seen) topLevel cons
    in  if topLevel then more else Set.insert var more
collectTypes mappings seen topLevel (ConsT _ args) =
  Set.unions
    . map (collectTypes mappings (if topLevel then Set.empty else seen) False)
    $ args

checkNoInfiniteTypes :: VarName -> Map.Map Int ConsE -> ()
checkNoInfiniteTypes name mappings =
  foldr
      ( seq
      . (\var ->
          if Set.member var
             . collectTypes mappings Set.empty True
             . ConsV
             $ var
          then
            error $ "in function " ++ show name ++ ": infinite type"
          else
            ()
        )
      )
      ()
    . Map.keys
    $ mappings

typeCheckDecl :: ModResolver -> Decl -> ()
typeCheckDecl resolver (Def _ name _ expr) =
  let
    mappings = flip evalState 0 $ do
      tlVar <- newVar
      let SymDef _ ty _ = fst resolver Map.! name
      (tlConsType, mapping) <- autoNumberType name ty
      let fixed  = Set.fromList . Map.elems $ mapping
      let tlCons = (ConsV tlVar, tlConsType)
      exprConses <- analyzeExpr name (Map.map Left (fst resolver)) tlVar expr
      let expander = expand name (snd resolver)
      constraints <-
        mapM (\(lhs, rhs) -> (,) <$> expander lhs <*> expander rhs)
        $ tlCons
        : exprConses
      return . solveConstraints name fixed $ constraints
  in  checkNoInfiniteTypes name mappings `seq` ()
typeCheckDecl _ _ = ()

typeCheckBundle :: Resolver -> Bundle -> ()
typeCheckBundle (Resolver resolver) (Bundle _ mmap) =
  foldr
      ( seq
      . (\(mod, (decls, _)) ->
          foldr (seq . typeCheckDecl (resolver Map.! mod)) () decls
        )
      )
      ()
    . Map.toList
    $ mmap
