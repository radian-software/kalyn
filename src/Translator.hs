module Translator where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict               as Map

import           AST
import           Assembly
import           Subroutines

data Context = Context
  { bindings :: Map.Map String (Either Symbol VirtualRegister)
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

translateVar
  :: Context -> VirtualRegister -> String -> [Instruction VirtualRegister]
translateVar ctx dst name = case Map.lookup name (bindings ctx) of
  Just (Left  sym) -> [CALL (symName sym), OP MOV $ RR rax dst]
  Just (Right reg) -> [OP MOV $ RR reg dst]
  Nothing          -> error $ "reference to free variable: " ++ show name

translateExpr
  :: Context
  -> VirtualRegister
  -> Expr
  -> Stateful
       ([Instruction VirtualRegister], [Function VirtualRegister])
translateExpr ctx dst (Variable name) = return (translateVar ctx dst name, [])
translateExpr _   dst (Const    val ) = return ([MOV64 val dst], [])
translateExpr ctx dst (Call lhs rhs ) = do
  lhsTemp           <- newTemp
  rhsTemp           <- newTemp
  argPtr            <- newTemp
  argsLeft          <- newTemp
  popAmt            <- newTemp
  pushStart         <- newLabel
  pushDone          <- newLabel
  (lhsCode, lhsFns) <- translateExpr ctx lhsTemp lhs
  (rhsCode, rhsFns) <- translateExpr ctx rhsTemp rhs
  return
    ( lhsCode
    ++ rhsCode
    ++ [ OP MOV $ MR (getField 1 lhsTemp) argsLeft
       , LEA (getField 2 lhsTemp) argPtr
       , LABEL pushStart
       , OP CMP $ IR 0 argsLeft
       , JLE pushDone
       , PUSHM (deref argPtr)
       , OP ADD $ IR 8 argPtr
       , DEC argsLeft
       , JMP pushStart
       , LABEL pushDone
       , PUSH rhsTemp
       , CALLM (getField 0 lhsTemp)
       , OP MOV $ MR (getField 1 lhsTemp) popAmt
       , INC popAmt
       , OP IMUL $ IR 8 popAmt
       , OP ADD $ RR popAmt rsp
       , OP MOV $ RR rax dst
       ]
    , lhsFns ++ rhsFns
    )
translateExpr ctx dst (Case arg branches) = do
  undefined
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
      , CALL "memoryAlloc"
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
    , function lambdaName (bodyCode ++ [OP MOV $ RR bodyDst rax]) : bodyFns
    )
translateExpr ctx dst (Let name val body) = do
  temp                <- newTemp
  (letCode , letFns ) <- translateExpr ctx temp val
  (bodyCode, bodyFns) <- translateExpr (withBinding name temp ctx) dst body
  return (letCode ++ bodyCode, letFns ++ bodyFns)

translateDecl :: Resolver -> Decl -> [Function VirtualRegister]
translateDecl = undefined

translateBundle :: Resolver -> Bundle -> Program VirtualRegister
translateBundle resolver bundle = undefined
