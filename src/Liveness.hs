module Liveness
  ( assertNoFreeVariables
  , computeLiveness
  , showLiveness
  )
where

import           Data.List
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

import           Assembly
import           Util

{-# ANN module "HLint: ignore Use tuple-section" #-}

type Liveness reg = Map.Map Int (Set.Set reg, Set.Set reg)

lookupLabel :: Map.Map Label Int -> Label -> Int
lookupLabel labelMap label = case label `Map.lookup` labelMap of
  Nothing  -> error $ "liveness analysis hit unresolved label " ++ show label
  Just idx -> idx

assertNoFreeVariables :: Show reg => Liveness reg -> Liveness reg
assertNoFreeVariables analysis = if Set.null . fst . (Map.! 0) $ analysis
  then analysis
  else
    error
    $  "free variables: "
    ++ (show . Set.toList . fst . (Map.! 0) $ analysis)

computeLiveness
  :: (Eq reg, Ord reg, RegisterLike reg, Show reg)
  => [Instruction reg]
  -> Liveness reg
computeLiveness instrs = fixedPoint initial propagate
 where
  instrMap = Map.fromList $ zip (iterate (+ 1) 0) instrs
  labelMap = foldr
    (\(idx, instr) lm -> case instr of
      LABEL name -> Map.insert name idx lm
      _          -> lm
    )
    Map.empty
    (Map.toList instrMap)
  flowGraph = Map.mapWithKey
    (\idx instr -> case getJumpType instr of
      Straightline | idx == length instrs - 1 -> []
      Straightline | otherwise                -> [idx + 1]
      Jump label                              -> [lookupLabel labelMap label]
      Branch label | idx == length instrs - 1 -> [lookupLabel labelMap label]
      Branch label | otherwise -> [lookupLabel labelMap label, idx + 1]
    )
    instrMap
  initial = Map.map (const (Set.empty, Set.empty)) instrMap
  propagate info = Map.mapWithKey
    (\idx (_, liveOut) ->
      let (used, defined) = getRegisters $ instrMap Map.! idx
      in  ( ((liveOut Set.\\ Set.fromList defined) `Set.union` Set.fromList used
            )
            Set.\\ Set.fromList (map fromRegister specialRegisters)
          , Set.unions (map (\s -> fst (info Map.! s)) (flowGraph Map.! idx))
          )
    )
    info

showLiveness
  :: (Eq reg, Ord reg, RegisterLike reg, Show reg) => Program reg -> String
showLiveness (Program mainFn fns _) = concatMap
  (\(Function name instrs) -> name ++ ":\n" ++ concat
    (zipWith
      (\instr (liveIn, liveOut) ->
        ";; live IN: "
          ++ (intercalate ", " . map show . Set.toList $ liveIn)
          ++ "\n"
          ++ (case instr of
               LABEL lname -> lname ++ ":"
               _           -> "\t" ++ show instr
             )
          ++ "\n;; live OUT: "
          ++ (intercalate ", " . map show . Set.toList $ liveOut)
          ++ "\n"
      )
      instrs
      (Map.elems . computeLiveness $ instrs)
    )
  )
  (mainFn : fns)
