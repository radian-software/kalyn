module Liveness where

import           Control.Exception
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

import           Assembly
import           Util

{-# ANN module "HLint: ignore Use tuple-section" #-}

computeLiveness
  :: (Eq reg, Ord reg, RegisterLike reg)
  => [Instruction reg]
  -> Map.Map Int (Set.Set reg, Set.Set reg)
computeLiveness instrs =
  -- check no free variables at beginning of function
  let analysis = fixedPoint initial propagate
  in  assert (Set.null . fst . (Map.! 0) $ analysis) analysis
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
      Jump label                              -> [labelMap Map.! label]
      Branch label | idx == length instrs - 1 -> [labelMap Map.! label]
      Branch label | otherwise                -> [labelMap Map.! label, idx + 1]
    )
    instrMap
  initial = Map.map (const (Set.empty, Set.empty)) instrMap
  propagate info = Map.mapWithKey
    (\idx (_, liveOut) ->
      let (used, defined) = getRegisters $ instrMap Map.! idx
      in  ( (liveOut Set.\\ Set.fromList defined) `Set.union` Set.fromList used
          , Set.unions (map (\s -> fst (info Map.! s)) (flowGraph Map.! idx))
          )
    )
    info
