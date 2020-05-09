module Liveness
  ( Liveness
  , ProgramLiveness
  , assertNoFreeVariables
  , computeLiveness
  , showLiveness
  )
where

import           Data.List
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Vector                   as V

import           Assembly
import           Util

{-# ANN module "HLint: ignore Use tuple-section" #-}

type Liveness reg = V.Vector (Set.Set reg, Set.Set reg)
type ProgramLiveness reg = [(Function reg, Liveness reg)]

lookupLabel :: Map.Map Label Int -> Label -> Int
lookupLabel labelMap label = case label `Map.lookup` labelMap of
  Nothing  -> error $ "liveness analysis hit unresolved label " ++ show label
  Just idx -> idx

assertNoFreeVariables :: Show reg => Liveness reg -> Liveness reg
assertNoFreeVariables analysis = if Set.null . fst . V.head $ analysis
  then analysis
  else
    error $ "free variables: " ++ (show . Set.toList . fst . V.head $ analysis)

computeLiveness
  :: (Eq reg, Ord reg, RegisterLike reg, Show reg)
  => [Instruction reg]
  -> Liveness reg
computeLiveness instrs = fixedPoint initial propagate
 where
  instrMap = V.fromList instrs
  labelMap = V.ifoldr
    (\idx instr lm -> case instr of
      LABEL name -> Map.insert name idx lm
      _          -> lm
    )
    Map.empty
    instrMap
  flowGraph = V.imap
    (\idx instr -> case getJumpType instr of
      Straightline | idx == length instrs - 1 -> []
      Straightline | otherwise                -> [idx + 1]
      Jump label                              -> [lookupLabel labelMap label]
      Branch label | idx == length instrs - 1 -> [lookupLabel labelMap label]
      Branch label | otherwise -> [lookupLabel labelMap label, idx + 1]
    )
    instrMap
  initial = V.replicate (V.length instrMap) (Set.empty, Set.empty)
  propagate origInfo = foldl
    (\info idx ->
      let
        (used, defined) = getRegisters $ instrMap V.! idx
        liveOut = Set.unions (map (\s -> fst (info V.! s)) (flowGraph V.! idx))
        liveIn =
          ((liveOut Set.\\ Set.fromList defined) `Set.union` Set.fromList used)
            Set.\\ Set.fromList (map fromRegister specialRegisters)
      in
        info V.// [(idx, (liveIn, liveOut))]
    )
    origInfo
    (take (V.length origInfo) (iterate (\i -> i - 1) (V.length origInfo - 1)))

showLiveness
  :: (Eq reg, Ord reg, RegisterLike reg, Show reg)
  => ProgramLiveness reg
  -> String
showLiveness = concatMap
  (\((Function _ name instrs), liveness) ->
    ".globl " ++ name ++ "\n" ++ name ++ ":\n" ++ concat
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
        (V.toList liveness)
      )
  )
