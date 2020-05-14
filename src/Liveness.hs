module Liveness
  ( InstrLiveness
  , Liveness
  , ProgramLiveness
  , assertNoFreeVariables
  , assertNoFreeVariablesP
  , computeLiveness
  , computeProgramLiveness
  , instrDefined
  , instrLiveIn
  , instrLiveOut
  , instrUsed
  , showLiveness
  )
where

import           Data.List
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

import           Assembly
import           Util

{-# ANN module "HLint: ignore Use tuple-section" #-}

data InstrLiveness reg = InstrLiveness
  { instrLiveIn :: Set.Set reg
  , instrLiveOut :: Set.Set reg
  , instrUsed :: Set.Set reg
  , instrDefined :: Set.Set reg
  }
  deriving (Eq)

type Liveness reg = Map.Map Int (InstrLiveness reg)
type ProgramLiveness reg = [(Function reg, Liveness reg)]

lookupLabel :: Map.Map Label Int -> Label -> Int
lookupLabel labelMap label = case label `Map.lookup` labelMap of
  Nothing  -> error $ "liveness analysis hit unresolved label " ++ show label
  Just idx -> idx

assertNoFreeVariables :: Show reg => String -> Liveness reg -> Liveness reg
assertNoFreeVariables name analysis =
  if Set.null . instrLiveIn . (Map.! 0) $ analysis
    then analysis
    else
      error
      $  "in function "
      ++ show name
      ++ ", free variables: "
      ++ (show . Set.toList . instrLiveIn . (Map.! 0) $ analysis)

assertNoFreeVariablesP :: Show reg => ProgramLiveness reg -> ProgramLiveness reg
assertNoFreeVariablesP = map
  (\(fn@(Function _ name _), liveness) ->
    (fn, assertNoFreeVariables name liveness)
  )

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
      Straightline | otherwise -> [idx + 1]
      Jump label               -> [lookupLabel labelMap label]
      Branch label | idx == length instrs - 1 -> [lookupLabel labelMap label]
      Branch label | otherwise -> [lookupLabel labelMap label, idx + 1]
      Return                   -> []
    )
    instrMap
  initial = Map.map
    (const InstrLiveness { instrLiveIn  = Set.empty
                         , instrLiveOut = Set.empty
                         , instrUsed    = Set.empty
                         , instrDefined = Set.empty
                         }
    )
    instrMap
  propagate origInfo = foldr
    (\idx info ->
      let
        (used, defined) = getRegisters $ instrMap Map.! idx
        liveOut         = Set.unions
          (map (\s -> instrLiveIn (info Map.! s)) (flowGraph Map.! idx))
        liveIn =
          ((liveOut Set.\\ Set.fromList defined) `Set.union` Set.fromList used)
            Set.\\ Set.fromList (map fromRegister specialRegisters)
      in
        Map.insert
          idx
          InstrLiveness { instrLiveIn  = liveIn
                        , instrLiveOut = liveOut
                        , instrUsed    = Set.fromList used
                        , instrDefined = Set.fromList defined
                        }
          info
    )
    origInfo
    (Map.keys origInfo)

computeProgramLiveness
  :: (Eq reg, Ord reg, RegisterLike reg, Show reg)
  => Program reg
  -> ProgramLiveness reg
computeProgramLiveness (Program mainFn fns _) =
  map (\fn@(Function _ _ instrs) -> (fn, computeLiveness instrs)) (mainFn : fns)

orNone :: String -> String
orNone ""  = "(none)"
orNone str = str

showLiveness
  :: (Eq reg, Ord reg, RegisterLike reg, Show reg)
  => ProgramLiveness reg
  -> String
showLiveness = intercalate "\n" . map
  (\((Function _ name instrs), liveness) ->
    ".globl " ++ name ++ "\n" ++ name ++ ":\n" ++ concat
      (zipWith
        (\instr il ->
          "\n;; live IN: "
            ++ orNone
                 (intercalate ", " . map show . Set.toList . instrLiveIn $ il)
            ++ "\n;; used: "
            ++ orNone
                 (intercalate ", " . map show . Set.toList . instrUsed $ il)
            ++ "\n"
            ++ (case instr of
                 LABEL lname -> lname ++ ":"
                 _           -> "\t" ++ show instr
               )
            ++ "\n;; defined: "
            ++ orNone
                 (intercalate ", " . map show . Set.toList . instrDefined $ il)
            ++ "\n;; live OUT: "
            ++ orNone
                 (intercalate ", " . map show . Set.toList . instrLiveOut $ il)
            ++ "\n"
        )
        instrs
        (Map.elems liveness)
      )
  )
