{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

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
  ) where

import           Control.DeepSeq
import           Data.List
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           GHC.Generics

import           Assembly
import           Util

{-# ANN module "HLint: ignore Use tuple-section" #-}

data InstrLiveness reg = InstrLiveness
  { instrLiveIn  :: Set.Set reg
  , instrLiveOut :: Set.Set reg
  , instrUsed    :: Set.Set reg
  , instrDefined :: Set.Set reg
  }
  deriving (Eq, Generic, NFData)

type Liveness reg = [InstrLiveness reg]
type ProgramLiveness reg = [(Function reg, Liveness reg)]

assertNoFreeVariables :: Show reg => String -> Liveness reg -> Liveness reg
assertNoFreeVariables name analysis =
  if Set.null . instrLiveIn . head $ analysis
    then analysis
    else
      error
      $  "in function "
      ++ show name
      ++ ", free variables: "
      ++ (show . Set.toList . instrLiveIn . head $ analysis)

assertNoFreeVariablesP
  :: Show reg => ProgramLiveness reg -> ProgramLiveness reg
assertNoFreeVariablesP = map
  (\(fn@(Function _ name _), liveness) ->
    (fn, assertNoFreeVariables name liveness)
  )

computeLiveness
  :: (Eq reg, Ord reg, RegisterLike reg, Show reg)
  => [Instruction reg]
  -> Liveness reg
computeLiveness instrs =
  let (livenesses, _) = fixedPoint ([], Map.empty) propagate
  in  zipWith
        (\(liveIn, liveOut) (used, defined) -> InstrLiveness
          { instrLiveIn  = liveIn
          , instrLiveOut = liveOut
          , instrUsed    = used
          , instrDefined = defined
          }
        )
        livenesses
        useDefs
 where
  special = Set.fromList (map fromRegister specialRegisters)
  useDefs = map (both Set.fromList . getRegisters) instrs
  propagate (_, origLabelLivenesses) = foldr
    (\(instr, (used, defined)) (newLivenesses, labelLivenesses) ->
      let getNextLiveness = case newLivenesses of
            []             -> []
            (liveness : _) -> [liveness]
          getLabelLiveness label = case Map.lookup label labelLivenesses of
            Nothing       -> []
            Just liveness -> [liveness]
          succLivenesses = case getJumpType instr of
            Straightline -> getNextLiveness
            Jump   label -> getLabelLiveness label
            Branch label -> getNextLiveness ++ getLabelLiveness label
            Return       -> []
          liveOut = Set.unions (map fst succLivenesses)
          liveIn = ((liveOut Set.\\ defined) `Set.union` used) Set.\\ special
          newLiveness = (liveIn, liveOut)
      in  case instr of
            LABEL name ->
              ( newLiveness : newLivenesses
              , Map.insert name newLiveness labelLivenesses
              )
            _ -> (newLiveness : newLivenesses, labelLivenesses)
    )
    ([], origLabelLivenesses)
    (zip instrs useDefs)

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
  (\((Function _ name instrs), livenesses) ->
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
        livenesses
      )
  )
