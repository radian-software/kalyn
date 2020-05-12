module Bridge where

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Assembly
import           MemoryManager
import           Primitives
import           Subroutines

{-# ANN module "HLint: ignore Use lambda-case" #-}

handleCurried
  :: Int
  -> String
  -> Stateful VirtualFunction
  -> (String, Stateful [VirtualFunction])
handleCurried n name fn = (name, (:) <$> fn <*> curryify n name)

handleCurriedM
  :: Int
  -> String
  -> Stateful VirtualFunction
  -> (String, Stateful [VirtualFunction])
handleCurriedM n name fn =
  ( name
  , do
    core       <- fn
    monadified <- monadify n (name ++ "__uncurried")
    curried    <- curryify n name
    return ([core, monadified] ++ curried)
  )

stdlibPublic :: Map.Map String (String, Stateful [VirtualFunction])
stdlibPublic = Map.fromList
  [ ("+"          , handleCurried 2 "plus" plus)
  , ("-"          , handleCurried 2 "minus" minus)
  , ("*"          , handleCurried 2 "times" times)
  , ("/"          , handleCurried 2 "divide" divide)
  , ("%"          , handleCurried 2 "modulo" modulo)
  , ("&"          , handleCurried 2 "and" bitAnd)
  , ("|"          , handleCurried 2 "or" bitOr)
  , ("^"          , handleCurried 2 "xor" xor)
  , ("~"          , handleCurried 1 "not" bitNot)
  , ("shl"        , handleCurried 2 "shl" shl)
  , ("shr"        , handleCurried 2 "shr" shr)
  , ("sal"        , handleCurried 2 "sal" sal)
  , ("sar"        , handleCurried 2 "sar" sar)
  , ("print"      , handleCurriedM 1 "print" monadPrint)
  , ("writeFile"  , handleCurriedM 2 "writeFile" monadWriteFile)
  , ("setFileMode", handleCurriedM 2 "setFileMode" setFileMode)
  , ("error"      , handleCurried 1 "error" primitiveError)
  , ("=="         , handleCurried 2 "equal" equal)
  , ("/="         , handleCurried 2 "notEqual" notEqual)
  , ("<"          , handleCurried 2 "lessThan" lessThan)
  , ("<="         , handleCurried 2 "lessThanEqual" lessThanEqual)
  , (">"          , handleCurried 2 "greaterThan" greaterThan)
  , (">="         , handleCurried 2 "greaterThanEqual" greaterThanEqual)
  , ("pure"       , handleCurriedM 1 "pure" monadPure)
  , (">>="        , handleCurriedM 2 "bind" monadBind)
  ]

stdlibPrivate :: [Stateful VirtualFunction]
stdlibPrivate = [memoryInit, memoryAlloc, memoryPackString, primitiveCrash]

getCalls :: VirtualFunction -> Set.Set String
getCalls (Function _ _ instrs) = Set.fromList $ concatMap
  (\instr -> case instr of
    JUMP CALL label -> [label]
    _               -> []
  )
  instrs

stdlibFns :: [VirtualFunction] -> Stateful [VirtualFunction]
stdlibFns fns = do
  let calls = Set.unions . map getCalls $ fns
  public <-
    concat
      <$> ( mapM snd
          . filter (\(name, _) -> name `Set.member` calls)
          . Map.elems
          $ stdlibPublic
          )
  private <- sequence stdlibPrivate
  return $ public ++ private

stdlibData :: [Datum]
stdlibData = [memoryFirstFree, memoryProgramBreak] ++ msgDatums ++ [heap]
