module Bridge where

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           AST
import           Assembly
import           MemoryManager
import           Primitives
import           Subroutines

{-# ANN module "HLint: ignore Use lambda-case" #-}

handleCurried
  :: Int
  -> String
  -> Stateful VirtualFunction
  -> Type
  -> (String, Stateful [VirtualFunction], Type, Int)
handleCurried n name fn ty = (name, (:) <$> fn <*> curryify n name, ty, n)

handleCurriedM
  :: Int
  -> String
  -> Stateful VirtualFunction
  -> Type
  -> (String, Stateful [VirtualFunction], Type, Int)
handleCurriedM n name fn ty =
  ( name
  , do
    core       <- fn
    monadified <- monadify n (name ++ "__uncurried")
    curried    <- curryify n name
    return ([core, monadified] ++ curried)
  , ty
  , n
  )

baseType :: TypeName -> Type
baseType name = Type [] name []

funcType' :: [Type] -> Type
funcType' []         = error "can't construct empty function type"
funcType' [ty      ] = ty
funcType' (ty : tys) = Type [] "Func" [ty, funcType' tys]

funcType :: [TypeName] -> Type
funcType = funcType' . map baseType

ioType :: TypeName -> Type
ioType name = Type [] "IO" [Type [] name []]

stdlibPublic :: Map.Map String (String, Stateful [VirtualFunction], Type, Int)
stdlibPublic = Map.fromList
  [ ("+"  , handleCurried 2 "plus" plus $ funcType ["Int", "Int", "Int"])
  , ("-"  , handleCurried 2 "minus" minus $ funcType ["Int", "Int", "Int"])
  , ("*"  , handleCurried 2 "times" times $ funcType ["Int", "Int", "Int"])
  , ("/"  , handleCurried 2 "divide" divide $ funcType ["Int", "Int", "Int"])
  , ("%"  , handleCurried 2 "modulo" modulo $ funcType ["Int", "Int", "Int"])
  , ("&"  , handleCurried 2 "and" bitAnd $ funcType ["Int", "Int", "Int"])
  , ("|"  , handleCurried 2 "or" bitOr $ funcType ["Int", "Int", "Int"])
  , ("^"  , handleCurried 2 "xor" xor $ funcType ["Int", "Int", "Int"])
  , ("~"  , handleCurried 1 "not" bitNot $ funcType ["Int", "Int"])
  , ("shl", handleCurried 2 "shl" shl $ funcType ["Int", "Int", "Int"])
  , ("shr", handleCurried 2 "shr" shr $ funcType ["Int", "Int", "Int"])
  , ("sal", handleCurried 2 "sal" sal $ funcType ["Int", "Int", "Int"])
  , ("sar", handleCurried 2 "sar" sar $ funcType ["Int", "Int", "Int"])
  , ( "print"
    , handleCurriedM 1 "print" monadPrint
      $ funcType' [baseType "String", ioType "Empty"]
    )
  , ( "writeFile"
    , handleCurriedM 2 "writeFile" monadWriteFile
      $ funcType' [baseType "String", baseType "String", ioType "Empty"]
    )
  , ( "setFileMode"
    , handleCurriedM 2 "setFileMode" setFileMode
      $ funcType' [baseType "String", baseType "Int", ioType "Empty"]
    )
  , ("error", handleCurried 1 "error" primitiveError $ funcType ["String", "a"])
  , ("==Int", handleCurried 2 "equal" equal $ funcType ["Int", "Int", "Bool"])
  , ( "/=Int"
    , handleCurried 2 "notEqual" notEqual $ funcType ["Int", "Int", "Bool"]
    )
  , ( "<Int"
    , handleCurried 2 "lessThan" lessThan $ funcType ["Int", "Int", "Bool"]
    )
  , ( "<=Int"
    , handleCurried 2 "lessThanEqual" lessThanEqual
      $ funcType ["Int", "Int", "Bool"]
    )
  , ( ">Int"
    , handleCurried 2 "greaterThan" greaterThan
      $ funcType ["Int", "Int", "Bool"]
    )
  , ( ">=Int"
    , handleCurried 2 "greaterThanEqual" greaterThanEqual
      $ funcType ["Int", "Int", "Bool"]
    )
  , ( "returnIO"
    , handleCurriedM 1 "return" monadReturn
      $ funcType' [baseType "a", ioType "a"]
    )
  , ( ">>=IO"
    , handleCurriedM 2 "bind" monadBind
      $ funcType' [ioType "a", funcType' [baseType "a", ioType "b"], ioType "b"]
    )
  , ( "trace"
    , handleCurried 2 "trace" primitiveTrace $ funcType ["String", "a", "a"]
    )
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
stdlibFns userFns = do
  let calls = Set.unions . map getCalls $ userFns
  allPublic <- mapM (\(_, fns, _, _) -> fns) . Map.elems $ stdlibPublic
  let public =
        concat
          . filter (any (\(Function _ fnName _) -> Set.member fnName calls))
          $ allPublic
  private <- sequence stdlibPrivate
  return $ public ++ private

stdlibData :: [Datum]
stdlibData = [memoryFirstFree, memoryProgramBreak] ++ msgDatums
