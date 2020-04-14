module Primitives where

import           Control.Monad.State

import           Assembly

newTemp :: Stateful VirtualRegister
newTemp = do
  count <- get
  put $ count + 1
  return $ Virtual $ Temporary $ "%t" ++ show count

newLabel :: Stateful Label
newLabel = do
  count <- get
  put $ count + 1
  return $ Label $ "l" ++ show count

getField :: Int -> VirtualRegister -> Mem VirtualRegister
getField n reg = Mem (fromIntegral $ 8 * n) reg Nothing

-- warning: gets arguments in reverse order!
getArg :: Int -> Mem VirtualRegister
getArg n = getField (n + 1) rsp

basicOp :: String -> Op -> Stateful (Function VirtualRegister)
basicOp name op = do
  temp <- newTemp
  return $ function
    name
    [ Right
        [ OP MOV $ MR (getArg 2) temp
        , OP op $ MR (getArg 1) temp
        , OP MOV $ RR temp rax
        , RET
        ]
    ]

plus :: Stateful (Function VirtualRegister)
plus = basicOp "plus" ADD

minus :: Stateful (Function VirtualRegister)
minus = basicOp "minus" SUB

times :: Stateful (Function VirtualRegister)
times = basicOp "times" IMUL

divOp :: [Instruction VirtualRegister] -> Stateful (Function VirtualRegister)
divOp name post = do
  temp <- newTemp
  return $ function
    name
    [ Right
      $  [ OP MOV $ MR (getArg 2) rax
         , CQTO
         , OP MOV $ MR (getArg 1) temp
         , IDIV temp
         ]
      ++ post
      ++ [RET]
    ]

divide :: Stateful (Function VirtualRegister)
divide = divOp "divide" []

modulo :: Stateful (Function VirtualRegister)
modulo = divOp "modulo" [OP MOV $ RR rdx rax]

and :: Stateful (Function VirtualRegister)
and = basicOp "and" AND

or :: Stateful (Function VirtualRegister)
or = basicOp "or" OR

xor :: Stateful (Function VirtualRegister)
xor = basicOp "xor" XOR

bitnot :: Stateful (Function VirtualRegister)
bitnot = do
  temp <- newTemp
  return $ function
    "not"
    [Right [OP MOV $ MR (getArg 1) temp, NOT temp, OP MOV $ RR temp rax]]

shiftOp :: Shift -> Stateful (Function VirtualRegister)
shiftOp name op = do
  arg       <- newTemp
  fixup     <- newLabel
  fixupDone <- newLabel
  let needsZeroing = case op of
        SHL -> True
        SAL -> True
        SHR -> True
        SAR -> False
  return $ function
    name
    [ Right
      [ OP MOV $ MR (getArg 2) arg
      , OP MOV $ MR (getArg 1) rcx
      , OP CMP $ IR 64 rcx
      , JGE fixup
      ]
    , Right [SHIFT Nothing op arg]
    , Left fixupDone
    , Right [OP MOV $ RR arg rax, RET]
    , Left fixup
    , Right
      $ if needsZeroing then [OP MOV $ IR 0 arg] else [SHIFT (Just 63) op arg]
    , Right [RET]
    ]

shl :: Stateful (Function VirtualRegister)
shl = shiftOp "shl" SHL

shr :: Stateful (Function VirtualRegister)
shr = shiftOp "shr" SHR

sal :: Stateful (Function VirtualRegister)
sal = shiftOp "sal" SAL

sar :: Stateful (Function VirtualRegister)
sar = shiftOp "sar" SAR

print :: Stateful (Function VirtualRegister)
print = do
  arg   <- newTemp
  start <- newLabel
  done  <- newLabel
  return $ function
    "print"
    [ Right [OP MOV $ MR (getArg 1) arg]
    , Left start
    , Right
      [ OP CMP $ IM 0 (getField 0 arg)
      , JE done
      , OP MOV $ IR 1 rax
      , OP MOV $ IR 1 rdi
      -- relies on little-endian architecture to pick out correct byte
      , OP MOV $ MR (getField 1 arg) rsi
      , OP MOV $ IR 1 rdx
      , SYSCALL 3
      , OP CMP $ IR 0
      , JL (Label "crash")
      , OP MOV $ MR (getField 2 arg) arg
      , JMP start
      ]
    ]
