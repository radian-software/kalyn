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

-- warning: gets arguments in reverse order!
getArg :: Int -> Mem VirtualRegister
getArg n = Mem (fromIntegral $ 8 * (n + 1)) rsp Nothing

basicOp :: Op -> Stateful (Function VirtualRegister)
basicOp op = do
  temp <- newTemp
  return $ function
    [ Right
        [ OP MOV $ MR (getArg 2) temp
        , OP op $ MR (getArg 1) temp
        , OP MOV $ RR temp rax
        , RET
        ]
    ]

plus :: Stateful (Function VirtualRegister)
plus = basicOp ADD

minus :: Stateful (Function VirtualRegister)
minus = basicOp SUB

times :: Stateful (Function VirtualRegister)
times = basicOp IMUL

divOp :: [Instruction VirtualRegister] -> Stateful (Function VirtualRegister)
divOp post = do
  temp <- newTemp
  return $ function
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
divide = divOp []

modulo :: Stateful (Function VirtualRegister)
modulo = divOp [OP MOV $ RR rdx rax]

and :: Stateful (Function VirtualRegister)
and = basicOp AND

or :: Stateful (Function VirtualRegister)
or = basicOp OR

xor :: Stateful (Function VirtualRegister)
xor = basicOp XOR

shiftOp :: Shift -> Stateful (Function VirtualRegister)
shiftOp op = do
  arg       <- newTemp
  fixup     <- newLabel
  fixupDone <- newLabel
  let needsZeroing = case op of
        SHL -> True
        SAL -> True
        SHR -> True
        SAR -> False
  return $ function
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
shl = shiftOp SHL

shr :: Stateful (Function VirtualRegister)
shr = shiftOp SHR

sal :: Stateful (Function VirtualRegister)
sal = shiftOp SAL

sar :: Stateful (Function VirtualRegister)
sar = shiftOp SAR

print :: Stateful (Function VirtualRegister)
print = undefined
