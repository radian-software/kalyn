module Subroutines where

import           Control.Monad.State

import           Assembly

type Stateful = State Int

newTemp :: Stateful VirtualRegister
newTemp = do
  count <- get
  put $ count + 1
  return $ Virtual $ Temporary $ "%t" ++ show count

newLabel :: Stateful Label
newLabel = do
  count <- get
  put $ count + 1
  return $ "l" ++ show count

newLambda :: String -> Stateful Label
newLambda fnName = do
  count <- get
  put $ count + 1
  return $ fnName ++ "__lambda" ++ show count

getField :: Int -> VirtualRegister -> Mem VirtualRegister
getField n reg = Mem (Right $ fromIntegral $ 8 * n) reg Nothing

deref :: VirtualRegister -> Mem VirtualRegister
deref = getField 0

unpush :: Int -> VirtualInstruction
unpush n = OP ADD $ IR (fromIntegral $ 8 * n) rsp

-- warning: gets arguments in reverse order! indexed from 1
getArg :: Int -> Mem VirtualRegister
getArg n = getField (n + 2) rbp
