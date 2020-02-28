module Lexer
  ( getTokens
  )
where

import qualified Data.ByteString.Lazy          as B
import           Text.Regex.TDFA

import           Tokens

getToken :: B.ByteString -> Int -> Token
getToken = undefined

getTokens' :: B.ByteString -> Int -> [Token]
getTokens' = undefined

getTokens :: B.ByteString -> [Token]
getTokens bs = getTokens' bs 0
