module Tokens where

import           Data.Int

data Token = LPAREN
           | RPAREN
           | LBRACKET
           | RBRACKET
           | SYMBOL String
           | INTEGER Int64
           | STRING String
  deriving (Show)
