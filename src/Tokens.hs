module Tokens where

import           Data.Int

data Token = LPAREN
           | RPAREN
           | LBRACKET
           | RBRACKET
           | SYMBOL
           | INTEGER Int64
           | STRING String
