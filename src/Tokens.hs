module Tokens where

import           Data.Int

data Token = LPAREN
           | RPAREN
           | LBRACKET
           | RBRACKET
           | SYMBOL String
           | INTEGER Int64
           | CHAR Char
           | STRING String
  deriving (Eq, Show)

data Form = RoundList [Form]
          | SquareList [Form]
          | Symbol String
          | IntAtom Int64
          | CharAtom Char
          | StrAtom String

instance Show Form where
  show (RoundList  forms) = "(" ++ unwords (map show forms) ++ ")"
  show (SquareList forms) = "[" ++ unwords (map show forms) ++ "]"
  show (Symbol     s    ) = s
  show (IntAtom    i    ) = show i
  show (CharAtom   c    ) = show c
  show (StrAtom    s    ) = show s
