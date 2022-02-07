module Reader
  ( readModule
  ) where

import           Tokens

-- Simple recursive descent parser for Lisp syntax.

parseForm :: [Token] -> (Form, [Token])
parseForm (SYMBOL name : AT : rest) =
  let (aliased, rest') = parseForm rest in (At name aliased, rest')
parseForm (SYMBOL  s : rest) = (Symbol s, rest)
parseForm (INTEGER i : rest) = (IntAtom i, rest)
parseForm (CHAR    c : rest) = (CharAtom c, rest)
parseForm (STRING  s : rest) = (StrAtom s, rest)
parseForm (LPAREN : rest) =
  let (forms, rest') = parseForms rest
  in  case rest' of
        RPAREN : rest'' -> (RoundList forms, rest'')
        token -> error $ "unexpected " ++ show token ++ " when parsing ( ... )"
parseForm (LBRACKET : rest) =
  let (forms, rest') = parseForms rest
  in  case rest' of
        RBRACKET : rest'' -> (SquareList forms, rest'')
        token -> error $ "unexpected " ++ show token ++ " when parsing ( ... )"
parseForm tokens = error $ "failed to parse: " ++ show tokens

parseForms :: [Token] -> ([Form], [Token])
parseForms [] = ([], [])
parseForms (token : rest) | token `elem` [RPAREN, RBRACKET] = ([], token : rest)
parseForms tokens =
  let (form , rest ) = parseForm tokens
      (forms, rest') = parseForms rest
  in  (form : forms, rest')

readModule :: [Token] -> [Form]
readModule tokens =
  let (forms, rest) = parseForms tokens
  in  if null rest
        then forms
        else error $ "trailing garbage following program: " ++ show rest
