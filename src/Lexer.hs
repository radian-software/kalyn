module Lexer
  ( tokenize
  ) where

import           Data.Char
import           Data.Maybe

import           Tokens

disallowedChars :: String
disallowedChars = ";()[]@"

getStringChar :: String -> (Char, String, Bool)
getStringChar [] = error "unexpected end of string literal"
getStringChar ('\\' : 'x' : a : b : s) = (read $ "0x" ++ [a, b], s, True)
getStringChar ('\\' : c : s) =
  ( case c of
    '0' -> '\0'
    'a' -> '\a'
    'b' -> '\b'
    'f' -> '\f'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    'v' -> '\v'
    _   -> c
  , s
  , True
  )
getStringChar (c : s) = (c, s, False)

readString :: Char -> String -> (String, String)
readString delim str =
  let (char, rest, escaped) = getStringChar str
  in  if char == delim && not escaped
        then ("", rest)
        else
          let (parsed, rest') = readString delim rest in (char : parsed, rest')

-- hand-roll instead of using regexes for two reasons:
-- 1. because that's what we have to do in Kalyn
-- 2. https://github.com/haskell-hvr/regex-tdfa/issues/12
getToken :: String -> (Maybe Token, String)
getToken [] = error "shouldn't be getting a token from an empty string"
getToken (c : s) | isSpace c = (Nothing, s)
getToken (';' : s)           = (Nothing, tail . dropWhile (/= '\n') $ s)
getToken ('(' : s)           = (Just LPAREN, s)
getToken (')' : s)           = (Just RPAREN, s)
getToken ('[' : s)           = (Just LBRACKET, s)
getToken (']' : s)           = (Just RBRACKET, s)
getToken ('@' : s)           = (Just AT, s)
getToken ('"' : s) =
  let (parsed, rest) = readString '"' s in (Just . STRING $ parsed, rest)
getToken full@('\'' : s) =
  let (parsed, rest) = readString '\'' s
  in  if length parsed == 1
        then (Just . CHAR $ head parsed, rest)
        else error $ "character literal had more than one character: " ++ full
getToken s@(c : _) | isDigit c =
  let (d, s') = span isAlphaNum s in (Just . INTEGER . read $ d, s')
getToken ('-' : s@(c : _)) | isDigit c =
  let (d, s') = span isAlphaNum s in (Just . INTEGER . read $ '-' : d, s')
getToken s =
  let (v, s') =
        span (\c -> (not . isSpace $ c) && c `notElem` disallowedChars) s
  in  (Just . SYMBOL $ v, s')

tokenize :: String -> [Token]
tokenize str = catMaybes $ getTokens str
 where
  getTokens [] = []
  getTokens s  = let (t, s') = getToken s in t : getTokens s'
