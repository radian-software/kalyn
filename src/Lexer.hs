module Lexer
  ( tokenize
  )
where

import           Control.Applicative
import           Text.Regex.TDFA
import           Text.Regex.TDFA.String         ( )

import           Tokens
import           Util

-- Chars that cannot appear in a symbol, for use inside character
-- class. We have to put the brackets first for regex syntax reasons.
nonSymbol :: String
nonSymbol = "][()\"';[:space:]"

patterns :: [(String, String -> Maybe Token)]
patterns =
  [ ("[[:space:]]+"            , const Nothing)
  , (";[^\n]*"                 , const Nothing)
  , ("\\("                     , const $ Just LPAREN)
  , ("\\)"                     , const $ Just RPAREN)
  , ("\\["                     , const $ Just LBRACKET)
  , ("\\]"                     , const $ Just RBRACKET)
  , ("[0-9]+|0[xX][0-9a-fA-F]+", Just . INTEGER . read)
  , ("[^" ++ nonSymbol ++ "0-9][^" ++ nonSymbol ++ "]*", Just . SYMBOL)
  , ( "\"([^\\\\\"]|\\\\[\\\\\"0abfnrtv]|\\\\x[0-9a-zA-Z]{2})*\""
    , Just . STRING . readString
    )
  , ( "'([^\\\\\']|\\\\[\\\\\'0abfnrtv]|\\\\x[0-9a-zA-Z]{2})'"
    , (\s ->
        let s' = readString s
        in  if length s' == 1
              then Just . CHAR $ head s'
              else
                error
                $  error "character literal had more than one character: "
                ++ show s
      )
    )
  ]

readString :: String -> String
readString s = readString' (init $ tail s)
 where
  readString' "" = ""
  readString' ('\\' : 'x' : a : b : rst) =
    read ("0x" ++ [a, b]) : readString' rst
  readString' ('\\' : e : rst) =
    (case e of
        '\\' -> '\\'
        '"'  -> '"'
        '0'  -> '\0'
        'a'  -> '\a'
        'b'  -> '\b'
        'f'  -> '\f'
        'r'  -> '\r'
        't'  -> '\t'
        'v'  -> '\v'
        _    -> error $ "readString got to unreachable code with: " ++ s
      )
      : readString' rst
  readString' (c : rst) = c : readString' rst

getToken :: String -> (String, Maybe Token)
getToken s =
  let parses = map
        (\(regex, ctor) -> do
          text <- s =~~ ("\\`(" ++ regex ++ ")") :: Maybe String
          pure (text, ctor text)
        )
        patterns
  in  case foldr (<|>) Nothing parses of
        Nothing            -> error $ "failed to read token at: " ++ take 10 s
        Just (text, token) -> (drop (length text) s, token)

tokenize :: String -> [Token]
tokenize str = collectMaybes $ getTokens str
 where
  getTokens [] = []
  getTokens s  = let (s', t) = getToken s in t : getTokens s'
