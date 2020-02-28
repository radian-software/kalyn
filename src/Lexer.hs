module Lexer
  ( getTokens
  )
where

import           Codec.Binary.UTF8.String
import           Control.Applicative
import qualified Data.ByteString.Lazy          as B
import           Text.Regex.TDFA
import           Text.Regex.TDFA.String         ( )

import           Tokens
import           Util

patterns :: [(String, String -> Maybe Token)]
patterns =
  [ ("[[:space:]]+"            , const Nothing)
  , (";[^\n]*"                 , const Nothing)
  , ("\\("                     , const $ Just LPAREN)
  , ("\\)"                     , const $ Just RPAREN)
  , ("\\["                     , const $ Just LBRACKET)
  , ("\\]"                     , const $ Just RBRACKET)
  , ("[0-9]+|0[xX][0-9a-fA-F]+", Just . INTEGER . read)
  , ("[^][0-9();[:space:]\"][^][();[:space:]\"]*", Just . SYMBOL)
  , ( "\"([^\\\\\"]|\\\\[\\\\\"0abfnrtv]|\\\\x[0-9a-zA-Z]{2})*\""
    , Just . STRING . readString
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

getTokens :: B.ByteString -> [Token]
getTokens bs = collectMaybes $ getTokens' (decode $ B.unpack bs)
 where
  getTokens' [] = []
  getTokens' s  = let (s', t) = getToken s in t : getTokens' s'
