module Util where

import           Control.Arrow
import           Data.Char
import           Data.List
import qualified Data.Map                      as Map

class Pretty a where
  pretty :: a -> String

fixedPoint :: Eq a => a -> (a -> a) -> a
fixedPoint x f = let fx = f x in if x == fx then x else fixedPoint fx f

fixedPointN :: Eq a => Int -> a -> (a -> a) -> (Int, a)
fixedPointN n x f =
  let fx = f x in if x == fx then (n, x) else fixedPointN (n + 1) fx f

leftover :: Integral n => n -> n -> n
leftover f x = (f - x) `mod` f

roundUp :: Integral n => n -> n -> n
roundUp f x = x + leftover f x

groupBy :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupBy k = Map.fromListWith (++) . map (k &&& pure)

listUnique :: Eq k => [k] -> Bool
listUnique ks = nub ks == ks

mapUnionsWithKey :: Ord k => (k -> v -> v -> v) -> [Map.Map k v] -> Map.Map k v
mapUnionsWithKey f = foldl' (Map.unionWithKey f) Map.empty

userAllowedChars :: String
userAllowedChars = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

-- important properties: deterministic, stateless, and injective
sanitize :: String -> String
sanitize = concatMap
  $ \c -> if c `elem` userAllowedChars then [c] else "_u" ++ show (ord c)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a1, a2) = (f a1, f a2)
