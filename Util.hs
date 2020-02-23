module Util where

fixedPoint :: Eq a => a -> (a -> a) -> a
fixedPoint x f = let fx = f x in if x == fx then x else fixedPoint fx f
