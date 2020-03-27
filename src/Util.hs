module Util where

fixedPoint :: Eq a => a -> (a -> a) -> a
fixedPoint x f = let fx = f x in if x == fx then x else fixedPoint fx f

collectMaybes :: [Maybe a] -> [a]
collectMaybes []             = []
collectMaybes (Nothing : ms) = collectMaybes ms
collectMaybes (Just a  : ms) = a : collectMaybes ms

leftover :: Integral n => n -> n -> n
leftover f x = (f - x) `mod` f

roundUp :: Integral n => n -> n -> n
roundUp f x = x + leftover f x
