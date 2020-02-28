module Util where

fixedPoint :: Eq a => a -> (a -> a) -> a
fixedPoint x f = let fx = f x in if x == fx then x else fixedPoint fx f

collectMaybes :: [Maybe a] -> [a]
collectMaybes []             = []
collectMaybes (Nothing : ms) = collectMaybes ms
collectMaybes (Just a  : ms) = a : collectMaybes ms
