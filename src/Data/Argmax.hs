module Data.Argmax
  ( argmax
  , argmin
  ) where

import           Data.Ord

argmax :: (Ord b) => [a] -> (a -> b) -> a
argmax [] _     = error "argmin of empty set is undefined"
argmax (x:xs) f = fst . foldl best (x, f x) $ (zip <*> map f) xs

argmin :: (Ord b) => [a] -> (a -> b) -> a
argmin [] _ = error "argmin of empty set is undefined"
argmin xs f = argmax xs (Down . f)

best :: Ord b => (a, b) -> (a, b) -> (a, b)
best x@(_, dx) y@(_, dy) =
  if dx > dy
    then x
    else y
