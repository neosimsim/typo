module Data.Metric.Levenshtein
  ( levenshtein
  ) where

import           Control.Monad.State
import qualified Data.Map.Strict     as Map

levenshtein :: (Eq a) => [a] -> [a] -> Int
levenshtein s1 s2 = d
  where
    (d, _) = runState (_levenshtein s1 s2 0 0) Map.empty

_levenshtein :: (Eq a) => [a] -> [a] -> Int -> Int -> State Cache Int
_levenshtein [] y idx idy = insert (idx, idy) (length y) >> return (length y)
_levenshtein x [] idx idy = insert (idx, idy) (length x) >> return (length x)
_levenshtein s@(x:xs) t@(y:ys) idx idy = do
  a <- fromCache _levenshtein xs t (idx + 1) idy
  b <- fromCache _levenshtein s ys idx (idy + 1)
  c <- fromCache _levenshtein xs ys (idx + 1) (idy + 1)
  let m = minimum [a + 1, b + 1, c + fromEnum (x /= y)]
  insert (idx, idy) m
  return m

type Cache = Map.Map (Int, Int) Int

insert :: (Int, Int) -> Int -> State Cache ()
insert k v = state $ \cache -> ((), Map.insert k v cache)

fromCache ::
     (a -> b -> Int -> Int -> State Cache Int)
  -> a
  -> b
  -> Int
  -> Int
  -> State Cache Int
fromCache f x y n m =
  state $ \cache ->
    case Map.lookup (n, m) cache of
      Nothing -> runState (f x y n m) cache
      Just j  -> (j, cache)
