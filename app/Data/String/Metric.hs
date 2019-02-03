module Data.String.Metric
  ( levenshtein
  ) where

import qualified Data.Map.Strict as Map

levenshtein :: String -> String -> Int
levenshtein s1 s2 = d
  where
    (d, _) = _levenshtein Map.empty s1 s2 0 0

_levenshtein :: Cache -> String -> String -> Int -> Int -> (Int, Cache)
_levenshtein cache [] y idx idy =
  (length y, Map.insert (idx, idy) (length y) cache)
_levenshtein cache x [] idx idy =
  (length x, Map.insert (idx, idy) (length x) cache)
_levenshtein cache s@(x:xs) t@(y:ys) idx idy = (m, Map.insert (idx, idy) m c3)
  where
    m = minimum [a + 1, b + 1, c + fromEnum (x /= y)]
    (a, c1) = fromCache cache _levenshtein xs t (idx + 1) idy
    (b, c2) = fromCache c1 _levenshtein s ys idx (idy + 1)
    (c, c3) = fromCache c2 _levenshtein xs ys (idx + 1) (idy + 1)

type Cache = Map.Map (Int, Int) Int

fromCache ::
     Cache
  -> (Cache -> String -> String -> Int -> Int -> (Int, Cache))
  -> String
  -> String
  -> Int
  -> Int
  -> (Int, Cache)
fromCache cache f x y n m =
  case Map.lookup (n, m) cache of
    Nothing -> f cache x y n m
    Just j  -> (j, cache)
