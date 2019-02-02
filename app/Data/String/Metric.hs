module Data.String.Metric
  ( stringCompare
  , StringComparison(Different)
  , levenshtein
  ) where

import qualified Data.Map.Strict as Map

data StringComparison
  = Equal
  | Swapped
  | WrongChar
  | Substring
  | Supstring
  | Different
  deriving (Show, Eq, Enum)

instance Ord StringComparison where
  a <= b = fromEnum a <= fromEnum b

stringCompare :: String -> String -> StringComparison
stringCompare s t
  | s == t = Equal
  | swapped s t = Swapped
  | s `hasOneElementDifferentThan` t = WrongChar
  | isSubset s t = Substring
  | isSupset s t = Supstring
  | otherwise = Different
  where
    nEquals xs = sum . map fromEnum . zipWith (==) xs
    hasOneElementDifferentThan s1 s2 =
      length s1 == length s2 && nEquals s1 s2 == length s1 - 1

isSubset :: String -> String -> Bool
isSubset s t = all (`elem` t) s

isSupset :: String -> String -> Bool
isSupset s t = isSubset t s

swapped :: String -> String -> Bool
swapped _ _ = False

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
