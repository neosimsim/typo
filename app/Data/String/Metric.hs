module Data.String.Metric
  ( stringCompare
  , StringComparison(Different)
  ) where

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
{-stringDistance s t =-}
  {-sum $-}
  {-zipWith-}
    {-(\s t ->-}
       {-if s == t-}
         {-then 0-}
         {-else 1)-}
    {-s-}
    {-t-}
