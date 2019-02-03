module Main
  ( main
  ) where

import           Control.Monad
import           Data.Argmax
import           Data.String.Metric
import           Data.Tree
import           System.Exit
import           System.IO.Class
import           Test.HUnit
import           Test.QuickCheck
import           Typo               hiding (main)

argminProjection :: [Integer] -> Integer -> Integer
argminProjection xs y = argmin xs (\z -> abs (z - y))

prop_argmin_projection_idempotent :: NonEmptyList Integer -> Integer -> Bool
prop_argmin_projection_idempotent (NonEmpty xs) y =
  argminProjection xs y == (argminProjection xs . argminProjection xs $ y)

prop_argmin_projection_identity :: [Integer] -> Bool
prop_argmin_projection_identity xs = all (\x -> argminProjection xs x == x) xs

prop_bestMatch_findsItself :: [String] -> String -> Bool
prop_bestMatch_findsItself ss s = bestMatch (s : ss) s == Just s

fileSystem :: Forest FilePath
fileSystem = unfoldForest tree ["var", "usr"]
  where
    tree "var"   = ("var", ["lib", "data"])
    tree "lib"   = ("lib", [])
    tree "data"  = ("data", ["psql", "www", "srv"])
    tree "psql"  = ("psql", [])
    tree "www"   = ("www", [])
    tree "srv"   = ("srv", [])
    tree "usr"   = ("usr", ["share"])
    tree "share" = ("share", [])
    tree _       = undefined

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  quickCheckResult prop_argmin_projection_idempotent >>= \r ->
    unless (isSuccess r) exitFailure
  quickCheckResult prop_argmin_projection_identity >>= \r ->
    unless (isSuccess r) exitFailure
  quickCheckResult prop_bestMatch_findsItself >>= \r ->
    unless (isSuccess r) exitFailure
  quickCheckResult prop_levenshtein_non_negative >>= \r ->
    unless (isSuccess r) exitFailure
  quickCheckResult prop_levenshtein_positive >>= \r ->
    unless (isSuccess r) (putStrLn "prop_levenshtein_positive" >> exitFailure)
  quickCheckResult prop_levenshtein_symmetric >>= \r ->
    unless (isSuccess r) (putStrLn "prop_levenshtein_symmetric" >> exitFailure)
  quickCheckResult prop_levenshtein_identity >>= \r ->
    unless (isSuccess r) (putStrLn "prop_levenshtein_identity" >> exitFailure)
  quickCheckResult prop_levenshtein_triangle >>= \r ->
    unless (isSuccess r) (putStrLn "prop_levenshtein_triangle" >> exitFailure)
  quickCheckResult prop_oldlevenshtein >>= \r ->
    unless (isSuccess r) (putStrLn "prop_oldlevenshtein" >> exitFailure)
  runTestTT
    (TestList
       [ TestLabel "listDirectory" testListDirectories
       , TestLabel "closeFilePath" testCloseFilePath
       , TestLabel "bestMatch" testBestMatch
       , TestLabel "exists" testExists
       ]) >>=
    (\c -> unless (errors c + failures c == 0) exitFailure)

testBestMatch :: Test
testBestMatch =
  TestCase $ assertEqual "" (bestMatch ["hello", "world"] "xxx") $ Just "world"

testExists :: Test
testExists =
  TestCase $ assertBool "" (runTreeState (exists "var/data/srv") fileSystem)

testListDirectories :: Test
testListDirectories =
  TestCase $ do
    assertEqual
      ""
      (runTreeState (listDirectory "var") fileSystem)
      ["lib", "data"]
    assertEqual "" (runTreeState (listDirectory "usr") fileSystem) ["share"]
    assertEqual "" (runTreeState (listDirectory "/") fileSystem) ["var", "usr"]
    assertEqual "" (runTreeState (listDirectory ".") fileSystem) ["var", "usr"]
    assertEqual
      ""
      (runTreeState (listDirectory "var/data") fileSystem)
      ["psql", "www", "srv"]
    assertEqual
      ""
      (runTreeState (listDirectory "/var/data") fileSystem)
      ["psql", "www", "srv"]
    assertEqual
      ""
      (runTreeState (listDirectory "./var/data") fileSystem)
      ["psql", "www", "srv"]
    assertEqual "" (runTreeState (listDirectory "var/data/srv") fileSystem) []

testCloseFilePath :: Test
testCloseFilePath =
  TestCase $ do
    assertEqual
      ""
      (runTreeState (closeFilePath "." ["var", "data", "pqsl"]) fileSystem)
      (Just "var/data/psql")
    assertEqual
      ""
      (runTreeState (closeFilePath "." ["vr", "daa", "pqsl"]) fileSystem)
      (Just "var/data/psql")

prop_levenshtein_non_negative :: String -> String -> Bool
prop_levenshtein_non_negative s t = levenshtein s t >= 0

prop_levenshtein_positive :: String -> String -> Property
prop_levenshtein_positive s t = s /= t ==> levenshtein s t > 0

prop_levenshtein_symmetric :: String -> String -> Bool
prop_levenshtein_symmetric s t = levenshtein s t == levenshtein t s

prop_levenshtein_identity :: String -> Bool
prop_levenshtein_identity s = levenshtein s s == 0

prop_levenshtein_triangle :: String -> String -> String -> Bool
prop_levenshtein_triangle s t u =
  levenshtein s t + levenshtein t u >= levenshtein s u

prop_oldlevenshtein :: String -> String -> Bool
prop_oldlevenshtein s t =
  levenshtein (take 5 s) (take 5 t) == levenshtein' (take 5 s) (take 5 t)

-- ! This is the trivial implementatoin of levenshteininstein distance. This
-- implementation is inefficient and should only be used as reference function.
levenshtein' :: String -> String -> Int
levenshtein' a b = innerlevenshtein a b (length a) (length b)
  where
    innerlevenshtein :: String -> String -> Int -> Int -> Int
    innerlevenshtein s t i j
      | min i j == 0 = max i j
      | otherwise =
        minimum
          [ innerlevenshtein s t (i - 1) j + 1
          , innerlevenshtein s t i (j - 1) + 1
          , innerlevenshtein s t (i - 1) (j - 1) +
            fromEnum ((s !! (i - 1)) /= (t !! (j - 1)))
          ]
