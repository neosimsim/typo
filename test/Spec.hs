module Main
  ( main
  ) where

import           Control.Monad
import           Data.Argmax
import           Data.Tree
import           System.Exit
import           System.IO.Class
import           Test.HUnit
import           Test.QuickCheck
import           Typo            hiding (main)

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
  TestCase $ assertEqual "" (bestMatch ["hello", "world"] "xxx") Nothing

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
