module Main
  ( main
  ) where

import           Control.Monad
import           Data.Argmax
import           Data.Maybe
import           Data.Tree
import           System.Exit
import           System.IO.Class
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
  unless (isNothing $ bestMatch ["hello", "world"] "xxx") $ do
    putStrLn "error in best match"
    exitFailure
  unless (runTreeState (listDirectory "var") fileSystem == ["lib", "data"]) $ do
    putStrLn "var should contain [lib, data]"
    exitFailure
  unless (runTreeState (listDirectory "usr") fileSystem == ["share"]) $ do
    putStrLn "usr should contain [share]"
    exitFailure
  unless (runTreeState (listDirectory "/") fileSystem == ["var", "usr"]) $ do
    putStrLn "/ should contain [var, usr]"
    exitFailure
  unless (runTreeState (listDirectory ".") fileSystem == ["var", "usr"]) $ do
    putStrLn ". should contain [var, usr]"
    exitFailure
  unless
    (runTreeState (listDirectory "var/data") fileSystem ==
     ["psql", "www", "srv"]) $ do
    putStrLn "var/data should contain [psql, www, srv]"
    exitFailure
  unless
    (runTreeState (listDirectory "/var/data") fileSystem ==
     ["psql", "www", "srv"]) $ do
    putStrLn "/var/data should contain [psql, www, srv]"
    exitFailure
  unless
    (runTreeState (listDirectory "./var/data") fileSystem ==
     ["psql", "www", "srv"]) $ do
    putStrLn "./var/data should contain [psql, www, srv]"
    exitFailure
  unless (null $ runTreeState (listDirectory "var/data/srv") fileSystem) $ do
    putStrLn "var/data/srv should should be empty"
    exitFailure
  unless (runTreeState (exists "var/data/srv") fileSystem) $ do
    putStrLn "a should exist"
    exitFailure
  unless
    (runTreeState (closeFilePath "." ["var", "data", "psql"]) fileSystem ==
     Just "var/data/psql") $ do
    putStrLn $
      "var/data/psql /= " ++
      show (runTreeState (closeFilePath "." ["var", "data", "psql"]) fileSystem)
    exitFailure
  unless
    (runTreeState (closeFilePath "." ["vr", "daa", "pqsl"]) fileSystem ==
     Just "var/data/psql") $ do
    putStrLn "nothing works"
    exitFailure
