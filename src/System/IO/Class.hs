module System.IO.Class
  ( MonadDirectory(..)
  , TreeState
  , runTreeState
  , exists
  ) where

import           Control.Monad.State
import           Data.Tree
import qualified System.Directory    as IO
import           System.FilePath

class Monad m =>
      MonadDirectory m
  where
  listDirectory :: FilePath -> m [FilePath]

instance MonadDirectory IO where
  listDirectory = IO.listDirectory

newtype TreeState a = TreeState
  { treeState :: State (Forest FilePath) a
  }

runTreeState :: TreeState a -> Forest FilePath -> a
runTreeState s = evalState $ treeState s

instance Functor TreeState where
  fmap f (TreeState x) = TreeState $ fmap f x

instance Applicative TreeState where
  (TreeState f) <*> (TreeState x) = TreeState $ f <*> x
  pure = TreeState . pure

instance Monad TreeState where
  (TreeState s) >>= f = TreeState $ s >>= (treeState . f)

instance MonadDirectory TreeState where
  listDirectory path =
    TreeState . state $ \s ->
      (map rootLabel (treeAtPath s (splitDirectories path)), s)

treeAtPath :: [Tree FilePath] -> [FilePath] -> [Tree FilePath]
treeAtPath t [] = t
treeAtPath t ["."] = t
treeAtPath t (".":ps) = treeAtPath t ps
treeAtPath t ("/":ps) = treeAtPath t ps
treeAtPath [] _ = undefined
treeAtPath [Node x forest] (p:ps)
  | x == p = treeAtPath forest ps
  | otherwise = error "not found"
treeAtPath (Node x forest:others) path@(p:ps)
  | x == p = treeAtPath forest ps
  | otherwise = treeAtPath others path

exists :: (MonadDirectory m) => FilePath -> m Bool
exists f = do
  files <- listDirectory $ takeDirectory f
  return $ takeFileName f `elem` files
