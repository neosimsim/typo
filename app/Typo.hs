module Typo
  ( main
  , closeFilePath
  , bestMatch
  ) where

import           Data.Maybe
import           Data.String.Metric
import           GHC.Exts
import           System.Environment
import           System.FilePath
import           System.IO
import           System.IO.Class
import           System.Process

-- | Given a String looking for the "closest" match in a String list.
bestMatch :: [String] -> String -> Maybe String
bestMatch [] _ = Nothing
bestMatch xs s =
  let best = head $ sortWith (stringCompare s) xs
   in if stringCompare s best == Different
        then Nothing
        else Just best

closeFilePath ::
     MonadDirectory m => FilePath -> [FilePath] -> m (Maybe FilePath)
closeFilePath _ [] = return $ Just ""
closeFilePath pwd (x:xs) = do
  files <- listDirectory pwd
  case bestMatch files x of
    Nothing -> return Nothing
    Just y -> do
      ys <- closeFilePath (joinPath [pwd, y]) xs
      case ys of
        Nothing  -> return Nothing
        Just foo -> return . Just $ joinPath [y, foo]

usage :: IO ()
usage = hPutStrLn stderr "usage: typo CMD PATHS..."

main :: IO ()
main = do
  args <- getArgs
  case args of
    (cmd:paths) -> do
      matches <- mapM (closeFilePath "." . splitDirectories) paths
      let suggestion = zipWith fromMaybe paths matches
      if suggestion == paths
        then callProcess cmd paths
        else do
          putStrLn $ "did you mean: " ++ cmd ++ " " ++ show suggestion
          answer <- getLine
          case answer of
            ('y':_) -> callProcess cmd suggestion
            _       -> return ()
    _ -> usage
