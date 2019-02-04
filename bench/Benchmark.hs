module Main
  ( main
  ) where

import           Criterion.Main
import           Data.Metric.Levenshtein

main :: IO ()
main =
  defaultMain
    [ bgroup
        "levenshtein"
        [ bench "0" $ whnf (levenshtein "") ""
        , bench "1" $ whnf (levenshtein "h") "h"
        , bench "2" $ whnf (levenshtein "ha") "he"
        , bench "3" $ whnf (levenshtein "hal") "hel"
        , bench "4" $ whnf (levenshtein "hall") "hell"
        , bench "5" $ whnf (levenshtein "hallo") "hello"
        , bench "11" $ whnf (levenshtein "hallo welt") "hello world"
        ]
    ]
