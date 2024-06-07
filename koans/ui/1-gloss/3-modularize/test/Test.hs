module Main where

-- base
import Control.Monad (when)
import Data.List (nub)
import System.Exit (exitFailure)

-- gloss
import Graphics.Gloss.Data.Picture

-- test-gloss
import TestGloss

-- koan
import Koan (rhine)

main :: IO ()
main = do
  pics <- stepGlossRhine rhine $ (/ 30) <$> [0, 1 .. 30]
  case nub pics of
    [Blank, Translate 0 fiveish (ThickCircle 5.0 10.0), Translate 0 tenish (ThickCircle 5.0 10.0)] ->
      when (fiveish < 5 || fiveish > 6 || tenish < 10 || tenish > 11) $ do
        putStrLn "Those were the right pictures, but the speed at which they moved seems off."
        exitFailure
    unexpectedPics -> do
      putStrLn $ "Unexpected pictures: " ++ show unexpectedPics
      exitFailure
