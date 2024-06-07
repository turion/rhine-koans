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
  pics <- nub <$> stepGlossRhineWithInput rhine ((/ 30) <$> [0, 1 .. 30]) [keyRight, keyLeft]
  let expected = [Scale 20.0 20.0 Blank, Scale 20.0 20.0 (Translate 1.0 0.0 (ThickCircle 0.3 0.6)), Scale 20.0 20.0 (Translate 1.0 1.0 (ThickCircle 0.3 0.6))]
  when (pics /= expected) $
    do
      putStrLn $ "Unexpected pictures: " ++ show pics
      exitFailure
