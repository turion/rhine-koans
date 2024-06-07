module Main where

-- rhine-gloss
import FRP.Rhine.Gloss

-- test-gloss
import TestGloss

-- koan
import Koan (rhine)

main :: IO ()
main = do
  pics <- stepGlossRhine rhine [0, 1]
  expectPics pics [[translate 0 0 $ circleSolid 10], [translate 0 10 $ circleSolid 10]]
