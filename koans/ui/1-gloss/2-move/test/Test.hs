module Main where

-- rhine-gloss
import FRP.Rhine.Gloss

-- test-gloss
import TestGloss

-- koan
import Koan (rhine)

main :: IO ()
main = do
  [pic1, pic2] <- stepGlossRhine rhine [0, 1]
  expectPic pic1 [translate 0 0 $ circleSolid 10]
  expectPic pic2 [translate 0 10 $ circleSolid 10]
