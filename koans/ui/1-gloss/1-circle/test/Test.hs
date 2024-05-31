module Main where

-- rhine-gloss
import FRP.Rhine.Gloss

-- test-gloss
import TestGloss

-- koan
import Koan (rhine)

main :: IO ()
main = do
  [pic] <- stepGlossRhine rhine [1]
  expectPic pic [circleSolid 10]
