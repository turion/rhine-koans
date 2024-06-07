module Main where

-- base
import Control.Monad (when)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import System.Exit (exitFailure)

-- gloss
import Graphics.Gloss.Data.Picture

-- test-gloss
import TestGloss

-- koan

import Control.Monad.Random (mkStdGen, setStdGen)
import Data.Set (singleton)
import Koan (Apple (Apple), Direction (North), Position (..), Snake (..), render, rhine, snek)

main :: IO ()
main = do
  setStdGen $ mkStdGen 0
  pics <- fmap nub $ stepGlossRhineWithInput rhine ((/ 30) <$> [0, 1 .. 150]) $ cycle [keyRight, keyLeft]
  let apple = Apple $ Position {x = 4, y = 4} -- By fixing the stdgen the apple will always be here
      beforeEating =
        (,singleton apple) . snek North
          <$> [ Position {x = 1, y = 0}
              , Position {x = 1, y = 1}
              , Position {x = 2, y = 1}
              , Position {x = 2, y = 2}
              , Position {x = 3, y = 2}
              , Position {x = 3, y = 3}
              , Position {x = 4, y = 3}
              ]
      afterEating =
        (,mempty)
          <$> [ Snake {direction = North, body = Position {x = 4, y = 4} :| []}
              , Snake {direction = North, body = Position {x = 5, y = 4} :| [Position {x = 4, y = 4}]}
              , Snake {direction = North, body = Position {x = 5, y = 5} :| [Position {x = 5, y = 4}]}
              ]
      expected = map (scale 20 20) $ blank : map render (beforeEating ++ afterEating)
  when (pics /= expected) $
    do
      putStrLn $ "Unexpected pictures:\n" ++ unlines (show <$> pics)
      putStrLn $ "Expected:\n" ++ unlines (show <$> expected)
      exitFailure
