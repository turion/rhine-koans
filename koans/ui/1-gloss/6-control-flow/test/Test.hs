module Main where

-- base
import Control.Monad (when)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set (singleton)
import System.Exit (exitFailure)

-- MonadRandom
import Control.Monad.Random (mkStdGen, setStdGen)

-- gloss
import Graphics.Gloss.Data.Picture

-- test-gloss
import TestGloss

-- koan
import Koan (Apple (Apple), Direction (North), Position (..), Snake (..), render, rhine, snek)

main :: IO ()
main = do
  putStrLn "Don't worry, this will take half a minute :) ..."
  setStdGen $ mkStdGen 0
  pics <- fmap nub $ stepGlossRhineWithInput rhine ((/ 30) <$> [0, 1 .. 360]) $ cycle [keyRight, keyLeft]
  let apple = Apple Position {x = 4, y = 4} -- By fixing the stdgen the apple will always be here
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
      secondApple =
        (,singleton $ Apple Position {x = -9, y = -7})
          <$> [ Snake {direction = North, body = Position {x = 6, y = 5} :| [Position {x = 5, y = 5}]}
              , Snake {direction = North, body = Position {x = 6, y = 6} :| [Position {x = 6, y = 5}]}
              , Snake {direction = North, body = Position {x = 7, y = 6} :| [Position {x = 6, y = 6}]}
              , Snake {direction = North, body = Position {x = 7, y = 7} :| [Position {x = 7, y = 6}]}
              , Snake {direction = North, body = Position {x = 8, y = 7} :| [Position {x = 7, y = 7}]}
              , Snake {direction = North, body = Position {x = 8, y = 8} :| [Position {x = 8, y = 7}]}
              , Snake {direction = North, body = Position {x = 9, y = 8} :| [Position {x = 8, y = 8}]}
              , Snake {direction = North, body = Position {x = 9, y = 9} :| [Position {x = 9, y = 8}]}
              ]
      expected = map (scale 20 20) $ blank : map render ((Just <$> beforeEating ++ afterEating ++ secondApple) ++ [Nothing])
  when (pics /= expected) $
    do
      putStrLn $ "Unexpected pictures:\n" ++ unlines (show <$> pics)
      putStrLn $ "Expected:\n" ++ unlines (show <$> expected)
      exitFailure
