{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Koan where

-- lens
import Control.Lens

-- automaton
import Data.Stream.Result

-- rhine
import FRP.Rhine

-- rhine-gloss
import Data.List.NonEmpty hiding (unfold)
import FRP.Rhine.Gloss

data Position = Position
  { x :: Int
  , y :: Int
  }

instance Semigroup Position where
  Position x1 y1 <> Position x2 y2 = Position (x1 + x2) (y1 + y2)

instance Monoid Position where
  mempty = Position 0 0

data Direction = East | North | West | South
  deriving (Enum)

stepPosition :: Direction -> Position -> Position
stepPosition East = (<> Position 1 0)
stepPosition North = (<> Position 0 1)
stepPosition West = (<> Position (-1) 0)
stepPosition South = (<> Position 0 (-1))

data Turn = Stay | TurnRight | TurnLeft

changeDirection :: Turn -> Direction -> Direction
changeDirection Stay direction = direction
changeDirection TurnRight direction = toEnum $ (fromEnum direction - 1) `mod` 4
changeDirection TurnLeft direction = toEnum $ (fromEnum direction + 1) `mod` 4

data Eat = Eat | DontEat

data Snake = Snake
  { _direction :: Direction
  , _body :: NonEmpty Position
  }

makeLenses ''Snake

snek :: Direction -> Position -> Snake
snek _direction tinyBody =
  Snake
    { _direction
    , _body = pure tinyBody
    }

stepSnake :: Turn -> Eat -> Snake -> Snake
stepSnake turn eat snake =
  let
    newDirection = changeDirection turn $ _direction snake
    newHead = stepPosition newDirection $ Data.List.NonEmpty.head $ _body snake
    newTail = tailAfterMeal eat snake
   in
    Snake
      { _direction = newDirection
      , _body = newHead :| newTail
      }
  where
    tailAfterMeal :: Eat -> Snake -> [Position]
    tailAfterMeal DontEat = Data.List.NonEmpty.init . _body
    tailAfterMeal Eat = toList . _body

renderPosition :: Position -> Picture
renderPosition Position { x, y} = translate (fromIntegral x) (fromIntegral y) $ circleSolid 1

renderSnake :: Snake -> Picture
renderSnake = foldMap renderPosition . (^. body)

type SnakeClock = Millisecond 500

snake :: ClSF m SnakeClock Turn Snake
snake = unfold (snek North mempty) $ \turn s -> let s' = stepSnake turn DontEat s in Result s' s'

rhine :: Rhine (GlossConcT IO) (SeqClock GlossSimClockIO SnakeClock) () ()
rhine = _


-- FIXME excavate the PR that makes gloss clocks UTCTime
-- | Rescale the gloss clocks so they will be compatible with real 'UTCTime' (needed for compatibility with 'Millisecond')
type GlossClockUTC cl = RescaledClockS (GlossConcT IO) cl UTCTime (Tag cl)

glossClockUTC :: (Real (Time cl)) => cl -> GlossClockUTC cl
glossClockUTC cl =
  RescaledClockS
    { unscaledClockS = cl
    , rescaleS = proc (timePassed, event) -> do
        initTime <- onStart_ $ liftIO getCurrentTime -< ()
        returnA -< (addUTCTime (realToFrac timePassed) initTime, event)
    }

-- Make sure to keep this definition here as it is: The tests depend on it.
main :: IO ()
main = flowGlossIO defaultSettings rhine
