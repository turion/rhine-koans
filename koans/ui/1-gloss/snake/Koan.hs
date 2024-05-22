{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Koan where

-- base
import Data.List.NonEmpty hiding (unfold)
import Data.Maybe (fromMaybe)

-- time
import Data.Time.Clock

-- lens
import Control.Lens

-- automaton
import Data.Stream.Result

-- rhine
import FRP.Rhine

-- rhine-gloss
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
  deriving Show

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
renderPosition Position {x, y} = translate (fromIntegral x) (fromIntegral y) $ circleSolid 1

renderSnake :: Snake -> Picture
renderSnake = foldMap renderPosition . (^. body)

type SnakeClock m = IOClock m (Millisecond 1000)

snakeClock :: (MonadIO m) => SnakeClock m
snakeClock = ioClock waitClock

snake :: (Applicative m) => ClSF m (SnakeClock m) Turn Snake
snake = unfold (snek North mempty) $ \turn s -> let s' = stepSnake turn DontEat s in Result s' s'

visualize :: (Monad m, MonadIO m) => BehaviourF (GlossConcT m) UTCTime Picture ()
visualize = arrMCl $ scale 10 10 >>> paintAllIO

type UserClock = SelectClock GlossEventClockIO Turn

userClock :: UserClock
userClock =
  SelectClock
    { mainClock = GlossEventClockIO
    , select = \case
        (EventKey (SpecialKey KeyRight) Down _ _) -> Just TurnRight
        (EventKey (SpecialKey KeyLeft) Down _ _) -> Just TurnLeft
        _ -> Nothing
    }

user :: (Monad m, MonadIO m) => ClSF m (GlossClockUTC UserClock) () Turn
user = tagS

rhine = user @@ glossClockUTC userClock >-- fifoBounded 1000 --> (arr (fromMaybe Stay) >-> snake >-> arr renderSnake @@ snakeClock) >-- keepLast mempty --> visualize @@ glossClockUTC GlossSimClockIO

-- FIXME excavate the PR that makes gloss clocks UTCTime

-- | Rescale the gloss clocks so they will be compatible with real 'UTCTime' (needed for compatibility with 'Millisecond')
type GlossClockUTC cl = RescaledClockS (GlossConcT IO) cl UTCTime (Tag cl)

glossClockUTC :: (Real (Time cl)) => cl -> GlossClockUTC cl
glossClockUTC cl =
  RescaledClockS
    { unscaledClockS = cl
    , rescaleS = const $ do
        now <- liftIO getCurrentTime
        return (arr $ \(timePassed, event) -> (addUTCTime (realToFrac timePassed) now, event), now)
    }

main :: IO ()
-- Make sure to keep this definition here as it is: The tests depend on it.
main = flowGlossIO defaultSettings rhine
