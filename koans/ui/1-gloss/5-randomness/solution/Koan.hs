{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Randomness.
-}
module Koan where

-- base
import Data.List.NonEmpty hiding (insert, unfold)
import Data.Maybe (fromMaybe)
import GHC.Generics

-- random
import System.Random

-- MonadRandom
import Control.Monad.Random

-- containers
import Data.Set hiding (toList)

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss
import System.Random.Stateful (UniformRange (..))
import Prelude hiding (head)

-- * Grid positions on the playing board

boardSize :: Int
boardSize = 20

data Position = Position
  { x :: Int
  , y :: Int
  }
  deriving (Generic, Eq, Ord)

instance Semigroup Position where
  Position x1 y1 <> Position x2 y2 = Position (x1 + x2) (y1 + y2)

instance Monoid Position where
  mempty = Position 0 0

-- | To generate random apple positions
instance Uniform Position

instance UniformRange Position where
  uniformRM (Position xLow yLow, Position xHigh yHigh) g = Position <$> uniformRM (xLow, xHigh) g <*> uniformRM (yLow, yHigh) g

instance Random Position

renderPosition :: Position -> Picture
renderPosition Position {x, y} = translate (fromIntegral x) (fromIntegral y) $ circleSolid 0.6

-- * Directions in which the snake can head

data Direction = East | North | West | South
  deriving (Enum)

-- | A position changes by a direction in one step
stepPosition :: Direction -> Position -> Position
stepPosition East = (<> Position 1 0)
stepPosition North = (<> Position 0 1)
stepPosition West = (<> Position (-1) 0)
stepPosition South = (<> Position 0 (-1))

-- | The user can change the direction of the snake
data Turn
  -- | Don't change the direction. This happens if no key is pressed.
  = Stay
  -- | Turn right (clockwise) when the right arrow is pressed.
  | TurnRight
  -- | Turn left (counterclockwise) when the left arrow is pressed.
  | TurnLeft
  deriving (Show)

changeDirection :: Turn -> Direction -> Direction
changeDirection Stay direction = direction
changeDirection TurnRight direction = toEnum $ (fromEnum direction - 1) `mod` 4
changeDirection TurnLeft direction = toEnum $ (fromEnum direction + 1) `mod` 4

-- | Whether the snake currently eats an apple.
data Eat = Eat | DontEat

data Snake = Snake
  { direction :: Direction
  , body :: NonEmpty Position
  }

-- | A small snake.
snek :: Direction -> Position -> Snake
snek direction tinyBody =
  Snake
    { direction
    , body = pure tinyBody
    }

-- | On every step, a snake can make a turn, and possibly eat an apple
stepSnake :: Turn -> Eat -> Snake -> Snake
stepSnake turn eat snake =
  let
    newDirection = changeDirection turn $ direction snake
    newHead = stepPosition newDirection $ Data.List.NonEmpty.head $ body snake
    newTail = tailAfterMeal eat snake
   in
    Snake
      { direction = newDirection
      , body = newHead :| newTail
      }
  where
    tailAfterMeal :: Eat -> Snake -> [Position]
    tailAfterMeal DontEat = Data.List.NonEmpty.init . body
    tailAfterMeal Eat = toList . body

renderSnake :: Snake -> Picture
renderSnake = foldMap renderPosition . body


newtype Apple = Apple {getApple :: Position}
  deriving (Eq, Ord)

newApple :: (MonadRandom m) => ClSF m GameClock () (Maybe Apple)
newApple = proc _ -> do
  nSteps :: Int <- count -< ()
  if nSteps `mod` 10 == 0
    then arr (Just <<< Apple) <<< getRandomRS -< (Position (-10) (-10), Position 10 10)
    else returnA -< Nothing

type Apples = Set Apple

addAndEatApple ::
  -- | Possibly a new apple appeared
  Maybe Apple ->
  -- | On this position the snake attempted to eat the apple
  Position ->
  -- | The previous collection of apples
  Apples ->
  (Apples, Eat)
addAndEatApple addedApple eatPosition oldApples =
  let addedApples = maybe oldApples (`insert` oldApples) addedApple
      newApples = delete (Apple eatPosition) addedApples
   in (newApples, if size newApples < size addedApples then Eat else DontEat)

renderApple :: Apple -> Picture
renderApple = color red . renderPosition . getApple

type GameClock = GlossConcTClock IO (Millisecond 500)

gameClock :: GameClock
gameClock = glossConcTClock waitClock

snakeSF :: ClSF GlossConc GameClock (Turn, Eat) Snake
snakeSF = unfold_ (snek North mempty) $ \(turn, eat) s -> stepSnake turn eat s

applesSF :: ClSF GlossConc GameClock Position (Apples, Eat)
applesSF = feedback empty $ proc (eatPosition, oldApples) -> do
  addedApple <- evalRandIOS' newApple -< ()
  let (newApples, eat) = addAndEatApple addedApple eatPosition oldApples
  returnA -< ((newApples, eat), newApples)

game :: ClSF GlossConc GameClock Turn (Snake, Apples)
game = feedback DontEat $ proc (turn, eat) -> do
  snake <- snakeSF -< (turn, eat)
  (apples, eatNext) <- applesSF -< head $ body snake
  returnA -< ((snake, apples), eatNext)

render :: (Snake, Apples) -> Picture
render (snake, apples) = renderSnake snake <> foldMap renderApple apples

-- | Scale and paint a gloss picture
visualize :: BehaviourF GlossConc UTCTime Picture ()
visualize = arrMCl $ scale 10 10 >>> paintAllIO

-- | Draw at 30 FPS
type VisualizationClock = GlossClockUTC IO GlossSimClockIO

visualizationClock :: VisualizationClock
visualizationClock = glossClockUTC GlossSimClockIO

-- | Select only those input events that correspond to turns of the snake
type UserClock = GlossClockUTC IO (SelectClock GlossEventClockIO Turn)

userClock :: UserClock
userClock =
  glossClockUTC $
    SelectClock
      { mainClock = GlossEventClockIO
      , select = \case
          (EventKey (SpecialKey KeyRight) Down _ _) -> Just TurnRight
          (EventKey (SpecialKey KeyLeft) Down _ _) -> Just TurnLeft
          _ -> Nothing
      }

-- | User input to turn the snake
user :: ClSF GlossConc UserClock () Turn
user = tagS

rhine = user @@ userClock >-- fifoBounded 1000 --> (arr (fromMaybe Stay) >-> game >-> arr render @@ gameClock) >-- keepLast mempty --> visualize @@ visualizationClock

main :: IO ()
-- Make sure to keep this definition here as it is: The tests depend on it.
main = flowGlossIO defaultSettings rhine
