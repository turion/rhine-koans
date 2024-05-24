{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

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

boardSize :: Int
boardSize = 20

data Direction = East | North | West | South
  deriving (Enum)

stepPosition :: Direction -> Position -> Position
stepPosition East = (<> Position 1 0)
stepPosition North = (<> Position 0 1)
stepPosition West = (<> Position (-1) 0)
stepPosition South = (<> Position 0 (-1))

data Turn = Stay | TurnRight | TurnLeft
  deriving (Show)

changeDirection :: Turn -> Direction -> Direction
changeDirection Stay direction = direction
changeDirection TurnRight direction = toEnum $ (fromEnum direction - 1) `mod` 4
changeDirection TurnLeft direction = toEnum $ (fromEnum direction + 1) `mod` 4

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

renderPosition :: Position -> Picture
renderPosition Position {x, y} = translate (fromIntegral x) (fromIntegral y) $ circleSolid 0.6

renderSnake :: Snake -> Picture
renderSnake = foldMap renderPosition . body

type SnakeClock = GlossConcTClock IO (Millisecond 500)

snakeClock :: SnakeClock
snakeClock = glossConcTClock waitClock

snakeSF :: ClSF GlossConc SnakeClock (Turn, Eat) Snake
snakeSF = unfold (snek North mempty) $ \(turn, eat) s -> let s' = stepSnake turn eat s in Result s' s'


newtype Apple = Apple {getApple :: Position}
  deriving (Eq, Ord)

newApple :: (MonadRandom m) => ClSF m SnakeClock () (Maybe Apple)
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

applesSF :: ClSF GlossConc SnakeClock Position (Apples, Eat)
applesSF = feedback empty $ proc (eatPosition, oldApples) -> do
  addedApple <- evalRandIOS' newApple -< ()
  let (newApples, eat) = addAndEatApple addedApple eatPosition oldApples
  returnA -< ((newApples, eat), newApples)

renderApple :: Apple -> Picture
renderApple = color red . renderPosition . getApple

snakeAndApples :: ClSF GlossConc SnakeClock Turn (Snake, Apples)
snakeAndApples = feedback DontEat $ proc (turn, eat) -> do
  snake <- snakeSF -< (turn, eat)
  (apples, eatNext) <- applesSF -< head $ body snake
  returnA -< ((snake, apples), eatNext)

illegal :: Snake -> Bool
illegal Snake {body = head@Position {x, y} :| tail} =
  head `elem` tail
    || x < (-boardSize)
    || x > boardSize
    || y < (-boardSize)
    || y > boardSize

game :: ClSF GlossConc SnakeClock Turn (Maybe (Snake, Apples))
game = safely $ do
  try $ liftClSF snakeAndApples >>> throwOnCond (fst >>> illegal) () >>> arr Just
  safe $ pure Nothing

render :: Maybe (Snake, Apples) -> Picture
render (Just (snake, apples)) = renderSnake snake <> foldMap renderApple apples
render Nothing = gameover

gameover :: Picture
gameover = translate (-10) 0 $ scale 0.03 0.03 $ text "Game over!"

visualize :: BehaviourF GlossConc UTCTime Picture ()
visualize = arrMCl $ scale 10 10 >>> paintAllIO

type VisualizationClock = GlossClockUTC IO GlossSimClockIO

visualizationClock :: VisualizationClock
visualizationClock = glossClockUTC GlossSimClockIO

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

user :: ClSF GlossConc UserClock () Turn
user = tagS

rhine = user @@ userClock >-- fifoBounded 1000 --> (arr (fromMaybe Stay) >-> game >-> arr render @@ snakeClock) >-- keepLast mempty --> visualize @@ visualizationClock

main :: IO ()
-- Make sure to keep this definition here as it is: The tests depend on it.
main = flowGlossIO defaultSettings rhine
