{-# LANGUAGE Arrows #-}

{- | Randomness.

The classic game of snake has apples appearing at random places on the playing grid,
and the snake can move to eat them, growing larger every time it consumes one.
-}
module Koan where

-- base
import Data.List.NonEmpty hiding (insert, unfold)
import GHC.Generics
import Prelude hiding (head)

-- random
import System.Random
import System.Random.Stateful (UniformRange (..))

-- MonadRandom
import Control.Monad.Random

-- containers
import Data.Set hiding (toList)

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss

-- * Grid positions on the playing board

boardSize :: Int
boardSize = 9

-- | A grid position on which a part of the snake body, or an apple, may be.
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

-- * User input

-- | The user can change the direction of the snake
data Turn
  = -- | Turn right (clockwise) when the right arrow is pressed.
    TurnRight
  | -- | Turn left (counterclockwise) when the left arrow is pressed.
    TurnLeft
  deriving (Show)

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

-- * Game logic

-- ** Snake

-- | Applying a turn to the current direction can give a new direction, shifted clockwise or counterclockwise.
changeDirection :: Turn -> Direction -> Direction
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
stepSnake :: Maybe Turn -> Eat -> Snake -> Snake
stepSnake turnMaybe eat snake =
  let
    newDirection = maybe (direction snake) (`changeDirection` direction snake) turnMaybe
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

-- ** Apples

newtype Apple = Apple {getApple :: Position}
  deriving (Eq, Ord)

-- | Randomly generate a new apple every 10 steps, anywhere on the playing board.
newApple :: (Monad m) => ClSF (RandT StdGen m) GameClock () (Maybe Apple)
newApple = proc _ -> do
  nSteps :: Int <- count -< ()
  if nSteps `mod` 10 == 1
    then -- Create a new random position for an apple, within a given range.
    -- See https://hackage.haskell.org/package/rhine/docs/FRP-Rhine-ClSF-Random.html
    -- and keep in mind that RandT gives an instance of MonadRandom.
      arr (Just <<< Apple) <<< getRandomRS -< (Position (-boardSize) (-boardSize), Position boardSize boardSize)
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

-- ** Combining snake and apples

type GameClock = GlossConcTClock IO (Millisecond 500)

gameClock :: GameClock
gameClock = glossConcTClock waitClock

{- | Given the current user input and whether an apple was eaten in the last round,
  output the current snake.
-}
snakeSF :: ClSF GlossConc GameClock (Maybe Turn, Eat) Snake
snakeSF = unfold_ (snek North mempty) $ \(turn, eat) s -> stepSnake turn eat s

{- | Given the current position of the snake head,
  output the set of apples, and whether an apple is currently being eaten
-}
applesSF :: ClSF GlossConc GameClock Position (Apples, Eat)
applesSF = feedback empty $ proc (eatPosition, oldApples) -> do
  -- We want to reuse newApple from above to occasionally add new apples.
  -- But it's not in the GlossConc monad!
  -- Have a look again at https://hackage.haskell.org/package/rhine/docs/FRP-Rhine-ClSF-Random.html
  -- to find a way to run newApple in GlossConc by interpreting the RandT monad transformer.
  addedApple <- evalRandIOS' newApple -< ()
  let (newApples, eat) = addAndEatApple addedApple eatPosition oldApples
  returnA -< ((newApples, eat), newApples)

-- | Given the current user input, output the current snake and the apples
game :: ClSF GlossConc GameClock (Maybe Turn) (Snake, Apples)
-- Tie the big knot! Combine snakeSF and applesSF, but take care:
-- Each needs input from the other.
-- snakeSF needs to know whether an apple was eaten in the last round,
-- and applesSF needs to know where the head of the snake is now.
game = feedback DontEat $ proc (turn, eat) -> do
  snake <- snakeSF -< (turn, eat)
  (apples, eatNext) <- applesSF -< head $ body snake
  returnA -< ((snake, apples), eatNext)

-- Hint 1: Have a look back at the koan basic-2-4-count-all-the-words!
-- Hint 2: Save the information of whether an apple is currently being eaten as internal state,
-- so you will know in the next step whether one was eaten in the last round.
-- Start like this:
-- feedback DontEat _

render :: (Snake, Apples) -> Picture
render (snake, apples) = renderSnake snake <> foldMap renderApple apples

-- * Visualization

-- | Scale and paint a gloss picture
visualize :: BehaviourF GlossConc UTCTime Picture ()
visualize = arrMCl $ scale 20 20 >>> paintAllIO

-- | Draw at 30 FPS
type VisualizationClock = GlossClockUTC IO GlossSimClockIO

visualizationClock :: VisualizationClock
visualizationClock = glossClockUTC GlossSimClockIO

-- * The whole program

rhine :: Rhine GlossConc (UserClock `SequentialClock` (GameClock `SequentialClock` VisualizationClock)) () ()
rhine =
  user
    @@ userClock
    >-- fifoBounded 1000
    --> (game >-> arr render @@ gameClock)
      >-- keepLast mempty
    --> visualize
      @@ visualizationClock

main :: IO ()
-- Make sure to keep this definition here as it is: The tests depend on it.
main = flowGlossIO defaultSettings rhine
