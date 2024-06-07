{-# LANGUAGE Arrows #-}

{- | Control flow.

Sometimes you might want to change the game.
In Rhine (and other frameworks based on monadic stream functions like dunai, bearriver, and essence-of-live-coding),
we throw and catch exceptions to switch between different signal functions.
To do this, there is a monad interface for exception-throwing signal functions, 'ClSFExcept'.

In short:

* Exceptions are effects in the 'ExceptT' monad transformer, which is added to the monad stack of the 'ClSF'.
* To enter the new 'ClSFExcept' monad context, you can use:
  * 'try :: ClSF (ExceptT e m) cl a b -> ClSFExcept cl a b m e' for switching to a 'ClSF' that may throw an exception
  * 'safe :: ClSF m cl a b -> ClSFExcept cl a b m void' for finally switching to a 'ClSF' that will never throw an exception
* To handle exceptions, use do notation:
  A 'ClSFExcept' value returns its exception, and you can then switch to the next signal function by adding a statement to the do block.
* To leave the 'ClSFExcept' context after all exceptions have been handled, use 'safely :: ClSFExcept cl a b m Void -> ClSF m cl a b'

See https://hackage.haskell.org/package/rhine/docs/FRP-Rhine-ClSF-Except.html
and section 4.2 of the original research article https://www.manuelbaerenz.de/files/Rhine.pdf for more details.

With these ingredients, we can change the game to be over!
Fix the code such that if the snake hits the boundaries or itself, the game will change to a special gameover state.
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

-- | The user can change the direction of the snake
data Turn
  = -- | Turn right (clockwise) when the right arrow is pressed.
    TurnRight
  | -- | Turn left (counterclockwise) when the left arrow is pressed.
    TurnLeft
  deriving (Show)

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

newtype Apple = Apple {getApple :: Position}
  deriving (Eq, Ord)

newApple :: (MonadRandom m) => ClSF m GameClock () (Maybe Apple)
newApple = proc _ -> do
  nSteps :: Int <- count -< ()
  if nSteps `mod` 10 == 1
    then arr (Just <<< Apple) <<< getRandomRS -< (Position (-boardSize) (-boardSize), Position boardSize boardSize)
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

snakeSF :: ClSF GlossConc GameClock (Maybe Turn, Eat) Snake
snakeSF = unfold_ (snek North mempty) $ \(turn, eat) s -> stepSnake turn eat s

applesSF :: ClSF GlossConc GameClock Position (Apples, Eat)
applesSF = feedback empty $ proc (eatPosition, oldApples) -> do
  addedApple <- evalRandIOS' newApple -< ()
  let (newApples, eat) = addAndEatApple addedApple eatPosition oldApples
  returnA -< ((newApples, eat), newApples)

snakeAndApples :: ClSF GlossConc GameClock (Maybe Turn) (Snake, Apples)
snakeAndApples = feedback DontEat $ proc (turn, eat) -> do
  snake <- snakeSF -< (turn, eat)
  (apples, eatNext) <- applesSF -< head $ body snake
  returnA -< ((snake, apples), eatNext)

-- | Whether a snake hits the boundaries or bites itself
illegal :: Snake -> Bool
illegal Snake {body = head_@Position {x, y} :| tail_} =
  head_ `elem` tail_
    || x < (-boardSize)
    || x > boardSize
    || y < (-boardSize)
    || y > boardSize

-- | Play snake until the snake is in an illegal state
game :: ClSF GlossConc GameClock (Maybe Turn) (Maybe (Snake, Apples))
game = safely $ do
  _

-- Have a look at https://hackage.haskell.org/package/rhine/docs/FRP-Rhine-ClSF-Except.html.
-- We first want to play 'snakeAndApples' until the 'illegal' function returns 'True' on the current snake.
-- This has to throw an exception.
-- Catch the exception by outputting 'Nothing' forever.

-- Hint 1: Combine the following building blocks (plus a few things from base) to solve the puzzle:
-- try, liftClSF, snakeAndApples, throwOnCond, illegal, safe
-- Hint 2: The functions you need to combine are already in the right order
-- Hint 3: The do notation has to have 2 statements in total
-- Hint 4: These are the base functions and constructors you might also need:
--   $ (several times), >>> (several times), fst, arr, Just, pure, Nothing

render :: Maybe (Snake, Apples) -> Picture
render (Just (snake, apples)) = renderSnake snake <> foldMap renderApple apples
render Nothing = gameover

gameover :: Picture
gameover = translate (-3.5) 0 $ scale 0.01 0.01 $ text "Game over!"

-- | Scale and paint a gloss picture
visualize :: BehaviourF GlossConc UTCTime Picture ()
visualize = arrMCl $ scale 20 20 >>> paintAllIO

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

rhine :: Rhine GlossConc (UserClock `SequentialClock` (GameClock `SequentialClock` VisualizationClock)) () ()
rhine = user @@ userClock >-- fifoBounded 1000 --> (game >-> arr render @@ gameClock) >-- keepLast mempty --> visualize @@ visualizationClock

main :: IO ()
-- Make sure to keep this definition here as it is: The tests depend on it.
main = flowGlossIO defaultSettings rhine
