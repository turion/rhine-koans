{- | User input.

The game moves forward every half a second.
The gloss pictures are drawn at a fixed frame rate of 30 frames per second.
User input, however, arrives at unpredictable times!
This means that user input events constitute a separate clock.

rhine-gloss provides such a clock, 'GlossEventClockIO', but it ticks at every event that the gloss backend emits.
In our game of snake, we are only interested in those events where the player presses the right arrow key or the left arrow key.
Every time this happens, the event should be forwarded to the game logic,
where it should turn the direction into which the snake is heading.
-}
module Koan where

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss

-- * Grid positions on the playing board

-- | Currently, a snake will have size 1, so its body is defined by a single position.
data Position = Position
  { x :: Int
  , y :: Int
  }
  deriving (Eq, Ord)

instance Semigroup Position where
  Position x1 y1 <> Position x2 y2 = Position (x1 + x2) (y1 + y2)

instance Monoid Position where
  mempty = Position 0 0

renderPosition :: Position -> Picture
renderPosition Position {x, y} = translate (fromIntegral x) (fromIntegral y) $ circleSolid 0.6

-- | Directions in which the snake can head
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

-- | Ticks whenever the user wants to turn right or left
userClock :: UserClock
userClock =
  glossClockUTC $
    SelectClock
      { mainClock = GlossEventClockIO
      , -- Select only those events here that are relevant for the game, the right arrow key and the left arrow key.
        -- Have a look at https://hackage.haskell.org/package/gloss/docs/Graphics-Gloss-Interface-IO-Interact.html#t:Event.
        -- All other events should be mapped to Nothing.
        select = \case
          (EventKey (SpecialKey KeyRight) Down _ _) -> Just TurnRight
          (EventKey (SpecialKey KeyLeft) Down _ _) -> Just TurnLeft
          _ -> Nothing
          -- Hint: The important bits are KeyLeft and KeyRight!
      }

-- | User input to turn the snake
user :: ClSF GlossConc UserClock () Turn
user = tagS -- This simply returns the value of the current event, that is, the selected turn.

-- * Game logic

-- | Applying a turn to the current direction can give a new direction, shifted clockwise or counterclockwise.
changeDirection :: Turn -> Direction -> Direction
changeDirection TurnRight direction = toEnum $ (fromEnum direction - 1) `mod` 4
changeDirection TurnLeft direction = toEnum $ (fromEnum direction + 1) `mod` 4

type GameClock = GlossConcTClock IO (Millisecond 500)

gameClock :: GameClock
gameClock = glossConcTClock waitClock

-- | The current position of the tiny snake, reacting to turns of the player.
game :: ClSF GlossConc GameClock (Maybe Turn) Position
-- unfold takes a starting state and a step function to create a signal function.
game = unfold (mempty, North) $ \turnMaybe (position, direction) ->
  -- Use helper functions defined above to calculate the new position and direction!
  let newDirection = maybe direction (`changeDirection` direction) turnMaybe
      newPosition = stepPosition newDirection position
   in Result (newPosition, newDirection) newPosition

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
    -- The choice of resampling buffer here has a big influence on the game play.
    -- A FIFO buffer will make sure that no user input is lost, but it also means that only one turn is performed per step.
    >-- fifoBounded 1000
    --> (game >-> arr renderPosition @@ gameClock)
      >-- keepLast mempty
    --> visualize
      @@ visualizationClock

main :: IO ()
-- Make sure to keep this definition here as it is: The tests depend on it.
main = flowGlossIO defaultSettings rhine
