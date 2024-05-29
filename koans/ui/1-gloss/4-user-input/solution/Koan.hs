{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Grid.
module Koan where

-- base
import Data.List.NonEmpty hiding (insert, unfold)
import Data.Maybe (fromMaybe)
import GHC.Generics

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss

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
  = -- | Don't change the direction. This happens if no key is pressed.
    Stay
  | -- | Turn right (clockwise) when the right arrow is pressed.
    TurnRight
  | -- | Turn left (counterclockwise) when the left arrow is pressed.
    TurnLeft
  deriving (Show)

changeDirection :: Turn -> Direction -> Direction
changeDirection Stay direction = direction
changeDirection TurnRight direction = toEnum $ (fromEnum direction - 1) `mod` 4
changeDirection TurnLeft direction = toEnum $ (fromEnum direction + 1) `mod` 4

type GameClock = GlossConcTClock IO (Millisecond 500)

gameClock :: GameClock
gameClock = glossConcTClock waitClock

game :: ClSF GlossConc GameClock Turn Position
game = unfold (mempty, North) $ \turn (position, direction) ->
  let direction' = changeDirection turn direction
      position' = stepPosition direction' position
   in Result (position', direction') position'

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

rhine = user @@ userClock >-- fifoBounded 1000 --> (arr (fromMaybe Stay) >-> game >-> arr renderPosition @@ gameClock) >-- keepLast mempty --> visualize @@ visualizationClock

main :: IO ()
-- Make sure to keep this definition here as it is: The tests depend on it.
main = flowGlossIO defaultSettings rhine
