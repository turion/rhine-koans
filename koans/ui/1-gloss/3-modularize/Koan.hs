{- | Modularize.

To make a round-based game, we need to encode the rounds in some way.
The most natural way to do this in Rhine is to define a separate clock where each tick corresponds to one round!

Let's do this here.
For the rest of this track, we will just assume that a round lasts half a second.
So we should use a @'Millisecond' 500@ clock!

The devil is in the details, though.
We now have two different components, the game clock and the visualization clock.
But they run on different monads and time domains!
You will have to translate between them in order to make everything flow together.
-}
module Koan where

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss

-- * Game logic

{- | A circle that moves upwards by 10 pixels every second.

Its type signature ensures that it will be run on the 'GameClock'.
-}
movingCircle :: ClSF GlossConc GameClock () Picture
-- The cryptic type error wants to tell us that the time since clock initialisation is in Double, but gloss expects a Float!
-- Can you convert one to the other?
movingCircle = sinceInitS >-> arr (\t -> translate 0 (10 * t) $ circleSolid 10) -- realToFrac works as well!

-- | A clock that ticks at every round of the game.
type GameClock =
  -- Actually we just want a Millisecond 500 clock, but that is in the 'IO' monad,
  -- while the gloss backend expects a particular monad, 'GlossConcT'.
  -- Luckily there is also has a utility to lift any 'IO' clock to it!
  -- Have a look at https://hackage.haskell.org/package/rhine-gloss/docs/FRP-Rhine-Gloss-IO.html.
  _ _ (Millisecond 500)

gameClock :: GameClock
-- The clock type lifting function from above also has a corresponding value level function!
gameClock = _ waitClock

-- * Visualization

-- | Paint a gloss picture
visualize :: BehaviourF GlossConc UTCTime Picture ()
visualize = arrMCl paintAllIO

-- | Draw at 30 FPS
type VisualizationClock = _ _ GlossSimClockIO

visualizationClock :: VisualizationClock
visualizationClock = _ GlossSimClockIO

-- Find the right resampling buffer to transport the rendered image from the game clock to the visualization clock.
-- It should have two properties:
-- 1. It should always output the newest image.
-- 2. At startup, before the first round of the game has started, a blank image should be displayed.
rhine = movingCircle @@ gameClock >-- _ blank --> visualize @@ visualizationClock

main :: IO ()
-- Make sure to keep this definition here as it is: The tests depend on it.
main = flowGlossIO defaultSettings rhine
