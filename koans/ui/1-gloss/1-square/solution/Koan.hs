{- | Square.

Let's draw something!
Rhine connects to the famous gloss library for 2d graphics.
-}
module Koan where

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss

-- | The main 'Rhine' of this program.
rhine :: Rhine (GlossConcT IO) GlossSimClockIO () ()
-- Can you create a solid square of length 1 here?
-- Have a look at https://hackage.haskell.org/package/gloss/docs/Graphics-Gloss-Data-Picture.html for inspiration.
rhine = constMCl (paintAllIO (rectangleSolid 1 1)) @@ GlossSimClockIO

-- Make sure to keep this definition here as it is: The tests depend on it.
main :: IO ()
main =
  flowGlossIO -- This function can replace 'flow' when you're using the gloss backend.
    defaultSettings -- Settings for the gloss window context such as size, title, and background colour.
    rhine
