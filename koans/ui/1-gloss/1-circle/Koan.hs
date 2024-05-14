{- | Circle.

Let's draw something!
Rhine connects to the famous gloss library for 2d graphics.
Have a look at https://hackage.haskell.org/package/gloss to learn more about it!

The connection between Rhine and gloss is provided by the library https://hackage.haskell.org/package/rhine-gloss,
which encapsulates the effects of drawing pictures in gloss in a monad, 'GlossConcT',
and provides several clocks to interact with the gloss system.

To warm up, let's just draw a circle.
-}
module Koan where

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss

{- | The main 'Rhine' of this program.

                     /--- We use effects in 'GlossConc' to draw images.
                     |
                     |               /--- This clock ticks whenever an image is drawn on the screen by the gloss backend.
                     |               |
                     v               v
-}
rhine :: Rhine GlossConc GlossSimClockIO () ()
-- Can you create a solid circle of radius 10 here?
-- Have a look at https://hackage.haskell.org/package/gloss/docs/Graphics-Gloss-Data-Picture.html for inspiration.
rhine =
  constMCl (paintAllIO _) -- paintAllIO clears the drawing canvas and draws the given image
    @@ GlossSimClockIO -- The singleton value of GlossSimClockIO.

main :: IO ()
-- Make sure to keep this definition here as it is: The tests depend on it.
main =
  flowGlossIO -- This function can replace 'flow' when you're using the gloss backend.
    defaultSettings -- Settings for the gloss window context such as size, title, and background colour.
    rhine
