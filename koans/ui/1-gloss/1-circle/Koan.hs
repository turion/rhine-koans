module Koan where

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss

rhine :: Rhine (GlossConcT IO) GlossSimClockIO () ()
rhine = _

-- Make sure to keep this definition here as it is: The tests depend on it.
main :: IO ()
main = flowGlossIO defaultSettings rhine
