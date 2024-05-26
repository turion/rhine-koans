{- | Move.


-}
module Koan where

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss

-- | The main 'Rhine' of this program.
rhine :: Rhine (GlossConcT IO) GlossSimClockIO () ()
rhine = sinceInitS >-> arrMCl (\t -> translate 0 t $ paintAllIO (circleSolid 0.6)) @@ GlossSimClockIO

main :: IO ()
-- Make sure to keep this definition here as it is: The tests depend on it.
main = flowGlossIO defaultSettings rhine
