{- | Move.

One central idea of Functional Reactive Animation (Conal Elliot & Paul Hudak, ICFP 1997)
is that an animation is a picture parametrised by time.
This idea is continued in Yampa and Rhine by providing knowledge of time as a builtin effect,
which can then be used to parametrise everything the program does.

In Rhine, one way to access time is to use ['sinceInitS'](https://hackage.haskell.org/package/rhine/docs/FRP-Rhine-ClSF-Util.html#v:sinceInitS),
which outputs the time since clock initialisation (which happens at the beginning of 'flow').
You can use this time in the definition of all gloss pictures, and they'll start moving!
-}
module Koan where

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss

-- | The main 'Rhine' of this program.
rhine :: Rhine (GlossConcT IO) GlossSimClockIO () ()
-- Somehow the order of these functions is wrong. Can you fix it?
rhine = sinceInitS >-> arrMCl (\t -> translate 0 (10 * t) $ paintAllIO $ circleSolid 10) @@ GlossSimClockIO

main :: IO ()
-- Make sure to keep this definition here as it is: The tests depend on it.
main = flowGlossIO defaultSettings rhine
