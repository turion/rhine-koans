{-# LANGUAGE Arrows #-}
{-# LANGUAGE StrictData #-}

{- | Asynchronize.

Our word count application is somewhat usable now,
but so far it was forced to be single threaded because of the use of 'StateT'.
Adding further clocks to it is not possible in a general state monad.

Luckily, there are other monads that can be scheduled concurrently in Rhine
which give us all the capability we need.
To track the total number of lines, words and characters,
we keep a central accumulation state which we update for every new line with an increment.
Such a state is modelled well as a monoid, so we can use the 'AccumT' monad transformer for it!
In case you haven't heard of it yet, now is a good time to familiarise yourself with it:
https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Accum.html

The advantage of 'AccumT' is that it is an instance of 'MonadSchedule',
so we can use several clocks in this monad and schedule them.
Let's use this to change the word count application slightly such that it doesn't output a running count on every 1000 lines,
but instead every second! This means adding a Millisecond 1000 clock, and printing on every tick of it.
-}
module Koan where

-- base
import Control.Exception qualified as Exception

-- transformers
import Control.Monad.Trans.Accum (AccumT (..), add, look, runAccumT)
import Control.Monad.Trans.Class (MonadTrans (lift))

-- text
import Data.Text qualified as Text (length, words)

-- rhine
import FRP.Rhine hiding (add)

{- | A count of chars, words and lines.

This is the central state of our application.
-}
data WordCount = WordCount
  { nChars :: Int
  , nWords :: Int
  , nLines :: Int
  }
  deriving (Show, Read)

instance Semigroup WordCount where
  WordCount c1 w1 l1 <> WordCount c2 w2 l2 =
    WordCount
      { nChars = c1 + c2
      , nWords = w1 + w2
      , nLines = l1 + l2
      }

instance Monoid WordCount where
  mempty = WordCount 0 0 0

-- | The application monad with exceptions and accumulation state
type App = ExceptT IOError (AccumT WordCount IO)

type StdinWithEOF = HoistClock IO App StdinClock

stdinWithEOF :: StdinWithEOF
stdinWithEOF =
  HoistClock
    { unhoistedClock = StdinClock
    , monadMorphism = ExceptT . lift . Exception.try
    }

-- | Count the number of lines, words and chars.
allCounts :: ClSF App StdinWithEOF () ()
allCounts = proc () -> do
  userInput <- tagS -< ()

  -- Caution: In AccumT, we only add increments to the state, we don't set the whole state.
  let nChars = _
      nWords = _
      nLines = _

  -- Have a look at https://hackage.haskell.org/package/transformers-0.6.1.0/docs/Control-Monad-Trans-Accum.html#g:3
  -- which operation is used to add an increment to the state.
  arrMCl $ lift . _
    -<
      WordCount
        { nLines
        , nWords
        , nChars
        }

-- | Print the three counts.
printCounts :: ClSF App (IOClock App (Millisecond 1000)) () ()
printCounts = proc () -> do
  -- To understand the runtime behaviour better, let's also output the absolute time and the time since clock initialisation.
  -- These are part of the TimeInfo which is always available in a ClSF.
  -- See https://hackage.haskell.org/package/rhine/docs/FRP-Rhine-Clock.html#t:TimeInfo for details.
  -- Can you match on the corresponding record fields and print them?
  TimeInfo {} <- timeInfo -< ()
  arrMCl $ liftIO . print -< _
  arrMCl $ liftIO . print -< _

  -- Have a look at https://hackage.haskell.org/package/transformers-0.6.1.0/docs/Control-Monad-Trans-Accum.html#g:3
  -- which operation is used to look up the current state.
  counts <- constMCl $ lift _ -< ()
  arrMCl $ liftIO . print -< counts

main :: IO ()
main = do
  (Left (_ :: IOError), result :: WordCount) <-
    flip runAccumT mempty $
      runExceptT $
        flow $
          -- The |@| operator combines two Rhines parallely in time.
          -- They will be executed concurrently, but share the same monad.
          -- Our App monad is an instance of MonadIO.
          -- Have a look in https://hackage.haskell.org/package/rhine/docs/FRP-Rhine-Clock.html
          -- for a function that lifts waitClock to an arbitrary MonadIO.
          allCounts @@ stdinWithEOF |@| printCounts @@ _ waitClock
  putStrLn $ "Final result: " ++ show result
