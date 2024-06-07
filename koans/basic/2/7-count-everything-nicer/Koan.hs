{-# LANGUAGE CPP #-}

-- Disabling formatter and linter because it would fail on the syntax error otherwise.
#ifndef __HLINT__
{- FOURMOLU_DISABLE -}

-- Start reading here
-- vvvvvvvvvvvvvvvvvv

{- | Count everything nicer.

The last problem got quite verbose, and fiddling around with nested tuples isn't fun.
Fortunately, Haskell has a language extension that provides very useful syntax
for data flow constructs like signal functions!
It is called "arrow notation", and you can read a bit more about it here: https://www.haskell.org/arrows/.
Have a look how the code of the previous koan can be cleaned up with it.
-}
module Koan where

-- text
import Data.Text qualified as Text (length, words)

-- rhine
import FRP.Rhine hiding (currentInput)

-- | Print the number of total words and characters so far.
printAllCounts :: ClSF IO StdinClock () ()
-- proc is a keyword. Think of it like a lambda expression!
-- But why does GHC spit out a nasty parse error here?
-- Read through the following to find out!
printAllCounts = proc () -> do
  -- This is nearly like do notation, except it also has syntax for input, the -<.

  -- /------/--- Everything left from a <- is the output _signal_ of a signal function.
  -- /|     |    It is a value that can depend on the current tick of the clock.
  -- /|     |
  -- /|     |   /--- Signal functions can be used between <- and -<.
  -- /|     |   |
  -- /|     |   |       /--- This is the input to the signal function. (tagS needs none.)
  -- /|     |   |       |
  --  v     v   v       v
  userInput <- tagS -< ()

  -- We can apply ordinary functions to signals.
  let wordCount = length $ Text.words userInput
      charCount = Text.length userInput + 1

  lineCount <- count @Int -< ()

  -- Signals can be inputs to signal functions.
  -- This way we can aggregate signals.
  totalWordCount <- sumN -< wordCount
  totalCharCount <- sumN -< charCount

  -- If a signal function has trivial output (), the <- is not needed.
  arrMCl print -< lineCount
  arrMCl print -< totalWordCount
  arrMCl print -< _ -- Which one is missing here?

-- As you've seen, arrow notation introduces two new syntactic constructions,
-- the proc keyword an the -< operator.
-- You need to turn on a GHC language extension so that they can be parsed!
-- Can you uncomment the following line, and move to the top of the file?
-- {-# LANGUAGE Arrows #-}

main :: IO ()
main = flow $ printAllCounts @@ StdinClock

-- Ignore the next line ;)
#endif
