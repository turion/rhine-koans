{-# LANGUAGE Arrows #-}

{- | Exceptions.

Have you ever tried to use the word count application you worked with in chapter 2 as a standalone console application?
If not, now is the time to do so!
How about counting the number of lines in the source code of one of the koans?
Try this (assuming you have a Unix-like system):

```
cat koans/basic/3/1-exceptions/solution/Koan.hs | cabal run basic-2-9-modularize
```

Unfortunately, this will throw an exception:

```
basic-2-9-modularize: <stdin>: hGetLine: end of file
```

The reason is that 'StdinClock' itself throws this 'IOError' as soon as the end of the file is reached.
The goal of this koan is to catch this error and handle it gracefully instead of letting it crash through.

Test it as a console application!

```
cat koans/basic/3/1-exceptions/solution/Koan.hs | cabal run basic-3-1-exceptions
```
-}
module Koan where

-- base
import Control.Exception qualified as Exception

-- text
import Data.Text qualified as Text (length, words)

-- rhine
import FRP.Rhine hiding (put)

type AppT = ExceptT IOError
type App = AppT IO

{- | 'StdinClock' lifted to the 'ExceptT' monad transformer.
                        /--- A clock is lifted to a monad transformer
                        |
                        |     /--- The base monad in which the clock originally ran
                        |     |
                        |     |   /--- The monad transformer applied to it
                        |     |   |
                        |     |   |     /--- The original clock
                        |     |   |     |
                        v     v   v     v
-}
type StdinWithEOF = LiftClock IO AppT StdinClock

-- | A 'StdinClock' that raises any 'IOError' algebraically in 'ExceptT'.
stdinWithEOF :: StdinWithEOF
stdinWithEOF =
  -- LiftClock is in fact a type synonym for HoistClock,
  -- which hoists a clock from one monad to another, using any monad morphism (not just lift).
  HoistClock
    { unhoistedClock = _
    , -- Your goal is to first catch the IOError, and then wrap it in the ExceptT transformer.
      monadMorphism = _
      -- Hint 1: Have a look at https://hackage.haskell.org/package/base/docs/Control-Exception-Base.html#v:try
      -- Hint 2: Have a look at https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Except.html
    }

-- | Count the number of lines, words and chars.
allCounts :: ClSF App StdinWithEOF () (Int, Int, Int)
allCounts = proc () -> do
  userInput <- tagS -< ()

  let wordCount = length $ Text.words userInput
      charCount = Text.length userInput + 1

  lineCount <- count @Int -< ()
  totalWordCount <- sumN -< wordCount
  totalCharCount <- sumN -< charCount
  returnA -< (lineCount, totalWordCount, totalCharCount)

-- | Print the three counts.
printCounts :: ClSF App StdinWithEOF (Int, Int, Int) ()
printCounts = proc (lineCount, totalWordCount, totalCharCount) -> do
  arrMCl $ liftIO . print -< lineCount
  arrMCl $ liftIO . print -< totalWordCount
  arrMCl $ liftIO . print -< totalCharCount

-- | On every 1000th line, print the number of total lines, words and characters so far.
printAllCounts :: ClSF App StdinWithEOF () ()
printAllCounts = proc () -> do
  counts@(lineCount, _, _) <- allCounts -< ()
  if lineCount `mod` 1000 == 0
    then printCounts -< counts
    else returnA -< ()

main :: IO ()
main = do
  -- The type is ambiguous because GHC cannot infer what e is.
  -- Give it a type signature to help it!
  Left e <- runExceptT $ flow $ printAllCounts @@ stdinWithEOF
  putStrLn $ "The following error occurred: " ++ show e
