module TestIO where

-- base
import Control.Concurrent
import Control.Monad
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Exit
import System.IO (Handle, IOMode (..), hFlush, stderr, stdin, withFile)
import Prelude hiding (lines, putStrLn, unlines, writeFile)

-- text
import Data.Text
import Data.Text.IO

-- silently
import System.IO.Silently

-- temporary
import System.IO.Temp

-- | Integration test a main function
testForSeconds ::
  -- | How many seconds the test should run
  Int ->
  -- | The main function to test
  IO () ->
  -- | The property to test on the produced output ([] = test passes)
  ([Text] -> [Text]) ->
  IO ()
testForSeconds nSeconds mainFunction testFunction = testForSecondsErrHandle nSeconds mainFunction testFunction stderr

-- Like testForSeconds, but with custom stderr handle
testForSecondsErrHandle :: Int -> IO () -> ([Text] -> [Text]) -> Handle -> IO ()
testForSecondsErrHandle nSeconds mainFunction testFunction stderrOld = do
  putStrLn "---------------------------"
  output <- do
    output <- newEmptyMVar
    void $ forkIO (capture_ mainFunction >>= putMVar output)
    hPutStr stderrOld "Testing"
    forM_ [(1 :: Int) .. 20] $ const $ hPutStr stderrOld "." >> threadDelay (50000 * nSeconds) >> hFlush stderrOld
    readMVar output
  putStrLn "\n---------------------------\n"
  case testFunction $ lines $ pack output of
    [] -> putStrLn "Well done!"
    errors -> do
      putStrLn "Oh no!"
      forM_ errors putStrLn
      putStrLn ""
      exitFailure

-- | Integration test a main function, providing input
testForSecondsInput ::
  -- | How many seconds the test should run
  Int ->
  -- | The standard input to supply
  [Text] ->
  -- | The main function to test
  IO () ->
  -- | The property to test on the produced output ([] = test passes)
  ([Text] -> [Text]) ->
  IO ()
testForSecondsInput nSeconds input mainFunction testFunction = do
  inputFileName <- emptySystemTempFile "input.txt"
  writeFile inputFileName $ unlines input
  withFile inputFileName ReadMode $ \stdinFile -> do
    hDuplicateTo stdinFile stdin
    stderrOld <- hDuplicate stderr
    withSystemTempFile "stderr.txt" $ \_path stderrFile -> do
      hDuplicateTo stderrFile stderr -- silence stderr to avoid "hGetLine: end of file" message
      testForSecondsErrHandle nSeconds mainFunction testFunction stderrOld

-- | Like 'show', but for 'Text'
tshow :: (Show a) => a -> Text
tshow = pack . show
