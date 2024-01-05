module TestIO where

-- base
import Control.Concurrent
import Control.Monad
import System.Exit
import System.IO

-- silently
import System.IO.Silently

-- | Integration test a main function
testForSeconds ::
  -- | How many seconds the test should run
  Int ->
  -- | The standard input to supply
  Maybe [String] ->
  -- | The main function to test
  IO () ->
  -- | The property to test on the produced output ([] = test passes)
  ([String] -> [String]) ->
  IO ()
testForSeconds nSeconds _input mainFunction testFunction = do
  putStrLn "---------------------------"
  void $ forkIO mainFunction
  output <- capture_ $ do
    hPutStr stderr "Testing"
    forM_ [(1 :: Int) .. 20] $ const $ hPutStr stderr "." >> threadDelay (50000 * nSeconds) >> hFlush stderr
  putStrLn "\n---------------------------\n"
  case testFunction $ lines output of
    [] -> putStrLn "Well done!"
    errors -> do
      putStrLn "Oh no!"
      forM_ errors putStrLn
      putStrLn ""
      exitFailure
