module Main where

-- base
import Control.Concurrent
import Control.Monad
import System.Exit
import System.IO

-- silently
import System.IO.Silently

-- rhine-koan
import qualified Koan1

main = do
  putStrLn "---------------------------"
  void $ forkIO Koan1.main
  output <- capture_ $ do
    hPutStr stderr "Testing"
    forM_ [1..20] $ const $ hPutStr stderr "." >> threadDelay 100000 >> hFlush stderr
  putStrLn "\n---------------------------"
  if length (filter (== "Hello Rhine!") (lines output)) `elem` [1,2]
    then putStrLn "Well done!"
    else putStrLn "Oh no!" >> exitFailure
