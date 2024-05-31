module TestGloss where

-- base
import Control.Concurrent
import Control.Monad
import Data.IORef
import System.Exit

-- rhine-gloss
import FRP.Rhine.Gloss

expectPic :: Picture -> [Picture] -> IO ()
expectPic received expected =
  let flattened = flattenPictures received
   in if flattened == expected
        then putStrLn "Well done!"
        else do
          putStrLn $ "Expected: " ++ show expected
          putStrLn $ "Received: " ++ show flattened
          exitFailure

flattenPictures :: Picture -> [Picture]
flattenPictures (Pictures ps) = ps >>= flattenPictures
flattenPictures Blank = []
flattenPictures picture = [picture]

stepGlossRhine :: Rhine (GlossConcT IO) GlossSimClockIO () () -> [Float] -> IO [Picture]
stepGlossRhine rhine timestamps = do
  vars <- makeGlossEnv
  void $ forkIO $ runGlossConcT (flow rhine) vars
  forM timestamps $ \timestamp -> do
    putMVar (timeVar vars) timestamp
    threadDelay 100000
    readIORef (picRef vars)
