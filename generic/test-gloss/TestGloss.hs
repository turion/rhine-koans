module TestGloss where

-- base
import Control.Concurrent
import Control.Monad
import Data.IORef
import System.Exit

-- rhine-gloss
import FRP.Rhine.Gloss

expectPic :: Picture -> [Picture] -> IO ()
expectPic received expected = expectPics [received] [expected]

expectPics :: [Picture] -> [[Picture]] -> IO ()
expectPics receiveds expecteds = do
  forM_ (zip receiveds expecteds) $ \(received, expected) -> do
    let flattened = flattenPictures received
    when (flattened /= expected) $ do
      putStrLn $ "Expected: " ++ show expected
      putStrLn $ "Received: " ++ show flattened
      exitFailure
  putStrLn "Well done!"

flattenPictures :: Picture -> [Picture]
flattenPictures (Pictures ps) = ps >>= flattenPictures
flattenPictures Blank = []
flattenPictures picture = [picture]

stepGlossRhine :: (Clock GlossConc cl, Time cl ~ Time (Out cl), Time cl ~ Time (In cl), GetClockProxy cl) => Rhine GlossConc cl () () -> [Float] -> IO [Picture]
stepGlossRhine rhine timestamps = stepGlossRhineWithInput rhine timestamps []

stepGlossRhineWithInput :: (Clock GlossConc cl, Time cl ~ Time (Out cl), Time cl ~ Time (In cl), GetClockProxy cl) => Rhine GlossConc cl () () -> [Float] -> [Event] -> IO [Picture]
stepGlossRhineWithInput rhine timestamps events = do
  vars <- makeGlossEnv
  void $ forkIO $ forM_ events $ putMVar $ eventVar vars
  void $ forkIO $ runGlossConcT (flow rhine) vars
  forM timestamps $ \timestamp -> do
    putMVar (timeVar vars) timestamp
    threadDelay 33333
    readIORef (picRef vars)

specialKey :: SpecialKey -> Event
specialKey key = EventKey (SpecialKey key) Down (Modifiers Down Down Down) (0, 0)

keyRight :: Event
keyRight = specialKey KeyRight

keyLeft :: Event
keyLeft = specialKey KeyLeft
