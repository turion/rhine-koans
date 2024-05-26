module Main where

-- base
import Control.Concurrent
import Control.Monad
import Data.IORef
import System.Exit

-- transformers
import Control.Monad.Trans.Reader

-- monad-schedule
import Control.Monad.Schedule.FreeAsync (runFreeAsyncT)

-- rhine-gloss
import FRP.Rhine.Gloss

-- koan
import Koan (rhine)

flattenPictures :: Picture -> [Picture]
flattenPictures (Pictures ps) = ps >>= flattenPictures
flattenPictures Blank = []
flattenPictures picture = [picture]

main :: IO ()
main = do
  vars <- liftIO $ GlossEnv <$> newEmptyMVar <*> newEmptyMVar <*> newIORef Blank <*> newIORef 0
  void $ forkIO $ runFreeAsyncT $ runReaderT (unGlossConcT (flow rhine)) vars
  putMVar (timeVar vars) 1
  putMVar (timeVar vars) 2
  threadDelay 100000
  pic <- readIORef (picRef vars)
  when (flattenPictures pic /= [rectangleSolid 1 1]) $ do
    print [rectangleSolid 1 1]
    print pic
    print $ flattenPictures pic
    exitFailure
