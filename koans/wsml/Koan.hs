module Koan where

import Wuss

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, unless, void)
import Data.Text (Text, pack)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)

main :: IO ()
-- main = runSecureClient "echo.websocket.org" 443 "/" ws
main = runSecureClient "www.seismicportal.eu" 443 "/standing_order/websocket" ws
-- main = runSecureClient "www.seismicportal.eu" 443 "/" ws -- try again at home
-- main = runSecureClient "api2.poloniex.com" 443 "/" ws

ws :: ClientApp ()
ws connection = do
  putStrLn "Connected!"

  void . forkIO . forever $ do
    message <- receiveData connection
    print (message :: Text)

    threadDelay $ 1000 * 1000000

  sendClose connection (pack "Bye!")
