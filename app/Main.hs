{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception qualified as E
import Control.Monad
import Control.Monad.State.Strict
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.GPSD.Types
import Data.List.NonEmpty qualified as NE
import Network.Socket
import Network.Socket.ByteString

main :: IO ()
main = do
  runGPSDClient "raspicam.local" "2947" print

getRow :: Socket -> StateT ByteString IO [Maybe Info]
getRow s = do
  buf <- get
  msg <- liftIO $ recv s 4096
  let msgs = B.split 10 (buf <> msg)
  put (last msgs)
  return (map (decode . B.fromStrict) (init msgs))

processMessage :: (Maybe Info -> IO ()) -> Socket -> IO ()
processMessage f s = do
  msg <- recv s 4096
  f (decode (B.fromStrict msg))
  sendAll s "?WATCH={\"enable\":true,\"json\":true}"
  void $
    runStateT
      ( forever $ do
          msgs <- getRow s
          liftIO $ forM msgs f
      )
      ""

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = do
  addr <- resolve
  E.bracket (open addr) close client
 where
  resolve = do
    let hints = defaultHints{addrSocketType = Stream}
    NE.head <$> getAddrInfo (Just hints) (Just host) (Just port)
  open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    connect sock $ addrAddress addr
    return sock

runGPSDClient :: HostName -> ServiceName -> (Maybe Info -> IO ()) -> IO ()
runGPSDClient host port f =
  runTCPClient host port $ processMessage f
