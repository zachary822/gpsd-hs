{-# LANGUAGE OverloadedStrings #-}

module Network.GPSD where

import Control.Exception qualified as E
import Control.Monad
import Control.Monad.State.Strict
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Network.GPSD.Types
import Network.Socket
import Network.Socket.ByteString

getRow :: Socket -> StateT ByteString IO [Info]
getRow s = do
  buf <- get
  msg <- liftIO $ recv s 4096
  case unsnoc (B.split 10 (buf <> msg)) of
    Just (xs, l) -> do
      put l
      return $ catMaybes (map (decode . B.fromStrict) xs)
    Nothing -> return []

processMessage :: (Info -> IO ()) -> Socket -> IO ()
processMessage f s = do
  msg <- recv s 4096
  case decode (B.fromStrict msg) of
    Just info -> do
      f info
      sendAll s "?WATCH={\"enable\":true,\"json\":true}"

      evalStateT
        ( forever $ do
            msgs <- getRow s
            liftIO $ traverse f msgs
        )
        ""
    Nothing -> fail "fail to initialize connection"

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

runGPSDClient :: HostName -> ServiceName -> (Info -> IO ()) -> IO ()
runGPSDClient host port = runTCPClient host port . processMessage
