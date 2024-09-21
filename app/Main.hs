{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.GPSD
import Network.Socket
import Options.Applicative

data Config = Config
  { host :: HostName
  , port :: ServiceName
  }
  deriving (Show, Eq)

configParser :: Parser Config
configParser =
  Config
    <$> strArgument (metavar "HOSTNAME")
    <*> strOption (long "port" <> short 'p' <> metavar "PORT" <> value "2947")

main :: IO ()
main = do
  config <- execParser opts
  runGPSDClient (host config) (port config) print
 where
  opts =
    info (configParser <**> helper) (fullDesc <> progDesc "connect to gpsd server")
