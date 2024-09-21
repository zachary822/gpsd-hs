{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.GPSD.Types where

import Data.Aeson
import Data.Text
import Data.Time.Clock
import GHC.Generics (Generic)

data Satellite = Satellite
  { prn :: Int
  , el :: Double
  , az :: Double
  , ss :: Double
  , used :: Bool
  , gnssid :: Int
  , svid :: Int
  }
  deriving (Generic, Show, Eq)

instance FromJSON Satellite where
  parseJSON = withObject "Satellite" $ \o ->
    Satellite
      <$> o .: "PRN"
      <*> o .: "el"
      <*> o .: "az"
      <*> o .: "ss"
      <*> o .: "used"
      <*> o .: "gnssid"
      <*> o .: "svid"

data Device = Device
  { path :: Text
  , driver :: Text
  , subtype :: Text
  , activated :: UTCTime
  , flags :: Int
  , native :: Int
  , bps :: Int
  , parity :: Text
  , stopbits :: Int
  , cycle :: Double
  , mincycle :: Double
  }
  deriving (Generic, Show, Eq, FromJSON)

data Info
  = VersionInfo
      { release :: Text
      , rev :: Text
      , proto_major :: Int
      , proto_minor :: Int
      }
  | DevicesInfo
      { devices :: [Device]
      }
  | TPVInfo
      { device :: Text
      , status :: Int
      , mode :: Int
      , time :: UTCTime
      , ept :: Double
      , lat :: Double
      , lon :: Double
      , altHAE :: Double
      , altMSL :: Double
      , alt :: Double
      , epx :: Double
      , epy :: Double
      , epv :: Double
      , track :: Double
      , magtrack :: Double
      , magvar :: Double
      , speed :: Double
      , climb :: Double
      , eps :: Double
      , epc :: Double
      , geoidSep :: Double
      , eph :: Double
      , sep :: Double
      }
  | SkyInfo
      { device :: Text
      , xdop :: Double
      , ydop :: Double
      , vdop :: Double
      , tdop :: Double
      , hdop :: Double
      , gdop :: Double
      , pdop :: Double
      , nSat :: Int
      , uSat :: Int
      , satellites :: [Satellite]
      }
  deriving (Generic, Show, Eq)

options :: Options
options =
  defaultOptions
    { sumEncoding = UntaggedValue
    }

instance FromJSON Info where
  parseJSON = genericParseJSON options
