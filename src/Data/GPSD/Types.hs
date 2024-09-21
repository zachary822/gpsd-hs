{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GPSD.Types where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)

data Class = Version | Devices | Watch | Sky | TPV
  deriving (Generic, Show, Eq)

instance FromJSON Class where
  parseJSON = withText "Tag" $ \case
    "VERSION" -> return Version
    "DEVICES" -> return Devices
    "WATCH" -> return Watch
    "SKY" -> return Sky
    "TPV" -> return TPV
    _ -> fail "invalid class"

data Info
  = VersionInfo
      { class_ :: Class
      , release :: Text
      , rev :: Text
      , protoMajor :: Integer
      , protoMinor :: Integer
      }
  | DevicesInfo
      { class_ :: Class
      }
  deriving (Generic, Show, Eq)

-- TODO: parse data correctly
instance FromJSON Info where
  parseJSON = withObject "Info" $ \o -> do
    cl :: Class <- o .: "class"
    case cl of
      Version ->
        VersionInfo cl
          <$> o .: "release"
          <*> o .: "rev"
          <*> o .: "proto_major"
          <*> o .: "proto_minor"
      Devices -> return DevicesInfo{class_ = cl}
      _ -> return DevicesInfo{class_ = cl}
