{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.GPSD

main :: IO ()
main = do
  runGPSDClient "raspicam.local" "2947" print
