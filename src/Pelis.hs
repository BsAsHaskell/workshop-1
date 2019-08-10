{-# LANGUAGE OverloadedStrings #-}
module Pelis where

import           Control.Monad           (forM_)
import           Data.Aeson              (FromJSON (..), Value (..), (.:))
import           Data.Text               (Text, pack, unpack)
import qualified Http
import           System.Environment      (getArgs)

run :: IO ()
run = error "Not implemented...yet!"
