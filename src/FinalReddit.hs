{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module FinalReddit
    ( server
    ) where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.String.Conversions
import System.Environment

-- Part 2
import Data.List (transpose)
import Control.Concurrent.Async (mapConcurrently)

-- Part 3
import Control.Monad.IO.Class
import Data.List.Split
import Data.Monoid

-- Part 1

data Post = Post
  { subreddit :: T.Text
  , author :: T.Text
  , score :: Int
  , url :: T.Text
  , title :: T.Text
  , thumbnail :: T.Text
  } deriving (Show)

data Listing = Listing { posts :: [Post] }
  deriving (Show)

getReddit :: String -> IO Listing
getReddit subreddit = do
  let url = "http://api.themoviedb.org/3/movie/550?api_key=2ba61b38c35668c26d754910aac7a729"
  response <- simpleHTTP (getRequest url)
  body <- getResponseBody response
  print body
  case eitherDecode (cs body) of
    Right listing -> pure listing
    Left e -> error e

-- Part 2

getReddits :: [String] -> IO Listing
getReddits reddits = do
  listings <- mapConcurrently getReddit reddits
  pure (mergeListings listings)

-- Technically can be any Foldable but this suffices for now.
mergeListings :: [Listing] -> Listing
mergeListings listings = Listing (interleaveMany (map posts listings))
  where interleaveMany = concat . transpose

printReddits :: IO ()
printReddits = do
  listing <- getReddits ["haskell", "darksouls"]
  print listing

-- Part 3 (Server)

server :: IO ()
server = do
  reddits <- getArgs
  case reddits of
    [] -> putStrLn "No Reddits Provided"
    _ -> do
      listing <- liftIO (getReddits reddits)
      print listing

instance FromJSON Post where
  parseJSON = withObject "post" $ \json -> do
    dataO <- json .: "data"
    subreddit_ <-  dataO .: "subreddit"
    author_ <- dataO .: "author"
    score_ <- dataO .: "score"
    url_ <- dataO .: "url"
    title_ <- dataO .: "title"
    thumbnail_ <- dataO .: "thumbnail"
    pure (Post subreddit_ author_ score_ url_ title_ thumbnail_)

instance FromJSON Listing where
  parseJSON = withObject "listing" $ \json -> do
    dataO <- json .: "data"
    children <- dataO .: "children"
    posts_ <- withArray "children" (mapM parseJSON . V.toList) children
    pure (Listing posts_)
