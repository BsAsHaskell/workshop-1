{-# LANGUAGE OverloadedStrings   #-}

module FinalReddit
  ( run
  ) where

import           Data.Aeson              (FromJSON (..), eitherDecode,
                                          withObject, (.:))
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import           Network.HTTP            (getRequest, getResponseBody,
                                          simpleHTTP)
import           System.Environment      (getArgs)
import           Text.Pretty.Simple      (pShow)

data Movie = Movie
  { movieId         :: Int
  , movieTitle      :: Text
  , movieOverview   :: Text
  , moviePopularity :: Float
  } deriving (Show)

instance FromJSON Movie where
  parseJSON = withObject "Movie" $ \r -> Movie <$> r .: "id" <*> r .: "title" <*> r .: "overview" <*> r .: "popularity"

newtype Results = Results
  { results :: [Movie]
  } deriving (Show)

instance FromJSON Results where
  parseJSON = withObject "Results" $ \o -> Results <$> o .: "results"

data Comando
  = Buscar String
  | Recomendar Int
  deriving (Show)

run :: IO ()
run = do
  args <- getArgs
  case parseArgs args of
    Buscar query   -> llamarAPI (searchURL query)
    Recomendar id' -> llamarAPI (recommendURL id')

llamarAPI :: String -> IO ()
llamarAPI url = do
  req <- simpleHTTP $ getRequest url
  body <- getResponseBody req
  putStrLn $
    case eitherDecode (convertString body) of
      Left e  -> e
      Right x -> mostrar x

mostrar :: Results -> String
mostrar = convertString . pShow

parseArgs :: [String] -> Comando
parseArgs ["buscar", texto]  = Buscar texto
parseArgs ["recomendar", id] = Recomendar (read id)
parseArgs _                  = error "No funciona asi papááááááa"

searchURL :: String -> String
searchURL query = "http://api.themoviedb.org/3/search/movie?api_key=" <> key <> "&query=" <> query

recommendURL :: Int -> String
recommendURL id' = "http://api.themoviedb.org/3/movie/" <> show id' <> "/recommendations?api_key=" <> key

key :: String
key = "2ba61b38c35668c26d754910aac7a729"
