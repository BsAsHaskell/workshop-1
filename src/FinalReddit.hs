{-# LANGUAGE OverloadedStrings #-}

module FinalReddit
  ( run
  ) where

import           Control.Monad.IO.Class
import           Data.Aeson              (FromJSON (..), Value (..),
                                          eitherDecode, withObject, (.:))
import           Data.Default.Class      (def)

import           Data.Monoid             (mempty)
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import qualified Network.HTTP.Req        as Req
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
    Buscar query   -> llamarAPI (searchURL (convertString query))
    Recomendar id' -> llamarAPI (recommendURL id')

llamarAPI :: Text -> IO ()
llamarAPI url =
  Req.runReq def $ do
    r <- Req.req Req.GET (Req.http url) Req.NoReqBody Req.jsonResponse mempty
    liftIO $ print $ (Req.responseBody r :: Value)

--  putStrLn (convertString (mostrar resultado))
mostrar :: Results -> String
mostrar = convertString . pShow

parseArgs :: [String] -> Comando
parseArgs ["buscar", texto]  = Buscar texto
parseArgs ["recomendar", id] = Recomendar (read id)
parseArgs _                  = error "No funciona asi papááááááa"

searchURL :: Text -> Text
searchURL query = "api.themoviedb.org/3/search/movie?api_key=" <> key <> "&query=" <> query

recommendURL :: Int -> Text
recommendURL id' = "api.themoviedb.org/3/movie/" <> (convertString (show id')) <> "/recommendations?api_key=" <> key

key :: Text
key = "2ba61b38c35668c26d754910aac7a729"
