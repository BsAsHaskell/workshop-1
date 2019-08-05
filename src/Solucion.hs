{-# LANGUAGE OverloadedStrings #-}
module FinalReddit where

import           Control.Monad           (forM_)
import           Data.Aeson              (FromJSON (..), Value (..), (.:))
import           Data.Text               (Text, pack, unpack)
import qualified Http
import           System.Environment      (getArgs)

data Movie = Movie
  { movieId         :: Int
  , movieTitle      :: Text
  , movieOverview   :: Text
  , moviePopularity :: Float
  } deriving (Show)

instance FromJSON Movie where
  parseJSON (Object o) = do
    id' <- o .: "id"
    title <- o .: "title"
    overview <- o .: "overview"
    popularity <- o .: "popularity"
    pure (Movie id' title overview popularity)
  parseJSON _ = fail "Error de parseo"

data Results = Results
  { resultsMovies :: [Movie]
  } deriving (Show)

instance FromJSON Results where
  parseJSON (Object o) = do
    movies <- o .: "results"
    pure (Results movies)
  parseJSON _ = fail "Error de parseo"

data Comando
  = Buscar Text
  | Recomendar Int
  deriving (Show)

run :: IO ()
run = do
  args <- getArgs
  case parseArgs args of
    Buscar query -> do
      resultados <- Http.get (searchURL query)
      imprimir resultados
    Recomendar id' -> do
      resultados <- Http.get (recommendURL id')
      imprimir resultados

parseArgs :: [String] -> Comando
parseArgs ["buscar", texto]   = Buscar (pack texto)
parseArgs ["recomendar", id'] = Recomendar (read id')
parseArgs _                   = error "Uso: recomendar <id> | buscar <texto>"

imprimir :: Results -> IO ()
imprimir (Results resultados) = forM_ resultados imprimirPeli
  where
    imprimirPeli (Movie id' titulo overview popularity) =
        (putStrLn . unpack)
        ("---\nID: " <> pack (show id')
         <> "\nTítulo: " <> titulo
         <> "\nPopularidad: " <> pack (show popularity)
         <> "\nResumen: " <> overview
         <> "\n")

searchURL :: Text -> Text
searchURL query = "http://api.themoviedb.org/3/search/movie?api_key=" <> key <> "&query=" <> query

recommendURL :: Int -> Text
recommendURL id' = "http://api.themoviedb.org/3/movie/" <> pack (show id') <> "/recommendations?api_key=" <> key

key :: Text
key = "2ba61b38c35668c26d754910aac7a729"
