{-# LANGUAGE OverloadedStrings #-}

module FinalReddit
  ( run
  ) where

import           Control.Monad           (forM_)
import           Control.Monad.IO.Class
import           Data.Aeson              (FromJSON (..), Value (..),
                                          eitherDecode, withObject, (.:))
import           Data.Monoid             (mempty)
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import           Http                    (get)
import           System.Environment      (getArgs)

data Movie = Movie
  { movieId         :: Int
  , movieTitle      :: Text
  , movieOverview   :: Text
  , moviePopularity :: Float
  } deriving (Show)

instance FromJSON Movie where
  parseJSON (Object o) = do
    id_ <- o .: "id"
    title_ <- o .: "title"
    overview_ <- o .: "overview"
    popularity_ <- o .: "popularity"
    pure (Movie id_ title_ overview_ popularity_)
  parseJSON _ = fail "Llel"

data Results = Results
  { results :: [Movie]
  } deriving (Show)

instance FromJSON Results where
  parseJSON value = withObject "Results" (\o -> do
    results_ <- o .: "results"
    pure (Results results_)) value
  parseJSON _ = fail "Lel"

data Comando
  = Buscar String
  | Recomendar Int
  deriving (Show)

run :: IO ()
run = do
  args <- getArgs
  case parseArgs args of
    Buscar query   -> do
      resultados <- llamarAPI (searchURL (convertString query))
      imprimir resultados
    Recomendar id' -> do
      resultados <- llamarAPI (recommendURL id')
      imprimir resultados

imprimir :: Results -> IO ()
imprimir (Results resultados) = do
  forM_ resultados imprimirPeli
 where
  imprimirPeli (Movie id_ titulo_ overview_ _) = putStrLn (convertString (titulo_ <> ": " <> convertString (show id_) <> "\n" <> overview_))

llamarAPI :: Text -> IO Results
llamarAPI url = Http.get url

parseArgs :: [String] -> Comando
parseArgs ["buscar", texto]  = Buscar texto
parseArgs ["recomendar", id] = Recomendar (read id)
parseArgs _                  = error "No funciona asi papááááááa"

searchURL :: Text -> Text
searchURL query = "http://api.themoviedb.org/3/search/movie?api_key=" <> key <> "&query=" <> query

recommendURL :: Int -> Text
recommendURL id' = "http://api.themoviedb.org/3/movie/" <> convertString (show id') <> "/recommendations?api_key=" <> key

key :: Text
key = "2ba61b38c35668c26d754910aac7a729"
