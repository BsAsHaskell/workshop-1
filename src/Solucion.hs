{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Solucion where

import           GHC.Generics
import           Control.Monad           (forM_)
import           Data.Aeson              (FromJSON (..), Value (..), (.:))
import qualified Http
import           System.Environment      (getArgs)

data Actriz = Actriz
  { nombre :: String
  , fechaDeNacimiento :: String
  } deriving (Show)

instance FromJSON Actriz where
  parseJSON (Object objeto) = do
      nombre_     <- objeto .: "name"
      cumpleaños_ <- objeto .: "birthday"
      pure (Actriz nombre_ cumpleaños_)
  parseJSON _ = fail "No pude interpretar el objeto"

data Pelicula = Pelicula
  { title      :: String
  , overview   :: String
  , popularity :: Float
  } deriving (Generic)

instance FromJSON Pelicula

data Peliculas = Peliculas
  { peliculas :: [Pelicula]
  } deriving (Show)

instance FromJSON Peliculas where
  parseJSON (Object o) = do
    pelis <- o .: "results"
    pure (Peliculas pelis)
  parseJSON _ = fail "Error de parseo"

data Comando
  = BuscarPersona Int
  | BuscarPelicula Int
  | Buscar String
  deriving (Show)

run :: IO ()
run = do
  args <- getArgs
  case parseArgs args of
    BuscarPersona id' -> do
      persona <- Http.get (urlPersona id') :: IO Actriz
      print persona
    BuscarPelicula id' -> do
      peli <- Http.get (urlPelicula id') :: IO Pelicula
      print peli
    Buscar query -> do
      resultados <- Http.get (urlBuscar query) :: IO Peliculas
      print resultados

parseArgs :: [String] -> Comando
parseArgs ["persona", id']  = BuscarPersona (read id')
parseArgs ["pelicula", id'] = BuscarPelicula (read id')
parseArgs ["buscar", texto] = Buscar texto
parseArgs _                 = error "Uso: recomendar <id> | buscar <texto>"

instance Show Pelicula where
  show (Pelicula movieTitle movieOverview moviePopularity) =
            "\nTítulo: " <> movieTitle
         <> "\nPopularidad: " <> show moviePopularity
         <> "\nResumen: " <> movieOverview
         <> "\n"

baseURL :: String
baseURL = "http://api.themoviedb.org/3/"

urlBuscar :: String -> String
urlBuscar query = baseURL <> "search/movie?api_key=" <> key <> "&query=" <> query

urlPersona :: Int -> String
urlPersona id' = baseURL <> "person/" <> show id' <> "?api_key=" <> key

urlPelicula :: Int -> String
urlPelicula id' = baseURL <> "movie/" <> show id' <> "?api_key=" <> key

key :: String
key = "2ba61b38c35668c26d754910aac7a729"
