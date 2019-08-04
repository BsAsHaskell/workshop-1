module Http
  ( get
  ) where

import           Data.Aeson              (FromJSON)
import           Data.Default.Class      (def)
import           Data.Maybe              (fromMaybe)
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import           Network.HTTP.Req

get :: FromJSON response => Text -> IO response
get url = do
  let (url', scheme) = fromMaybe (error "Malformed URL") (parseUrlHttp (convertString url))
  r <- runReq def $ req GET url' NoReqBody jsonResponse scheme
  pure (responseBody r)
