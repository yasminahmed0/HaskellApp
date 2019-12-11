module ApiRequest
    ( api_call
    ) where

import qualified Data.ByteString as B
import Network.HTTP.Simple

-- | function to extract data from API
api_call :: IO B.ByteString
api_call = do
   req <- parseRequest "https://footystats.org/c-dl.php?type=players&comp=1625"
   res <- httpBS req
   return $ getResponseBody res
