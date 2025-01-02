module Domain.Services.Fetch where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Domain.Exceptions.AppError (AppError(..))
import Effect.Aff (Aff)

fetchAudio :: String -> Aff (Either AppError ArrayBuffer)
fetchAudio url = do
  response <- AX.get AXRF.arrayBuffer url
  case response of
    Left e -> pure $ Left $ FetchError e
    Right a -> pure $ Right a.body
