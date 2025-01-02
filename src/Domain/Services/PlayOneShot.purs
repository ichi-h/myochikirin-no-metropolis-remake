module Domain.Services.PlayOneShot
  ( PlayOneShot(..)
  ) where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either)
import Domain.Exceptions.AppError (AppError)
import Domain.Values.Audio.Volume (Volume)
import Effect (Effect)
import Effect.Aff (Aff)

type PlayOneShot = Volume -> ArrayBuffer -> Effect (Aff (Either AppError Unit))
