module Domain.Services.PlayOneShot
  ( PlayOneShot(..)
  ) where

import Prelude

import Data.Either (Either)
import Domain.Exceptions.AppError (AppError)
import Domain.Values.Audio.Volume (Volume)
import Effect (Effect)
import Effect.Aff (Aff)
import Utils.Buffer (AudioBuffer)

type PlayOneShot = Volume -> AudioBuffer -> Effect (Aff (Either AppError Unit))
