module Adapter.Audio.PlayOneShot
  ( playOneShot
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Domain.Exceptions.AppError (AppError(..), mapError)
import Domain.Services.PlayOneShot (PlayOneShot)
import Domain.Values.Audio.Volume (Volume(..))
import Effect (Effect)
import Promise (Promise)
import Promise.Aff (toAffE)

foreign import playOneShotImpl :: Fn2 Number ArrayBuffer (Effect (Promise String))

playOneShot :: PlayOneShot
playOneShot (Volume volume) source = pure do
  res <- map (mapError (AudioError <<< show)) $ try $ toAffE $ (runFn2 playOneShotImpl) volume source
  case res of
    Left err -> pure $ Left err
    Right _ -> pure $ Right unit
