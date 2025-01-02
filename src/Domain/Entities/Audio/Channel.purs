module Entities.Audio.Channel
  ( ChangeVolume
  , Channel(..)
  , Loop(..)
  , Pause
  , Play
  , PlayStatus(..)
  , Register
  , Resume
  , Stop
  ) where

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Domain.Values.Audio.DelayMs (DelayMs)
import Domain.Values.Audio.FadeInMs (FadeInMs)
import Domain.Values.Audio.FadeOutMs (FadeOutMs)
import Domain.Values.Audio.OffsetMs (OffsetMs)
import Domain.Values.Audio.Samples (Samples)
import Domain.Values.Audio.Volume (Volume)
import Effect (Effect)
import Effect.Aff (Aff)

data PlayStatus = Playing | Stopped | Paused
newtype Loop = Loop { start :: Samples, end :: Samples }

newtype Channel = Channel
  { name :: String
  , playStatus :: PlayStatus
  , volume :: Volume
  , loop :: Maybe Loop
  }

type Register = String -> ArrayBuffer -> Volume -> Maybe Loop -> Effect (Aff (Either String Channel))
type Play = DelayMs -> OffsetMs -> FadeInMs -> FadeOutMs -> Channel -> Effect (Either String Channel)
type Stop = FadeOutMs -> Channel -> Effect (Either String Channel)
type Pause = FadeOutMs -> Channel -> Effect (Either String Channel)
type Resume = FadeInMs -> Channel -> Effect (Either String Channel)
type ChangeVolume = Volume -> Channel -> Effect (Either String Channel)
