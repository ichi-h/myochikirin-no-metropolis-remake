module Adapter.Audio.Channel
  ( changeVolume
  , pause
  , play
  , register
  , resume
  , stop
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn6, Fn5, runFn2, runFn6, runFn5)
import Data.Maybe (Maybe(..))
import Domain.Values.Audio.DelayMs (DelayMs(..))
import Domain.Values.Audio.FadeInMs (FadeInMs(..))
import Domain.Values.Audio.FadeOutMs (FadeOutMs(..))
import Domain.Values.Audio.OffsetMs (OffsetMs(..))
import Domain.Values.Audio.Samples (Samples(..))
import Domain.Values.Audio.Volume (Volume(..))
import Effect (Effect)
import Entities.Audio.Channel (ChangeVolume, PlayStatus(..), Channel(..), Loop(..), Pause, Play, Register, Stop, Resume)
import Promise (Promise)
import Promise.Aff (toAffE)

foreign import registerImpl :: Fn6 String ArrayBuffer Number Boolean Int Int (Effect (Promise Unit))
foreign import playImpl :: Fn5 String Int Int Int Int (Effect String)
foreign import stopImpl :: Fn2 String Int (Effect String)
foreign import pauseImpl :: Fn2 String Int (Effect String)
foreign import resumeImpl :: Fn2 String Int (Effect String)
foreign import changeVolumeImpl :: Fn2 String Number (Effect String)

register :: Register
register name buffer (Volume volume) loop = pure do
  let
    loopOps = case loop of
      Nothing -> { isLoop: false, start: 0, end: 0 }
      Just (Loop { start: (Samples start), end: (Samples end) }) -> { isLoop: true, start, end }
  res <- try $ toAffE $ (runFn6 registerImpl) name buffer volume loopOps.isLoop loopOps.start loopOps.end
  case res of
    Left err -> pure $ Left $ show err
    Right _ -> pure $ Right $ Channel { name, playStatus: Stopped, volume: Volume volume, loop }

play :: Play
play (DelayMs delayMs) (OffsetMs offsetMs) (FadeInMs fadeInMs) (FadeOutMs fadeOutMs) (Channel channel) = do
  res <- (runFn5 playImpl) channel.name delayMs offsetMs fadeInMs fadeOutMs
  if res == "" then
    pure $ Right $ Channel $ channel { playStatus = Playing }
  else
    pure $ Left res

stop :: Stop
stop (FadeOutMs fadeOutMs) (Channel channel) = do
  res <- (runFn2 stopImpl) channel.name fadeOutMs
  if res == "" then
    pure $ Right $ Channel $ channel { playStatus = Stopped }
  else
    pure $ Left res

pause :: Pause
pause (FadeOutMs fadeOutMs) (Channel channel) = do
  res <- (runFn2 pauseImpl) channel.name fadeOutMs
  if res == "" then
    pure $ Right $ Channel $ channel { playStatus = Paused }
  else
    pure $ Left res

resume :: Resume
resume (FadeInMs fadeInMs) (Channel channel) = do
  res <- (runFn2 resumeImpl) channel.name fadeInMs
  if res == "" then
    pure $ Right $ Channel $ channel { playStatus = Playing }
  else
    pure $ Left res

changeVolume :: ChangeVolume
changeVolume (Volume volume) (Channel channel) = do
  res <- (runFn2 changeVolumeImpl) channel.name volume
  if res == "" then
    pure $ Right $ Channel $ channel { volume = Volume volume }
  else
    pure $ Left res
