module UI.AppM where

import Prelude

import Adapter.Audio.Channel (play, register)
import Adapter.Audio.PlayOneShot (playOneShot)
import Control.Monad.Except (ExceptT(..))
import Data.Array (find)
import Data.Maybe (Maybe(..))
import Domain.Values.Audio.DelayMs (DelayMs(..))
import Domain.Values.Audio.FadeInMs (FadeInMs(..))
import Domain.Values.Audio.FadeOutMs (FadeOutMs(..))
import Domain.Values.Audio.OffsetMs (OffsetMs(..))
import Domain.Values.Audio.Samples (Samples(..), maxSamples)
import Domain.Values.Audio.Volume (Volume(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Entities.Audio.Channel (Loop(..), Channel(..))
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Safe.Coerce (coerce)
import UI.Capabilities.Audio (toUrl, toUrlSE)
import UI.Capabilities.Audio as Audio
import UI.Store as Store
import Utils.Buffer (array2AudioBuffer)
import Utils.Fetch (fetchBinary)
import Utils.Logger (runExceptTWithLog)

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reducer <<< coerce

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadStoreAppM :: MonadStore Store.Action Store.Store AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance audioAppM :: Audio.Audio AppM where
  playBGM bgm = void $ runExceptTWithLog do
    let url = toUrl bgm
    buffer <- ExceptT $ liftAff $ fetchBinary url
    asyncOperation <- liftEffect $ register "channel" buffer (Volume 1.0) (Just $ Loop { start: Samples 222966, end: maxSamples })
    c1 <- ExceptT $ liftAff $ asyncOperation
    c2 <- ExceptT $ liftEffect $ play (DelayMs 0) (OffsetMs 0) (FadeInMs 0) (FadeOutMs 0) c1
    updateStore $ Store.UpdateChannel c2

  playSE se =
    let
      getAudioBuffer = runExceptTWithLog do
        { seCaches } <- getStore
        case find (\{ se: se_ } -> se == se_) seCaches of
          Just { buffer } -> pure buffer
          Nothing -> do
            let url = toUrlSE se
            buffer <- ExceptT $ liftAff $ fetchBinary url
            audio <- liftAff $ array2AudioBuffer buffer
            updateStore $ Store.AddSECache se audio
            pure audio
    in
      void $ runExceptTWithLog do
        buffer <- ExceptT $ getAudioBuffer
        asyncOperation <- liftEffect $ playOneShot (Volume 0.7) buffer
        ExceptT $ liftAff $ asyncOperation

  changeBGMVolume volume = do
    { channel: (Channel c) } <- getStore
    updateStore $ Store.UpdateChannel $ Channel $ c { volume = volume }
