module UI.Capabilities.Audio where

import Prelude

import Control.Monad.Trans.Class (lift)
import Domain.Values.Audio.Volume (Volume)
import Halogen as H

data BGM = Theme

data SE = Bell | TurnPageLong | TurnPageShort

toUrl :: BGM -> String
toUrl Theme = "/assets/sounds/bgm.ogg"

toUrlSE :: SE -> String
toUrlSE Bell = "/assets/sounds/bell.ogg"
toUrlSE TurnPageLong = "/assets/sounds/turnPage1.ogg"
toUrlSE TurnPageShort = "/assets/sounds/turnPage2.ogg"

class Monad m <= Audio m where
  playBGM :: BGM -> m Unit
  playSE :: SE -> m Unit
  changeBGMVolume :: Volume -> m Unit

instance audioHalogenM :: Audio m => Audio (H.HalogenM st act slots msg m) where
  playBGM = lift <<< playBGM
  playSE = lift <<< playSE
  changeBGMVolume = lift <<< changeBGMVolume
