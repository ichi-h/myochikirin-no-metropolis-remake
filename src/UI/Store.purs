module UI.Store where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Domain.Values.Audio.Volume (Volume(..))
import Entities.Audio.Channel as Channel
import UI.Capabilities.Audio as Audio
import Utils.Buffer (AudioBuffer)

data Route = Loading | Top | Home | Novel | Gallery | Config

derive instance eqRoute :: Eq Route

type Store =
  { route :: Route
  , channel :: Channel.Channel
  , seCaches :: Array { se :: Audio.SE, buffer :: AudioBuffer }
  }

data Action
  = Navigate Route
  | UpdateChannel Channel.Channel
  | AddSECache Audio.SE AudioBuffer

initialStore :: Store
initialStore =
  { route: Loading
  , channel: Channel.Channel
      { name: "channel"
      , playStatus: Channel.Stopped
      , volume: Volume 1.0
      , loop: Nothing
      }
  , seCaches: []
  }

reducer :: Store -> Action -> Store
reducer store = case _ of
  (Navigate route) -> store { route = route }
  (UpdateChannel channel) -> store { channel = channel }
  (AddSECache se buffer) -> store { seCaches = { se, buffer } : store.seCaches }
