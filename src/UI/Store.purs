module UI.Store where

import Prelude

import Data.Maybe (Maybe(..))
import Domain.Values.Audio.Volume (Volume(..))
import Entities.Audio.Channel as Channel

data Route = Loading | Top | Home | Novel | Gallery | Config

derive instance eqRoute :: Eq Route

type Store =
  { route :: Route
  , channel :: Channel.Channel
  }

data Action
  = Navigate Route
  | UpdateChannel Channel.Channel

initialStore :: Store
initialStore =
  { route: Loading
  , channel: Channel.Channel
      { name: "channel"
      , playStatus: Channel.Stopped
      , volume: Volume 1.0
      , loop: Nothing
      }
  }

reducer :: Store -> Action -> Store
reducer store = case _ of
  (Navigate route) -> store { route = route }
  (UpdateChannel channel) -> store { channel = channel }
