module UI.Store where

import Prelude

data Route = Top | Home | Novel | Gallery | Config

derive instance eqRoute :: Eq Route

type Store =
  { route :: Route
  }

data Action = Navigate Route

initialState :: Store
initialState = { route: Top }

reducer :: Store -> Action -> Store
reducer store (Navigate route) = store { route = route }
