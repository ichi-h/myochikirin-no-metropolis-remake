module UI.Pages.Home.Page where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Monad (class MonadStore, updateStore)
import UI.Store as Store

type State = { label :: String }

data Action = ToNovel

component :: forall query input output m. MonadStore Store.Action Store.Store m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: forall i. i -> State
  initialState _ = { label: "Home" }

  render :: State -> H.ComponentHTML Action () m
  render _ = HH.div [] [ HH.button [ HE.onClick \_ -> ToNovel ] [ HH.text "to top" ] ]

  handleAction :: forall o. Action -> H.HalogenM State Action () o m Unit
  handleAction ToNovel = do
    updateStore $ Store.Navigate Store.Novel
