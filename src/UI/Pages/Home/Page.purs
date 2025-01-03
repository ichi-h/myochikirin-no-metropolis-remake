module UI.Pages.Home.Page where

import Halogen as H
import Halogen.HTML as HH

type State = { label :: String }

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  initialState :: forall i. i -> State
  initialState _ = { label: "Home" }

  render :: forall action. State -> H.ComponentHTML action () m
  render { label } = HH.div [] [ HH.text label ]
