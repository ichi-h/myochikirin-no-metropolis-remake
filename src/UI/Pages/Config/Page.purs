module UI.Pages.Config.Page where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = {}

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  initialState :: forall i. i -> State
  initialState _ = {}

  render :: forall action. State -> H.ComponentHTML action () m
  render {} = HH.div
    [ HP.class_ $ H.ClassName "relative text-secondary flex flex-col justify-center items-center max-md:gap-2 max-2xl:gap-4 gap-8 h-full"
    ]
    [ HH.h1
        [ HP.class_ $ H.ClassName "font-pigmo max-xs:text-3xl max-md:text-4xl max-2xl:text-5xl text-7xl"
        ]
        [ HH.text "設定" ]
    , HH.div
        [ HP.class_ $ H.ClassName "max-xs:text-base max-md:text-xl max-2xl:text-2xl text-4xl"
        ]
        [ HH.text "coming soon..." ]
    ]
