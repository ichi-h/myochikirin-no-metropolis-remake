module UI.Pages.Top.Page where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import UI.Capabilities.Audio as Audio
import UI.Store as Store

type State =
  { isReady :: Boolean
  , isNavigating :: Boolean
  }

data Action
  = Initialize
  | ToHome

component
  :: forall query input output m
   . MonadStore Store.Action Store.Store m
  => Audio.Audio m
  => MonadAff m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  initialState :: forall i. i -> State
  initialState _ =
    { isReady: false
    , isNavigating: false
    }

  handleAction :: forall o. Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Initialize -> do
      Audio.playBGM Audio.Theme
      H.modify_ \s -> s { isReady = true }
      H.liftAff $ delay $ Milliseconds $ 1000.0

    ToHome -> do
      { isNavigating } <- H.get
      if isNavigating then pure unit
      else do
        Audio.playSE Audio.Bell
        H.modify_ \s -> s { isNavigating = true }
        H.liftAff $ delay $ Milliseconds $ 3000.0
        updateStore $ Store.Navigate Store.Home

  render :: State -> H.ComponentHTML Action () m
  render { isReady, isNavigating } = HH.div
    [ HP.class_ $ H.ClassName "relative w-full h-svh bg-top bg-cover"
    , HE.onClick \_ -> ToHome
    ]
    [ HH.div
        [ HP.class_ $ H.ClassName "absolute top-0 w-full h-1/12 max-sm:h-16 max-lg:h-40 bg-primary"
        ]
        []
    , HH.div
        [ HP.class_ $ H.ClassName "absolute bottom-0 w-full h-1/12 max-sm:h-16 max-lg:h-40 bg-primary"
        ]
        []
    , HH.div
        [ HP.class_ $ H.ClassName "flex flex-col items-center justify-center max-sm:gap-4 gap-6 w-full h-full"
        ]
        [ HH.img
            [ HP.class_ $ H.ClassName "bg-title bg-no-repeat bg-contain w-256 max-lg:w-11/12 max-2xl:w-192 h-auto bg-center"
            , HP.src "assets/images/title.webp"
            ]
        , HH.div
            [ HP.class_ $ H.ClassName "text-secondary w-full text-center text-4xl max-xs:text-sm max-sm:text-lg max-md:text-2xl max-2xl:text-3xl animate-blinking"
            ]
            [ HH.text "画面をクリック/タップしてください。" ]
        ]
    , HH.div
        [ HP.class_ $ H.ClassName
            ( "absolute top-0 left-0 w-full h-svh flex items-center justify-center bg-primary " <>
                if isReady && not isNavigating then "duration-1000 opacity-0"
                else if isReady && isNavigating then "duration-2000 opacity-100"
                else ""
            )
        ]
        []
    ]
