module UI.Pages.Home.Page where

import Prelude

import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import UI.Capabilities.Audio as Audio
import UI.Novel as Novel
import UI.Store as Store

type State =
  { isNavigatingToNovel :: Boolean
  }

data Action = NavigateToNovel Novel.NovelTitle

type HomeSlot = forall query. H.Slot query Unit Unit

component
  :: forall query input output m
   . MonadStore Store.Action Store.Store m
  => Audio.Audio m
  => MonadAff m
  => H.Component query input output m
component = H.mkComponent
  { initialState: \_ -> { isNavigatingToNovel: false }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      }
  }
  where
  handleAction :: forall o. Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    NavigateToNovel novelTitle -> do
      { isNavigatingToNovel } <- H.get
      if isNavigatingToNovel then pure unit
      else do
        Audio.playSE Audio.Bell
        H.modify_ \s -> s { isNavigatingToNovel = true }
        H.liftAff $ delay $ Milliseconds 3000.0
        updateStore $ Store.Navigate $ Store.Novel novelTitle

  novelButton novelTitle = HH.button
    [ HP.class_ $ H.ClassName "text-secondary max-xs:text-base max-md:text-xl max-2xl:text-2xl text-4xl"
    , HE.onClick \_ -> NavigateToNovel novelTitle
    ]
    [ HH.text $ Novel.toString novelTitle ]

  render :: State -> H.ComponentHTML Action () m
  render { isNavigatingToNovel } =
    HH.div
      [ HP.class_ $ H.ClassName "relative text-secondary flex flex-col justify-center items-center max-md:gap-2 max-2xl:gap-4 gap-8 h-full"
      ]
      [ HH.h1
          [ HP.class_ $ H.ClassName "font-pigmo max-xs:text-3xl max-md:text-4xl max-2xl:text-5xl text-7xl"
          ]
          [ HH.text "ホーム" ]
      , novelButton Novel.Excursion
      , novelButton Novel.TheSongStealer
      , novelButton Novel.TheEclipse
      , novelButton Novel.QuestionWhereAmINow
      , novelButton Novel.StarEcology
      , novelButton Novel.Interception
      , novelButton Novel.TheSavageMoonCat
      , HH.div
          [ HP.class_ $ H.ClassName
              ( "absolute top-0 left-0 flex items-center justify-center w-full h-svh bg-primary " <>
                  if isNavigatingToNovel then "duration-2000 opacity-100"
                  else "duration-1000 opacity-0 pointer-events-none"
              )
          ]
          []
      ]
