module UI.Pages.Home.Page
  ( Action(..)
  , State
  , _footer
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import Type.Proxy (Proxy(..))
import UI.Capabilities.Audio as Audio
import UI.Components.Layouts.Home as HomeLayout
import UI.Novel as Novel
import UI.Store as Store

type State =
  { isReady :: Boolean
  , isNavigating :: Boolean
  , isNavigatingToNovel :: Boolean
  }

data Action
  = Initialize
  | NavigateToNovel Novel.NovelTitle
  | HandleFooter HomeLayout.FooterOutput

type Slots = (footer :: HomeLayout.FooterSlot)

_footer = Proxy :: Proxy "footer"

component
  :: forall query input output m
   . MonadStore Store.Action Store.Store m
  => Audio.Audio m
  => MonadAff m
  => H.Component query input output m
component = H.mkComponent
  { initialState: \_ -> { isReady: false, isNavigating: false, isNavigatingToNovel: false }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  delayForNavigation :: Number
  delayForNavigation = 500.0

  handleAction :: forall o. Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Initialize -> do
      H.liftAff $ delay $ Milliseconds 0.0 -- wait for rendering
      H.modify_ \s -> s { isReady = true }

    NavigateToNovel novelTitle -> do
      { isNavigatingToNovel } <- H.get
      if isNavigatingToNovel then pure unit
      else do
        Audio.playSE Audio.Bell
        H.modify_ \s -> s { isNavigatingToNovel = true }
        H.liftAff $ delay $ Milliseconds 3000.0
        updateStore $ Store.Navigate $ Store.Novel novelTitle

    HandleFooter a -> case a of
      HomeLayout.Navigated _ -> do
        H.modify_ \s -> s { isNavigating = true }
        H.liftAff $ delay $ Milliseconds delayForNavigation

  novelButton novelTitle = HH.button
    [ HP.class_ $ H.ClassName "text-secondary max-xs:text-base max-md:text-xl max-2xl:text-2xl text-4xl"
    , HE.onClick \_ -> NavigateToNovel novelTitle
    ]
    [ HH.text $ Novel.toString novelTitle ]

  render :: State -> H.ComponentHTML Action Slots m
  render { isReady, isNavigating, isNavigatingToNovel } =
    HomeLayout.layout
      [ HH.div
          [ HP.class_ $ H.ClassName ("relative text-secondary flex flex-col justify-center items-center max-md:gap-2 max-2xl:gap-4 gap-8 h-full duration-300" <> if isNavigating then " opacity-0" else "")
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
                      if isReady && not isNavigatingToNovel then "duration-1000 opacity-0 pointer-events-none"
                      else if isReady && isNavigatingToNovel then "duration-2000 opacity-100"
                      else "opacity-100"
                  )
              ]
              []
          ]
      ]
      [ HH.slot _footer unit HomeLayout.footerComponent { delayForNavigation } HandleFooter
      ]
