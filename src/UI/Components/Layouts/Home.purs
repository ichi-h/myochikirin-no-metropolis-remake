module UI.Components.Layouts.Home where

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
import UI.Pages.Credit.Page as CreditPage
import UI.Pages.Config.Page as ConfigPage
import UI.Pages.Gallery.Page as GalleryPage
import UI.Pages.Home.Page as HomePage
import UI.Store (connectRoute)
import UI.Store as Store

type HomeLayoutState =
  { route :: Store.Route
  , isReady :: Boolean
  , isNavigating :: Boolean
  }

type HomeLayoutSlot = forall query. H.Slot query Unit Unit

data HomeLayoutAction
  = Initialize
  | Receive (Store.RouteReceive Unit)
  | Navigate Store.Route

type Slots =
  ( homePage :: HomePage.HomeSlot
  , configPage :: forall query. H.Slot query Void Unit
  , galleryPage :: forall query. H.Slot query Void Unit
  , creditPage :: forall query. H.Slot query Void Unit
  )

_homePage = Proxy :: Proxy "homePage"
_galleryPage = Proxy :: Proxy "galleryPage"
_configPage = Proxy :: Proxy "configPage"
_creditPage = Proxy :: Proxy "creditPage"

component
  :: forall query input output m
   . MonadStore Store.Action Store.Store m
  => Audio.Audio m
  => MonadAff m
  => H.Component query input output m
component = connectRoute $ H.mkComponent
  { initialState: \({ context }) ->
      { route: context
      , isReady: false
      , isNavigating: false
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  delayForNavigation :: Number
  delayForNavigation = 500.0

  handleAction :: HomeLayoutAction -> H.HalogenM HomeLayoutState HomeLayoutAction Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      H.liftAff $ delay $ Milliseconds 100.0 -- wait for rendering
      H.modify_ \s -> s { isReady = true }

    Receive input -> do
      let route = Store.deriveRoute input
      H.modify_ \s -> s { route = route }

    Navigate route -> do
      { isNavigating, route: currentRoute } <- H.get
      if isNavigating || route == currentRoute then pure unit
      else do
        Audio.playSE Audio.TurnPageLong
        H.modify_ \s -> s { isNavigating = true }
        H.liftAff $ delay $ Milliseconds delayForNavigation
        updateStore $ Store.Navigate route
        H.modify_ \s -> s { isNavigating = false, route = route }

  render :: HomeLayoutState -> H.ComponentHTML HomeLayoutAction Slots m
  render { route, isReady, isNavigating } =
    HH.div
      [ HP.class_ $ H.ClassName "relative bg-home w-full h-svh flex flex-col bg-cover bg-center"
      ]
      [ HH.div
          [ HP.class_ $ H.ClassName $ "flex-1 duration-300" <> if isNavigating then " opacity-0" else ""
          ]
          [ case route of
              Store.Home -> HH.slot_ _homePage unit HomePage.component unit
              Store.Config -> HH.slot_ _configPage unit ConfigPage.component unit
              Store.Gallery -> HH.slot_ _galleryPage unit GalleryPage.component unit
              Store.Credit -> HH.slot_ _creditPage unit CreditPage.component unit
              _ -> HH.text ""
          ]
      , HH.div
          [ HP.class_ $ H.ClassName "w-full max-2xl:mb-2 mb-4 max-lg:pr-0 max-2xl:pr-4 pr-8" ]
          [ HH.div
              [ HP.class_ $ H.ClassName "w-full flex max-lg:justify-center justify-end max-md:gap-3 max-2xl:gap-6 gap-8 mb-2 text-secondary max-xs:text-sm max-md:text-base max-2xl:text-xl text-3xl"
              ]
              [ HH.button
                  [ HP.class_ $ H.ClassName if route == Store.Home then "underline" else ""
                  , HE.onClick \_ -> Navigate Store.Home
                  ]
                  [ HH.text "ホーム" ]
              , HH.button
                  [ HP.class_ $ H.ClassName if route == Store.Gallery then "underline" else ""
                  , HE.onClick \_ -> Navigate Store.Gallery
                  ]
                  [ HH.text "ギャラリー" ]
              , HH.button
                  [ HP.class_ $ H.ClassName if route == Store.Config then "underline" else ""
                  , HE.onClick \_ -> Navigate Store.Config
                  ]
                  [ HH.text "設定" ]
              , HH.button
                  [ HP.class_ $ H.ClassName if route == Store.Credit then "underline" else ""
                  , HE.onClick \_ -> Navigate Store.Credit
                  ]
                  [ HH.text "クレジット" ]
              ]
          ]
      , HH.div
          [ HP.class_ $ H.ClassName
              ( "absolute top-0 left-0 flex items-center justify-center w-full h-svh bg-primary duration-1000 " <>
                  if not isReady then "opacity-100"
                  else "opacity-0 pointer-events-none"
              )
          ]
          []
      ]
