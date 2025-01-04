module UI.Components.Layouts.Home where

import Prelude

import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import UI.Capabilities.Audio as Audio
import UI.Store (connectRoute)
import UI.Store as Store

layout ∷ ∀ (w2 ∷ Type) (i3 ∷ Type). Array (HH.HTML w2 i3) -> Array (HH.HTML w2 i3) → HH.HTML w2 i3
layout content footer = HH.div
  [ HP.class_ $ H.ClassName "bg-home w-full h-svh flex flex-col bg-cover bg-center"
  ]
  [ HH.div
      [ HP.class_ $ H.ClassName "flex-1"
      ]
      content
  , HH.div
      [ HP.class_ $ H.ClassName "w-full max-2xl:mb-2 mb-4 max-lg:pr-0 max-2xl:pr-4 pr-8" ]
      footer
  ]

type FooterState =
  { route :: Store.Route
  , delayForNavigation :: Number
  , isNavigating :: Boolean
  }

type FooterInput = { delayForNavigation :: Number }

type FooterSlot = forall query. H.Slot query FooterOutput Unit

data FooterAction
  = Receive (Store.RouteReceive FooterInput)
  | Navigate Store.Route

data FooterOutput = Navigated Store.Route

footerComponent
  :: forall query m
   . MonadStore Store.Action Store.Store m
  => Audio.Audio m
  => MonadAff m
  => H.Component query FooterInput FooterOutput m
footerComponent = connectRoute $ H.mkComponent
  { initialState: \({ context, input: { delayForNavigation } }) ->
      { route: context
      , delayForNavigation
      , isNavigating: false
      }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction :: FooterAction -> H.HalogenM FooterState FooterAction () FooterOutput m Unit
  handleAction = case _ of
    Receive input -> do
      let route = Store.deriveRoute input
      H.modify_ \s -> s { route = route }

    Navigate route -> do
      { delayForNavigation, isNavigating } <- H.get
      if isNavigating then pure unit
      else do
        Audio.playSE Audio.TurnPageLong
        H.modify_ \s -> s { isNavigating = true }
        H.raise $ Navigated route
        H.liftAff $ delay $ Milliseconds delayForNavigation
        updateStore $ Store.Navigate route
        H.modify_ \s -> s { isNavigating = false }

  render :: FooterState -> H.ComponentHTML FooterAction () m
  render { route } = HH.div
    [ HP.class_ $ H.ClassName "w-full flex max-lg:justify-center justify-end max-xs:gap-3 max-2xl:gap-6 gap-8 mb-2 text-secondary max-xs:text-sm max-md:text-base max-2xl:text-xl text-3xl" ]
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
        [ HH.text "コンフィグ" ]
    , HH.button
        [ HP.class_ $ H.ClassName if route == Store.Credit then "underline" else ""
        , HE.onClick \_ -> Navigate Store.Credit
        ]
        [ HH.text "クレジット" ]
    ]
