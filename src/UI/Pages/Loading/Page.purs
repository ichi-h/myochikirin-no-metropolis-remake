module UI.Pages.Loading.Page where

import Prelude

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
  { progress :: Number
  , isError :: Boolean
  , isAnimating :: Boolean
  }

data Action
  = ToTop
  | Initialize

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
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: forall i. i -> State
  initialState _ = { progress: 0.0, isError: false, isAnimating: false }

  handleAction :: forall o. Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Initialize -> do
      pure unit

    ToTop -> do
      Audio.playSE Audio.Bell
      H.modify_ \s -> s { isAnimating = true }
      H.liftAff $ delay $ Milliseconds $ 1000.0
      updateStore $ Store.Navigate Store.Top

  render :: State -> H.ComponentHTML Action () m
  render { progress, isError, isAnimating } =
    HH.div
      [ HP.class_ $ H.ClassName ("flex flex-col items-center justify-center gap-6 max-sm:gap-4 w-full h-svh text-center text-2xl max-xs:text-xs max-sm:text-sm max-md:text-xl duration-1000 " <> if isAnimating then "opacity-0" else "")
      ]
      [ HH.img
          [ HP.class_ $ H.ClassName "w-128 max-sm:w-60 max-lg:w-96 h-auto bg-center"
          , HP.src "assets/images/title.webp"
          ]
      , HH.div
          [ HP.class_ $ H.ClassName "text-secondary w-full"
          ]
          [ HH.text "このゲームは音声が流れます。"
          , HH.br []
          , HH.text "音量設定にご注意ください。"
          ]
      , HH.button
          [ HP.class_ $ H.ClassName "text-secondary border border-secondary rounded-sm outline-none px-4 py-1"
          , HE.onClick \_ -> ToTop
          ]
          [ HH.text "START" ]
      ]
