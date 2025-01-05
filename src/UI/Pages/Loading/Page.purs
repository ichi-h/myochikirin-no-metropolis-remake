module UI.Pages.Loading.Page where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, fromNonEmpty, toArray, zip)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import UI.Capabilities.Audio (toUrlSE)
import UI.Capabilities.Audio as Audio
import UI.Store as Store
import Utils.Buffer (array2AudioBuffer)
import Utils.Fetch (fetchBinary)

type State =
  { progress :: Number
  , isReady :: Boolean
  , isError :: Boolean
  , isNavigating :: Boolean
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
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  initialState :: forall i. i -> State
  initialState _ =
    { progress: 0.0
    , isReady: false
    , isError: false
    , isNavigating: false
    }

  handleAction :: forall o. Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Initialize -> do
      H.modify_ \s -> s { progress = 0.33 }

      -- assets pre-loading
      let
        urls =
          [ "/sounds/bgm.ogg"
          , "/images/1.webp"
          , "/images/2.webp"
          , "/images/3.webp"
          , "/images/4.webp"
          , "/images/5.webp"
          , "/images/6.webp"
          , "/images/7.webp"
          , "/images/home.webp"
          , "/images/secret.webp"
          , "/images/title.webp"
          , "/images/top.webp"
          , "/assets/fonts/pigmo01/pigmo01.min.woff2"
          , "/assets/fonts/togoshi-gothic/togoshi-gothic.min.woff2"
          ]
      _ <- liftAff $ parTraverse fetchBinary urls

      H.modify_ \s -> s { progress = 0.66 }

      -- cache SEs
      let
        ses :: NonEmptyArray Audio.SE
        ses = fromNonEmpty $
          ( Audio.Bell :|
              [ Audio.TurnPageLong
              , Audio.TurnPageShort
              ]
          )
        seUrls = map toUrlSE ses

      responses <- liftAff $ parTraverse fetchBinary $ toArray seUrls
      case fromArray responses of
        Nothing -> H.modify_ \s -> s { isError = true }
        Just nonEmpty -> do
          for_ (zip ses nonEmpty) \(Tuple se result) -> do
            case result of
              Left _ -> H.modify_ \s -> s { isError = true }
              Right buffer -> do
                audioBuffer <- liftAff $ array2AudioBuffer buffer
                updateStore $ Store.AddSECache se audioBuffer
                H.modify_ \s -> s { progress = 1.0 }
                H.liftAff $ delay $ Milliseconds $ 300.0
                H.modify_ \s -> s { isReady = true }

    ToTop -> do
      { isNavigating } <- H.get
      if isNavigating then pure unit
      else do
        Audio.playSE Audio.Bell
        H.modify_ \s -> s { isNavigating = true }
        H.liftAff $ delay $ Milliseconds $ 1000.0
        updateStore $ Store.Navigate Store.Top

  render :: State -> H.ComponentHTML Action () m
  render { progress, isReady, isError, isNavigating } =
    HH.div
      [ HP.class_ $ H.ClassName ("relative w-full h-svh")
      ]
      [ HH.div
          [ HP.class_ $ H.ClassName
              ( "absolute top-0 left-0 flex items-center justify-center bg-primary " <>
                  if progress < 1.0 then "w-full h-svh duration-300"
                  else if not isReady && progress == 1.0 then "w-full h-svh duration-300 opacity-0"
                  else if isReady || isNavigating then "w-0 h-0"
                  else "w-full h-svh"
              )
          ]
          [ HH.div
              [ HP.class_ $ H.ClassName "w-1/2 max-w-40"
              ]
              [ HH.div
                  [ HP.class_ $ H.ClassName "h-1 bg-secondary rounded-sm duration-200"
                  , HP.style $ "width: " <> (show $ 100.0 * progress) <> "%"
                  ]
                  []
              ]
          ]
      , HH.div
          [ HP.class_ $ H.ClassName ("flex flex-col items-center justify-center gap-6 max-sm:gap-4 w-full h-full text-center text-2xl max-xs:text-xs max-sm:text-sm max-md:text-xl duration-1000 " <> if isNavigating then "opacity-0" else "")
          ]
          [ HH.img
              [ HP.class_ $ H.ClassName "w-128 max-sm:w-60 max-lg:w-96 h-auto bg-center"
              , HP.src "images/title.webp"
              ]
          , if isError then
              HH.div
                [ HP.class_ $ H.ClassName "text-secondary w-full"
                ]
                [ HH.text "エラーが発生しました。"
                , HH.br []
                , HH.text "時間をおいて再度お試しください"
                ]
            else
              HH.div
                [ HP.class_ $ H.ClassName "text-secondary w-full"
                ]
                [ HH.text "このゲームは音声が流れます。"
                , HH.br []
                , HH.text "音量にご注意ください。"
                ]
          , if isError then
              HH.div [] []
            else
              HH.div
                [ HP.class_ $ H.ClassName "w-1/2 max-w-40 flex items-center h-7 max-xs:h-8 max-sm:h-9 max-md:h-10"
                ]
                [ HH.button
                    [ HP.class_ $ H.ClassName "text-secondary border border-secondary rounded-sm outline-none px-4 py-1 m-auto"
                    , HE.onClick \_ -> ToTop
                    ]
                    [ HH.text "はじめる" ]
                ]
          ]
      ]
