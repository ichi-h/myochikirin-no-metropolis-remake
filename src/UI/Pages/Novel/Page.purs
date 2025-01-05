module UI.Pages.Novel.Page where

import Prelude

import Control.Monad.Except (ExceptT(..), lift)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (output)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import Type.Proxy (Proxy(..))
import UI.Components.Novel (NovelOutput(..))
import UI.Components.Novel as NovelComponent
import UI.Novel as Novel
import UI.Store as Store
import Utils.Logger (debugLog, runExceptTWithLog)

data Action
  = Initialize
  | Turn Int
  | HandleNovel NovelComponent.NovelOutput
  | NoOp

type State =
  { isReady :: Boolean
  , isNavigating :: Boolean
  , novelTitle :: Novel.NovelTitle
  , novelContent :: Array Novel.NovelEvent
  , index :: Int
  }

type Input =
  { novelTitle :: Novel.NovelTitle
  }

speedRate :: Number
speedRate = 0.7

maxShowSec :: Number
maxShowSec = 0.1

totalTimeSec :: String -> Number
totalTimeSec sentence = maxShowSec * (1.0 - speedRate) * toNumber (length sentence)

type Slots = (novel :: NovelComponent.NovelSlot)

_novel = Proxy :: Proxy "novel"

component
  :: forall query output m
   . MonadStore Store.Action Store.Store m
  => MonadAff m
  => H.Component query Input output m
component =
  H.mkComponent
    { initialState: \({ novelTitle }) -> { isReady: false, isNavigating: false, novelTitle, novelContent: Novel.getContent novelTitle, index: 0 }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      H.liftAff $ delay $ Milliseconds 0.0 -- wait for rendering
      H.modify_ \s -> s { isReady = true }
      { index } <- H.get
      handleAction $ Turn index

    Turn i -> do
      H.tell _novel 0 (NovelComponent.Turn i)

    HandleNovel output -> do
      case output of
        NovelComponent.Turned i -> do
          H.modify_ \s -> s { index = i }

        NovelComponent.Finished -> do
          H.modify_ \s -> s { isNavigating = true }
          H.liftAff $ delay $ Milliseconds 2000.0
          updateStore $ Store.Navigate Store.Home

    NoOp -> pure unit

  render { isReady, isNavigating, novelTitle, novelContent, index } =
    HH.div
      [ HP.class_ $ HH.ClassName "relative w-full h-svh flex items-center justify-center"
      , HE.onClick \_ -> Turn $ index + 1
      ]
      [ HH.slot _novel 0 NovelComponent.component { novelContent, index: 0 } HandleNovel
      , HH.div
          [ HP.class_ $ H.ClassName
              ( "absolute top-0 left-0 flex items-center justify-center w-full h-svh bg-primary " <>
                  if isReady && not isNavigating then "duration-1000 opacity-0 pointer-events-none"
                  else if isReady && isNavigating then "duration-2000 opacity-100"
                  else "opacity-100"
              )
          ]
          []
      ]
