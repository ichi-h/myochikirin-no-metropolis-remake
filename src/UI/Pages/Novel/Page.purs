module UI.Pages.Novel.Page where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import Type.Proxy (Proxy(..))
import UI.Components.Novel as NovelComponent
import UI.Novel as Novel
import UI.Store as Store

data Action
  = Initialize
  | Turn
  | HandleNovel NovelComponent.NovelOutput
  | NoOp

type State =
  { isReady :: Boolean
  , isNavigating :: Boolean
  , novelTitle :: Novel.NovelTitle
  , novelContent :: Array Novel.NovelEvent
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
    { initialState: \({ novelTitle }) ->
        { isReady: false
        , isNavigating: false
        , novelTitle
        , novelContent: Novel.getContent novelTitle
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      H.liftAff $ delay $ Milliseconds 0.0 -- wait for rendering
      H.modify_ \s -> s { isReady = true }
      handleAction $ Turn

    Turn -> do
      H.tell _novel 0 (NovelComponent.Turn)

    HandleNovel output -> do
      case output of
        NovelComponent.Finished -> do
          H.modify_ \s -> s { isNavigating = true }
          H.liftAff $ delay $ Milliseconds 2000.0
          updateStore $ Store.Navigate Store.Home

    NoOp -> pure unit

  render { isReady, isNavigating, novelContent } =
    HH.div
      [ HP.class_ $ HH.ClassName "relative w-full h-svh flex items-center justify-center"
      , HE.onClick \_ -> Turn
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
