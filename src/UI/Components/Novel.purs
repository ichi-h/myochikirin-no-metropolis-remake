module UI.Components.Novel where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UI.Novel as Novel
import Utils.Logger (debugLog)

type NovelState =
  { novelContent :: Novel.NovelContent
  , eventStore :: Novel.NovelContent
  , index :: Int
  , isWaiting :: Boolean
  }

data NovelAction = Initialize

type NovelInput =
  { novelContent :: Novel.NovelContent
  , index :: Int
  }

data NovelOutput
  = Turned Int
  | Finished

data NovelQuery a = Turn Int a

type NovelSlot = H.Slot NovelQuery NovelOutput Int

component
  :: forall m
   . MonadAff m
  => H.Component NovelQuery NovelInput NovelOutput m
component = H.mkComponent
  { initialState: \{ novelContent, index } -> { novelContent, eventStore: [], index, isWaiting: false }
  , render
  , eval: H.mkEval H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: NovelAction -> H.HalogenM NovelState NovelAction () NovelOutput m Unit
  handleAction = case _ of
    Initialize -> do
      { index } <- H.get
      _ <- handleQuery $ Turn index Finished
      pure unit

  handleQuery
    :: forall a
     . NovelQuery a
    -> H.HalogenM NovelState NovelAction () NovelOutput m (Maybe a)
  handleQuery = case _ of
    Turn i next -> do
      { isWaiting } <- H.get
      if isWaiting then pure Nothing
      else do
        { novelContent } <- H.get
        case Array.index novelContent i of
          Just event -> do
            liftEffect $ debugLog event
            H.modify_ \s -> s { index = i, eventStore = s.eventStore <> [ event ] }
            H.raise $ Turned i
            case event of
              Novel.Wait { time } -> do
                H.modify_ \s -> s { isWaiting = true }
                H.liftAff $ delay $ Milliseconds time
                H.modify_ \s -> s { isWaiting = false }
                handleQuery $ Turn (i + 1) next
              Novel.Image _ -> do
                handleQuery $ Turn (i + 1) next
              _ -> do
                pure $ Just next
          Nothing -> do
            H.raise Finished
            pure $ Just next

  render :: NovelState -> H.ComponentHTML NovelAction () m
  render { eventStore } = HH.div
    [ HP.class_ $ H.ClassName "w-full h-full text-secondary" ]
    content
    where
    eventToHtml event = case event of
      Novel.Text { text, style } -> HH.p [ HP.class_ $ H.ClassName style ] [ HH.text text ]
      Novel.Image { src, style } -> HH.img [ HP.class_ $ H.ClassName style, HP.src src ]
      _ -> HH.text ""

    content = map eventToHtml eventStore
