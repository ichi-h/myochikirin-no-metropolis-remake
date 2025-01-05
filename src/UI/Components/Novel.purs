module UI.Components.Novel where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UI.Novel as Novel

type NovelState =
  { novelContent :: Array Novel.NovelEvent
  , eventStore ::
      { messages :: Array Novel.MessageEvent
      , images :: Array Novel.ImageEvent
      , texts :: Array Novel.TextEvent
      , baseLayer :: Maybe Novel.BaseLayerEvent
      , messageBox :: Maybe Novel.MessageBoxEvent
      }
  , index :: Int
  , isWaiting :: Boolean
  }

type NovelInput =
  { novelContent :: Array Novel.NovelEvent
  , index :: Int
  }

data NovelOutput
  = Turned Int
  | Finished

data NovelAction = NoOp

data NovelQuery a = Turn Int a

type NovelSlot = H.Slot NovelQuery NovelOutput Int

component
  :: forall m
   . MonadAff m
  => H.Component NovelQuery NovelInput NovelOutput m
component = H.mkComponent
  { initialState: \{ novelContent, index } ->
      { novelContent
      , eventStore: { messages: [], images: [], texts: [], baseLayer: Nothing, messageBox: Nothing }
      , index
      , isWaiting: false
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleQuery = handleQuery
      }
  }
  where
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
            { eventStore } <- H.get
            case event of
              Novel.Message e -> do
                let
                  { waitClick, clear } = e
                  newText = if clear then [ e ] else eventStore.messages <> [ e ]
                H.modify_ \s -> s { index = i, eventStore = eventStore { messages = newText } }
                H.raise $ Turned i
                if waitClick then pure Nothing
                else handleQuery $ Turn (i + 1) next
              Novel.Image e -> do
                H.modify_ \s -> s { index = i, eventStore = eventStore { images = eventStore.images <> [ e ] } }
                H.raise $ Turned i
                handleQuery $ Turn (i + 1) next
              Novel.Text e -> do
                H.modify_ \s -> s { index = i, eventStore = eventStore { texts = eventStore.texts <> [ e ] } }
                H.raise $ Turned i
                handleQuery $ Turn (i + 1) next
              Novel.BaseLayer e -> do
                H.modify_ \s -> s { index = i, eventStore = eventStore { baseLayer = Just e } }
                H.raise $ Turned i
                handleQuery $ Turn (i + 1) next
              Novel.MessageBox e -> do
                H.modify_ \s -> s { index = i, eventStore = eventStore { messageBox = Just e } }
                H.raise $ Turned i
                handleQuery $ Turn (i + 1) next
              Novel.Wait { time } -> do
                H.modify_ \s -> s { isWaiting = true, index = i }
                H.raise $ Turned i
                H.liftAff $ delay $ Milliseconds time
                H.modify_ \s -> s { isWaiting = false }
                handleQuery $ Turn (i + 1) next
          Nothing -> do
            H.raise Finished
            pure $ Just next

  render :: NovelState -> H.ComponentHTML NovelAction () m
  render { eventStore: { messages, images, texts, baseLayer, messageBox } } =
    HH.div
      [ HP.class_ $ H.ClassName $ case baseLayer of
          Just { style } -> style
          Nothing -> ""
      ]
      $ (map (\({ src, style }) -> HH.img [ HP.class_ $ H.ClassName style, HP.src src ]) images)
          <> (map (\({ text, style }) -> HH.div [ HP.class_ $ H.ClassName style ] [ HH.text text ]) texts)
          <>
            [ HH.div
                [ HP.class_ $ H.ClassName $ case messageBox of
                    Just { style } -> style
                    Nothing -> ""
                ]
                (map (\{ text, style } -> HH.p [ HP.class_ $ H.ClassName style ] [ HH.text text ]) messages)
            ]
