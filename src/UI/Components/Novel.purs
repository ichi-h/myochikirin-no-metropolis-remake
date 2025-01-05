module UI.Components.Novel where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UI.Novel as Novel

data MessageAnimation
  = Rendering
  | Showing
  | Completed

derive instance eqSentenceAnimation :: Eq MessageAnimation

type NovelState =
  { novelContent :: Array Novel.NovelEvent
  , eventStore ::
      { messages :: Array Novel.MessageEvent
      , images :: Array Novel.ImageEvent
      , texts :: Array Novel.TextEvent
      , baseLayer :: Maybe Novel.BaseLayerEvent
      , messageBox :: Maybe Novel.MessageBoxEvent
      }
  , messageAnimation :: MessageAnimation
  , index :: Int
  , isWaiting :: Boolean
  }

type NovelInput =
  { novelContent :: Array Novel.NovelEvent
  , index :: Int
  }

data NovelOutput = Finished

data NovelAction
  = StartAnimation
  | CompleteAnimation

data NovelQuery a = Turn a

type NovelSlot = H.Slot NovelQuery NovelOutput Int

component
  :: forall m
   . MonadAff m
  => H.Component NovelQuery NovelInput NovelOutput m
component = H.mkComponent
  { initialState: \{ novelContent, index } ->
      { novelContent
      , eventStore: { messages: [], images: [], texts: [], baseLayer: Nothing, messageBox: Nothing }
      , messageAnimation: Completed
      , index
      , isWaiting: false
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  speedRate :: Number
  speedRate = 0.7

  maxShowSec :: Number
  maxShowSec = 0.1

  totalTimeSec :: String -> Number
  totalTimeSec text = maxShowSec * (1.0 - speedRate) * toNumber (String.length text)

  handleAction :: NovelAction -> H.HalogenM NovelState NovelAction () NovelOutput m Unit
  handleAction = case _ of
    StartAnimation -> do
      { index: beforeIndex, eventStore: { messages } } <- H.get
      case Array.last messages of
        Just { text } -> do
          H.modify_ \s -> s { messageAnimation = Showing }
          H.liftAff $ delay $ Milliseconds $ 1000.0 * totalTimeSec text
          { index: afterIndex } <- H.get
          if beforeIndex == afterIndex then
            handleAction CompleteAnimation
          else
            pure unit
        Nothing -> pure unit

    CompleteAnimation -> do
      H.modify_ \s -> s { messageAnimation = Completed }

  handleQuery
    :: forall a
     . NovelQuery a
    -> H.HalogenM NovelState NovelAction () NovelOutput m (Maybe a)
  handleQuery = case _ of
    Turn next -> do
      { isWaiting, messageAnimation, index: i } <- H.get
      if isWaiting || messageAnimation == Rendering then pure $ Just next
      else if messageAnimation == Showing then do
        handleAction CompleteAnimation
        pure $ Just next
      else do
        let newIndex = i + 1
        H.modify_ \s -> s { index = newIndex }
        { novelContent } <- H.get
        case Array.index novelContent i of
          Just event -> do
            { eventStore } <- H.get
            case event of
              Novel.Message e -> do
                let
                  { waitClick, clear } = e
                  newText = if clear then [ e ] else eventStore.messages <> [ e ]
                H.modify_ \s -> s { eventStore = eventStore { messages = newText }, messageAnimation = Rendering }
                H.liftAff $ delay $ Milliseconds $ 20.0 -- wait for rendering
                handleAction StartAnimation
                if waitClick then pure $ Just next
                else handleQuery $ Turn next
              Novel.Image e -> do
                H.modify_ \s -> s { eventStore = eventStore { images = eventStore.images <> [ e ] } }
                handleQuery $ Turn next
              Novel.Text e -> do
                H.modify_ \s -> s { eventStore = eventStore { texts = eventStore.texts <> [ e ] } }
                handleQuery $ Turn next
              Novel.BaseLayer e -> do
                H.modify_ \s -> s { eventStore = eventStore { baseLayer = Just e } }
                handleQuery $ Turn next
              Novel.MessageBox e -> do
                H.modify_ \s -> s { eventStore = eventStore { messageBox = Just e } }
                handleQuery $ Turn next
              Novel.Wait { time } -> do
                H.modify_ \s -> s { isWaiting = true }
                H.liftAff $ delay $ Milliseconds time
                H.modify_ \s -> s { isWaiting = false }
                handleQuery $ Turn next
          Nothing -> do
            H.raise Finished
            pure $ Just next

  render :: NovelState -> H.ComponentHTML NovelAction () m
  render { eventStore: { messages, images, texts, baseLayer, messageBox }, messageAnimation } =
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
                ( Array.mapWithIndex
                    ( \i { text, style } ->
                        if i == (-) (Array.length messages) 1 then
                          HH.p
                            [ HP.class_ $ H.ClassName style ]
                            ( Array.mapWithIndex
                                ( \j c -> HH.span
                                    [ HP.style
                                        ( case messageAnimation of
                                            Rendering -> "opacity: 0;"
                                            Showing ->
                                              "transition-timing-function: ease-in; "
                                                <> (if speedRate /= 1.0 then "transition-duration: 0.05s;" else "")
                                                <> " transition-delay: "
                                                <> (show $ maxShowSec * (1.0 - speedRate) * toNumber j)
                                                <> "s; opacity: 1;"
                                            Completed -> "opacity: 1;"
                                        )
                                    ]
                                    [ HH.text $ fromCharArray [ c ] ]
                                )
                                (toCharArray text)
                            )
                        else
                          HH.p [ HP.class_ $ H.ClassName style ] [ HH.text text ]
                    )
                    messages
                )
            ]
