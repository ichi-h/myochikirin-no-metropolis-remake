module UI.Pages.Novel.Page where

import Prelude

import Adapter.Audio.Channel (changeVolume, pause, play, register, resume, stop)
import Adapter.Audio.PlayOneShot (playOneShot)
import Control.Monad.Except (ExceptT(..), lift)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Domain.Services.Fetch (fetchAudio)
import Domain.Values.Audio.DelayMs (DelayMs(..))
import Domain.Values.Audio.FadeInMs (FadeInMs(..))
import Domain.Values.Audio.FadeOutMs (FadeOutMs(..))
import Domain.Values.Audio.OffsetMs (OffsetMs(..))
import Domain.Values.Audio.Samples (Samples(..), maxSamples)
import Domain.Values.Audio.Volume (Volume(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Entities.Audio.Channel (Channel(..), Loop(..), PlayStatus(..))
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Utils.Logger (runExceptTWithLog)

data Action
  = Setup
  | Play
  | Pause
  | Resume
  | Stop
  | ChangeVolume Volume
  | StartAnimation
  | NextSentence
  | FinishAnimation
  | NoOp

data SentenceAnimation
  = Ready
  | Rendering
  | Showing
  | Finished

derive instance eqSentenceAnimation :: Eq SentenceAnimation

data State = State
  { channel :: Channel
  , sentenceAnimation :: SentenceAnimation
  , sentence :: String
  , sentenceIndex :: Int
  }

speedRate :: Number
speedRate = 0.7

maxShowSec :: Number
maxShowSec = 0.1

sentences :: Array String
sentences =
  [ "吾輩は猫である。名前はまだ無い。"
  , "どこで生れたかとんと見当がつかぬ。"
  , "何でも薄暗いじめじめした所でニャーニャー泣いていた事だけは記憶している。"
  , "吾輩はここで始めて人間というものを見た。"
  , "しかもあとで聞くとそれは書生という人間中で一番獰悪な種族であったそうだ。"
  , "この書生というのは時々我々を捕えて煮て食うという話である。"
  , "しかしその当時は何という考もなかったから別段恐しいとも思わなかった。"
  , "ただ彼の掌に載せられてスーと持ち上げられた時何だかフワフワした感じがあったばかりである。"
  , "掌の上で少し落ちついて書生の顔を見たのがいわゆる人間というものの見始であろう。"
  , "この時妙なものだと思った感じが今でも残っている。"
  , "第一毛をもって装飾されべきはずの顔がつるつるしてまるで薬缶だ。"
  , "その後猫にもだいぶ逢ったがこんな片輪には一度も出会わした事がない。"
  , "のみならず顔の真中があまりに突起している。"
  , "そうしてその穴の中から時々ぷうぷうと煙を吹く。"
  , "どうも咽せぽくて実に弱った。"
  , "これが人間の飲む煙草というものである事はようやくこの頃知った。……"
  , "（Replay）"
  ]

totalTimeSec :: String -> Number
totalTimeSec sentence = maxShowSec * (1.0 - speedRate) * toNumber (length sentence)

type Slots =
  ( router :: forall query. H.Slot query Void Int
  )

_router = Proxy :: Proxy "router"

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Setup }
    }
  where
  initialState :: forall i. i -> State
  initialState _ = State
    { channel: Channel
        { name: "channel"
        , playStatus: Stopped
        , volume: Volume 1.0
        , loop: Nothing
        }
    , sentenceAnimation: Ready
    , sentence: ""
    , sentenceIndex: -1
    }

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Setup -> void $ runExceptTWithLog do
      buffer <- ExceptT $ liftAff $ fetchAudio "/assets/sounds/bgm.ogg"
      asyncOperation <- liftEffect $ register "channel" buffer (Volume 1.0) (Just $ Loop { start: Samples 222966, end: maxSamples })
      channel <- ExceptT $ liftAff $ asyncOperation
      H.modify_ \(State s) -> State (s { channel = channel })

    Play -> void $ runExceptTWithLog do
      State state <- H.get
      channel <- ExceptT $ liftEffect $ play (DelayMs 0) (OffsetMs 0) (FadeInMs 0) (FadeOutMs 0) state.channel
      H.modify_ \(State s) -> State $ s { channel = channel }

    Pause -> void $ runExceptTWithLog do
      State state <- H.get
      c <- ExceptT $ liftEffect $ pause (FadeOutMs 500) state.channel
      H.modify_ \(State s) -> State (s { channel = c })

    Resume -> void $ runExceptTWithLog do
      State state <- H.get
      c <- ExceptT $ liftEffect $ resume (FadeInMs 500) state.channel
      H.modify_ \(State s) -> State (s { channel = c })

    Stop -> void $ runExceptTWithLog do
      State state <- H.get
      c <- ExceptT $ liftEffect $ stop (FadeOutMs 500) state.channel
      H.modify_ \(State s) -> State (s { channel = c })

    ChangeVolume volume -> void $ runExceptTWithLog do
      State state <- H.get
      c <- ExceptT $ liftEffect $ changeVolume volume state.channel
      H.modify_ \(State s) -> State (s { channel = c })

    StartAnimation -> do
      State beforeState <- H.get
      H.modify_ \(State s) -> State $ s { sentenceAnimation = Showing }
      H.liftAff $ delay $ Milliseconds $ 1000.0 * totalTimeSec beforeState.sentence
      State nextState <- H.get
      if nextState.sentenceIndex == beforeState.sentenceIndex then
        handleAction FinishAnimation
      else
        pure unit

    NextSentence -> void $ runExceptTWithLog do
      State state <- H.get
      buffer <- ExceptT $ liftAff $ fetchAudio "/assets/sounds/bell.ogg"
      asyncOperation <- liftEffect $ playOneShot (Volume 1.0) buffer
      _ <- ExceptT $ liftAff $ asyncOperation
      let nextIndex = if state.sentenceIndex + 1 >= Array.length sentences then 0 else state.sentenceIndex + 1
      H.modify_ \(State s) -> State $ s
        { sentenceAnimation = Rendering
        , sentenceIndex = nextIndex
        , sentence = fromMaybe "" $ Array.index sentences nextIndex
        }
      H.liftAff $ delay $ Milliseconds $ 20.0 -- wait for rendering
      lift $ handleAction StartAnimation

    FinishAnimation -> H.modify_ \(State s) -> State $ s { sentenceAnimation = Finished }

    NoOp -> pure unit

  render (State { sentenceAnimation, sentence }) =
    HH.div
      [ HP.class_ $ HH.ClassName "w-full h-svh flex items-center justify-center"
      ]
      [ if sentenceAnimation == Ready then
          HH.div
            [ HP.class_ $ HH.ClassName "w-full h-full flex items-center justify-center"
            ]
            [ HH.button
                [ HE.onClick \_ -> Setup
                , HP.class_ $ HH.ClassName "w-1/2 h-16 text-white text-4xl rounded-sm shadow-lg flex items-center justify-center border-2 border-white"
                ]
                [ HH.text "Setup" ]
            , HH.button
                [ HE.onClick \_ -> Play
                , HP.class_ $ HH.ClassName "w-1/2 h-16 text-white text-4xl rounded-sm shadow-lg flex items-center justify-center border-2 border-white"
                ]
                [ HH.text "Play" ]
            , HH.button
                [ HE.onClick \_ -> Pause
                , HP.class_ $ HH.ClassName "w-1/2 h-16 text-white text-4xl rounded-sm shadow-lg flex items-center justify-center border-2 border-white"
                ]
                [ HH.text "Pause" ]
            , HH.button
                [ HE.onClick \_ -> Resume
                , HP.class_ $ HH.ClassName "w-1/2 h-16 text-white text-4xl rounded-sm shadow-lg flex items-center justify-center border-2 border-white"
                ]
                [ HH.text "Resume" ]
            , HH.button
                [ HE.onClick \_ -> Stop
                , HP.class_ $ HH.ClassName "w-1/2 h-16 text-white text-4xl rounded-sm shadow-lg flex items-center justify-center border-2 border-white"
                ]
                [ HH.text "Stop" ]
            , HH.button
                [ HE.onClick \_ -> ChangeVolume (Volume 0.1)
                , HP.class_ $ HH.ClassName "w-1/2 h-16 text-white text-4xl rounded-sm shadow-lg flex items-center justify-center border-2 border-white"
                ]
                [ HH.text "ChangeVolume" ]
            , HH.button
                [ HE.onClick \_ -> NextSentence
                , HP.class_ $ HH.ClassName "w-1/2 h-16 text-white text-4xl rounded-sm shadow-lg flex items-center justify-center border-2 border-white"
                ]
                [ HH.text "NextSentence" ]
            ]
        else
          HH.div
            [ HP.class_ $ HH.ClassName "w-full h-full flex items-center justify-center"
            , HE.onClick \_ ->
                case sentenceAnimation of
                  Ready -> NextSentence
                  Rendering -> NoOp
                  Showing -> FinishAnimation
                  Finished -> NextSentence
            ]
            [ HH.div
                [ HP.class_ $ HH.ClassName "w-full max-w-5xl text-4xl text-white break-all select-none "
                ]
                ( mapWithIndex
                    ( \i c -> HH.span
                        [ HP.style
                            ( case sentenceAnimation of
                                Ready -> ""
                                Rendering -> "opacity: 0;"
                                Showing ->
                                  "transition-timing-function: ease-in; "
                                    <> (if speedRate /= 1.0 then "transition-duration: 0.05s;" else "")
                                    <> " transition-delay: "
                                    <> (show $ maxShowSec * (1.0 - speedRate) * toNumber i)
                                    <> "s; opacity: 1;"
                                Finished -> "opacity: 1;"
                            )
                        ]
                        [ HH.text (fromCharArray [ c ]) ]
                    ) $ toCharArray sentence
                )
            ]
      ]
