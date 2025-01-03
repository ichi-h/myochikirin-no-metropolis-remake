module UI.Pages.Top.Page where

import Prelude

import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import UI.Store as Store

type State = { isAnimating :: Boolean }

data Action = ToNovel

component :: forall query input output m. MonadStore Store.Action Store.Store m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: forall i. i -> State
  initialState _ = { isAnimating: false }

  handleAction :: forall o. Action -> H.HalogenM State Action () o m Unit
  handleAction ToNovel = do
    H.modify_ \s -> s { isAnimating = true }
    H.liftAff $ delay $ Milliseconds $ 3000.0
    updateStore $ Store.Navigate Store.Novel

  render :: State -> H.ComponentHTML Action () m
  render { isAnimating } = HH.div
    [ HP.class_ $ H.ClassName ("relative w-full h-svh bg-top bg-cover duration-2000 " <> (if isAnimating then "opacity-0" else "cursor-pointer"))
    , HE.onClick \_ -> ToNovel
    ]
    [ HH.div
        [ HP.class_ $ H.ClassName "absolute top-0 w-full h-1/12 max-sm:h-20 max-lg:h-40 bg-gray-950"
        ]
        []
    , HH.div
        [ HP.class_ $ H.ClassName "absolute bottom-0 w-full h-1/12 max-sm:h-20 max-lg:h-40 bg-gray-950"
        ]
        []
    , HH.div
        [ HP.class_ $ H.ClassName "flex flex-col items-center justify-center gap-4 max-sm:gap-2 w-full h-full"
        ]
        [ HH.img
            [ HP.class_ $ H.ClassName "bg-title bg-no-repeat bg-contain w-256 max-lg:w-11/12 max-2xl:w-192 h-auto bg-center"
            , HP.src "assets/images/title.webp"
            ]
        , HH.div
            [ HP.class_ $ H.ClassName "text-gray-50 w-full text-center text-4xl max-xs:text-base max-sm:text-lg max-md:text-2xl max-2xl:text-3xl animate-blinking"
            ]
            [ HH.text "画面をクリック/タップしてください。" ]
        ]
    ]
