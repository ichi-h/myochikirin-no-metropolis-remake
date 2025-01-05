module UI.Components.Router where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore)
import Type.Proxy (Proxy(..))
import UI.Capabilities.Audio (class Audio)
import UI.Components.Layouts.Home as HomeLayout
import UI.Pages.Loading.Page as LoadingPage
import UI.Pages.Novel.Page as NovelPage
import UI.Pages.Top.Page as TopPage
import UI.Store as Store

type State = { route :: Store.Route, isNavigating :: Boolean }

data Action = Receive (Store.RouteReceive Unit)

type Slots =
  ( loadingPage :: forall query. H.Slot query Void Unit
  , topPage :: forall query. H.Slot query Void Unit
  , novelPage :: forall query. H.Slot query Void Unit
  , homeLayout :: HomeLayout.HomeLayoutSlot
  )

_loadingPage = Proxy :: Proxy "loadingPage"
_topPage = Proxy :: Proxy "topPage"
_novelPage = Proxy :: Proxy "novelPage"
_homeLayout = Proxy :: Proxy "homeLayout"

component
  :: forall q m
   . MonadStore Store.Action Store.Store m
  => Audio m
  => MonadAff m
  => H.Component q Unit Void m
component = Store.connectRoute $ H.mkComponent
  { initialState: \{ context: route } -> { route: route, isNavigating: false }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  handleAction :: forall o. Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Receive input -> do
      let route = Store.deriveRoute input
      H.modify_ \s -> s { route = route }

  render :: State -> H.ComponentHTML Action Slots m
  render { route } =
    case route of
      Store.Loading -> HH.slot_ _loadingPage unit LoadingPage.component unit
      Store.Top -> HH.slot_ _topPage unit TopPage.component unit
      Store.Novel novelTitle -> HH.slot_ _novelPage unit NovelPage.component { novelTitle }
      _ -> HH.slot_ _homeLayout unit HomeLayout.component unit
