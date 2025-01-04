module UI.Components.Router
  ( Action(..)
  , Slots
  , State(..)
  , _configPage
  , _galleryPage
  , _homePage
  , _novelPage
  , _topPage
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Type.Proxy (Proxy(..))
import UI.Capabilities.Audio (class Audio)
import UI.Pages.Config.Page as ConfigPage
import UI.Pages.Gallery.Page as GalleryPage
import UI.Pages.Home.Page as HomePage
import UI.Pages.Loading.Page as LoadingPage
import UI.Pages.Novel.Page as NovelPage
import UI.Pages.Top.Page as TopPage
import UI.Store as Store

data State = State { route :: Store.Route }

data Action = Receive (Connected Store.Route Unit)

type Slots =
  ( loadingPage :: forall query. H.Slot query Void Unit
  , topPage :: forall query. H.Slot query Void Unit
  , homePage :: forall query. H.Slot query Void Unit
  , novelPage :: forall query. H.Slot query Void Unit
  , configPage :: forall query. H.Slot query Void Unit
  , galleryPage :: forall query. H.Slot query Void Unit
  )

deriveState :: Connected Store.Route Unit -> State
deriveState { context } = State { route: context }

_loadingPage = Proxy :: Proxy "loadingPage"
_topPage = Proxy :: Proxy "topPage"
_homePage = Proxy :: Proxy "homePage"
_novelPage = Proxy :: Proxy "novelPage"
_configPage = Proxy :: Proxy "configPage"
_galleryPage = Proxy :: Proxy "galleryPage"

component
  :: forall q m
   . MonadStore Store.Action Store.Store m
  => Audio m
  => MonadAff m
  => H.Component q Unit Void m
component = connect (selectEq _.route) $ H.mkComponent
  { initialState: \{ context: route } -> State { route: route }
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
      H.put $ deriveState input

  render :: forall action. State -> H.ComponentHTML action Slots m
  render (State { route }) =
    case route of
      Store.Loading -> HH.slot_ _loadingPage unit LoadingPage.component unit
      Store.Home -> HH.slot_ _homePage unit HomePage.component unit
      Store.Top -> HH.slot_ _topPage unit TopPage.component unit
      Store.Novel -> HH.slot_ _novelPage unit NovelPage.component unit
      Store.Config -> HH.slot_ _configPage unit ConfigPage.component unit
      Store.Gallery -> HH.slot_ _galleryPage unit GalleryPage.component unit
