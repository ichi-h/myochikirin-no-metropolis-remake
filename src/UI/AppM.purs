module UI.AppM where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Safe.Coerce (coerce)
import UI.Store (Action, Store, reducer)

newtype AppM a = AppM (StoreT Action Store Aff a)

runAppM :: forall q i o. Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store reducer <<< coerce

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM
derive newtype instance monadAffAppM :: MonadAff AppM
