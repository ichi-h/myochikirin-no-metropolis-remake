module Utils.Logger where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import debugLog :: forall a. a -> Effect Unit
foreign import warnLog :: forall a. a -> Effect Unit
foreign import errorLog :: forall a. a -> Effect Unit

runExceptTWithLog :: forall m a b. MonadEffect m => ExceptT a m b -> m (Either a b)
runExceptTWithLog action = do
  result <- runExceptT action
  case result of
    Left e -> do
      liftEffect $ errorLog e
      pure result
    Right _ -> pure result
