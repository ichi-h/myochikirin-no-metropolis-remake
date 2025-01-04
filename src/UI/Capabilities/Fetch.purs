module UI.Capabilities.Fetch where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either)
import Domain.Exceptions.AppError (AppError)
import Halogen as H

class Monad m <= Fetch m where
  fetchBlob :: String -> m (Either AppError ArrayBuffer)

instance fetchHalogenM :: Fetch m => Fetch (H.HalogenM st act slots msg m) where
  fetchBlob = lift <<< fetchBlob
