module Domain.Exceptions.AppError
  ( AppError(..)
  , mapError
  ) where

import Affjax as AX
import Data.Either (Either(..))

data AppError
  = FetchError AX.Error
  | AudioError String

mapError :: forall a c. (a -> AppError) -> Either a c -> Either AppError c
mapError _ (Right b) = Right b
mapError f (Left a) = Left (f a)
