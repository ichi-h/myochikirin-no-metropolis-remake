module Domain.Values.Audio.Samples
  ( Samples(..)
  , maxSamples
  ) where

newtype Samples = Samples Int

maxSamples :: Samples
maxSamples = Samples 2147483647
