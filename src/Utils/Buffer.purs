module Utils.Buffer where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Effect.Aff (Aff)
import Promise (Promise)
import Promise.Aff (toAff)

data AudioBuffer = Foreign

foreign import array2AudioBufferImpl :: ArrayBuffer -> Promise AudioBuffer

array2AudioBuffer :: ArrayBuffer -> Aff AudioBuffer
array2AudioBuffer array = do
  arrayBuffer <- toAff $ array2AudioBufferImpl array
  pure arrayBuffer
