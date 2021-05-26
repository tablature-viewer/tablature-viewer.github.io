module LZString where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign (Foreign, readString)

foreign import unsafeDecompressFromEncodedURIComponent :: String -> Foreign
foreign import unsafeCompressToEncodedURIComponent :: String -> Foreign

decompressFromEncodedURIComponent :: String -> Maybe String
decompressFromEncodedURIComponent compressed = case runExcept ((unsafeDecompressFromEncodedURIComponent >>> readString) compressed) of
  Right decompressed -> Just decompressed
  _ -> Nothing

compressToEncodedURIComponent :: String -> Maybe String
compressToEncodedURIComponent value = case runExcept ((unsafeCompressToEncodedURIComponent >>> readString) value) of
  Right compressed -> Just compressed
  _ -> Nothing
