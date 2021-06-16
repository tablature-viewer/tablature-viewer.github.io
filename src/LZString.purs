module LZString where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Foreign (Foreign, readString)

-- TODO turn this into a pursuit module
foreign import unsafeDecompressFromEncodedURIComponent :: String -> Foreign
foreign import unsafeCompressToEncodedURIComponent :: String -> Foreign

compressToEncodedURIComponent :: String -> Maybe String
compressToEncodedURIComponent value =
  case unsafeCompressToEncodedURIComponent value # readString # runExcept of
    Right compressed -> Just compressed
    _ -> Nothing

decompressFromEncodedURIComponent :: String -> Maybe String
decompressFromEncodedURIComponent compressed =
  case unsafeDecompressFromEncodedURIComponent compressed # readString # runExcept of
    Right decompressed -> Just decompressed
    _ -> Nothing
