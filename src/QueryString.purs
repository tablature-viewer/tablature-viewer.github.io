module QueryString where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign.Object as Object
import URI.Extra.QueryPairs

foreign import getRawQueryString :: Effect String
foreign import setRawQueryString :: String -> Effect Unit
foreign import setQueryParameters :: EffectFn1 (Object.Object String) Unit
