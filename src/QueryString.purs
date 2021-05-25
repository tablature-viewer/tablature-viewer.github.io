module QueryString where

import Prelude (Unit)

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Foreign.Object as Object

foreign import getQueryString :: Effect String
foreign import setQueryString :: String -> Effect Unit
foreign import getRawQueryString :: Effect String
foreign import setRawQueryString :: String -> Effect Unit
foreign import setQueryParameters :: EffectFn1 (Object.Object String) Unit
