module LocationString where

import Prelude (Unit)

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Foreign.Object as Object

foreign import getLocationString :: Effect String
foreign import getLocationBaseString :: Effect String
foreign import getFragmentString :: Effect String
foreign import setFragmentString :: String -> Effect Unit
foreign import getRawFragmentString :: Effect String
foreign import setRawFragmentString :: String -> Effect Unit
foreign import setFragmentParameters :: EffectFn1 (Object.Object String) Unit
