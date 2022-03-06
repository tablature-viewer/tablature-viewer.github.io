module JsonUtils where

import Data.Argonaut.Core (Json, caseJsonArray, caseJsonNumber, caseJsonObject, caseJsonString)
import Data.Maybe (Maybe(..))
import Foreign.Object (lookup)

child :: String -> Json -> Maybe Json
child name json = caseJsonObject Nothing (lookup name) json

array :: Json -> Maybe (Array Json)
array json = caseJsonArray Nothing Just json

string :: Json -> Maybe String
string json = caseJsonString Nothing Just json

number :: Json -> Maybe Number
number json = caseJsonNumber Nothing Just json
