module Misc exposing (Package)

import Dict exposing (Dict)
import Version exposing (Version, VersionRange)



-- TYPES


{-|

  - `initialState`
      - State of the package in parsed elm.json.

-}
type alias Package =
    { isDirect : Bool
    , selectedVersion : Version
    , initialState :
        Maybe
            { isDirect : Bool
            , version : Version
            }
    }
