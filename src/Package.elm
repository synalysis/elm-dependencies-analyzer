module Package exposing (Package)

import Dict exposing (Dict)
import Version exposing (Version, VersionRange)



-- TYPES


type alias Package =
    { isDirect : Bool
    , jsonVersion : Maybe Version
    , selectedVersion : Version
    }
