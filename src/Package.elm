module Package exposing
    ( ExtraPackage
    , Package
    , PackageType(..)
    )

import Dict exposing (Dict)
import Version exposing (Version, VersionRange)



-- TYPES


type alias Package =
    { isDirect : Bool
    , installedVersion : Version
    , selectedVersion : Version
    }


type alias ExtraPackage =
    { selectedVersion : Version
    }


type PackageType
    = Direct
    | Indirect
    | Extra
