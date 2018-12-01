module Package exposing
    ( ExtraPackage
    , Package
    , PackageType(..)
    , PackageVersion
    )

import Cache exposing (FetchedValue(..))
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


type alias PackageVersion =
    { timestamp : Int
    , depends : FetchedValue (Dict String VersionRange)
    }


type PackageType
    = Direct
    | Indirect
    | Extra
