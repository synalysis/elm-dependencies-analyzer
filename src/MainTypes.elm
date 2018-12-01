module MainTypes exposing
    ( Model
    , Msg(..)
    , State(..)
    , allPackages
    , modifyIsDirectOfPackages
    , selectedVersionOfExtraPackages
    , selectedVersionOfPackages
    )

import Cache exposing (Cache)
import Dict exposing (Dict)
import File exposing (File)
import Html.Styled as H exposing (Html)
import Http
import Monocle.Common
import Monocle.Compose
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import Package exposing (ExtraPackage, Package, PackageType)
import SortableDict exposing (SortableDict)
import Version exposing (Version, VersionId, VersionRange, VersionRangeX)
import ViewCache exposing (ViewCache)



-- MODEL


type State
    = NothingAnalyzed
    | JsonParsingError String
    | Fetching Int
    | FetchingSucceeded Cache ViewCache
    | FetchingFailed (List (Html Msg))


type alias Model =
    { state : State
    , inputJson : String
    , mouseOverVersion : Maybe VersionId
    , packages : SortableDict String Package
    , extraPackages : Dict String ExtraPackage

    -- WIP
    , fetchingCache : Cache.FetchingCache
    }



-- MSG


type Msg
    = InputJsonChanged String
    | ExampleClick
    | OpenFileClick
    | FileOpened File
    | GotFileContents String
    | AnalyzeButtonClick
    | Fetched Cache.FetchedMsg
    | VersionClick PackageType String Version
    | IsDirectCheckboxClick String
    | MouseOverVersion String Version
    | MouseOutVersion String Version



-- FUNCTIONS


{-| TODO: maybe better name
-}
allPackages : Model -> Dict String ( Version, Bool )
allPackages model =
    let
        packagesDict =
            model.packages
                -- TODO: I need SortableDict.toDict
                |> SortableDict.toList
                |> Dict.fromList
                |> Dict.map
                    (\_ package ->
                        ( package.selectedVersion, package.isDirect )
                    )

        extraPackagesDict =
            model.extraPackages
                |> Dict.map
                    (\_ extraPackage ->
                        ( extraPackage.selectedVersion, False )
                    )
    in
    Dict.union packagesDict extraPackagesDict



-- MONOCLE - OF PACKAGES


selectedVersionOfPackages : String -> Optional (SortableDict String Package) Version
selectedVersionOfPackages name =
    SortableDict.valueOfSortableDict name
        |> Monocle.Compose.optionalWithLens
            (Lens .selectedVersion (\b a -> { a | selectedVersion = b }))


isDirectOfPackages : String -> Optional (SortableDict String Package) Bool
isDirectOfPackages name =
    SortableDict.valueOfSortableDict name
        |> Monocle.Compose.optionalWithLens
            (Lens .isDirect (\b a -> { a | isDirect = b }))


modifyIsDirectOfPackages :
    String
    -> (Bool -> Bool)
    -> SortableDict String Package
    -> SortableDict String Package
modifyIsDirectOfPackages name fn =
    Monocle.Optional.modify (isDirectOfPackages name) fn



-- MONOCLE - OF EXTRA PACKAGES


selectedVersionOfExtraPackages : String -> Optional (Dict String ExtraPackage) Version
selectedVersionOfExtraPackages name =
    Monocle.Common.dict name
        |> Monocle.Compose.optionalWithLens
            (Lens .selectedVersion (\b a -> { a | selectedVersion = b }))
