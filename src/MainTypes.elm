module MainTypes exposing
    ( Model
    , Msg(..)
    , State(..)
    , allPackages
    , modifyIsDirectOfPackages
    , rangeDictOfDepends
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
import RangeDict exposing (RangeDict)
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


{-| Create RangeDict from depends of given versions of each package.

    - Returns list of errors if dependsCache doesn't contain needed values or given list is empty.

    TODO: maybe create new function getSelected to use new version more easily

-}
rangeDictOfDepends : Cache.DependsCache -> Dict String ( Version, Bool ) -> Result (List String) RangeDict
rangeDictOfDepends dependsCache packages =
    if Dict.isEmpty packages then
        Err [ "No packages selected" ]

    else
        let
            findAllPackages : List VersionId -> Dict String Version -> Dict String Version
            findAllPackages list dict =
                List.foldl folder dict list

            folder : VersionId -> Dict String Version -> Dict String Version
            folder ( parentName, parentVersion ) dict =
                case Dict.get parentName dict of
                    Just _ ->
                        dict

                    Nothing ->
                        let
                            newDict =
                                Dict.insert parentName parentVersion dict
                        in
                        case Dict.get ( parentName, parentVersion ) dependsCache of
                            Just depends ->
                                let
                                    newChildren =
                                        depends
                                            |> Dict.keys
                                            |> List.filter (\childName -> not <| Dict.member childName newDict)
                                            |> List.filterMap
                                                (\childName ->
                                                    case Dict.get childName packages of
                                                        Just ( selectedVersion, _ ) ->
                                                            Just ( childName, selectedVersion )

                                                        Nothing ->
                                                            -- TODO: ERROR
                                                            Nothing
                                                )
                                in
                                findAllPackages newChildren newDict

                            _ ->
                                -- TODO: ERROR
                                newDict

            allNeededPackages : List VersionId
            allNeededPackages =
                let
                    directPackages =
                        packages
                            |> Dict.toList
                            |> List.filterMap
                                (\( name, ( version, isDirect ) ) ->
                                    if isDirect then
                                        Just ( name, version )

                                    else
                                        Nothing
                                )
                in
                findAllPackages directPackages Dict.empty
                    |> Dict.toList

            ( maybeDepends, maybeErrors ) =
                allNeededPackages
                    |> List.map
                        (\( parentName, version ) ->
                            let
                                nameVerStr =
                                    parentName ++ " " ++ Version.versionToStr version
                            in
                            case Dict.get ( parentName, version ) dependsCache of
                                Just depends ->
                                    ( Dict.toList depends
                                        |> List.map
                                            (\( name, vr ) ->
                                                ( ( parentName, version ), name, vr )
                                            )
                                        |> Just
                                    , Nothing
                                    )

                                Nothing ->
                                    ( Nothing, Just (nameVerStr ++ " not found in dependsCache. (IMPOSSIBLE)") )
                        )
                    -- List (Maybe depends, Maybe errors)
                    |> List.unzip

            allDepends =
                List.filterMap identity maybeDepends

            allErrors =
                List.filterMap identity maybeErrors
        in
        case allErrors of
            [] ->
                allDepends
                    |> List.concat
                    >> RangeDict.fromVrList
                    >> RangeDict.insertMustContain allNeededPackages
                    |> Ok

            _ ->
                Err allErrors



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
