module Cache exposing
    ( Cache
    , DependsCache
    , FetchedMsg(..)
    , FetchedValue(..)
    , FetchingCache
    , FetchingDependsCache
    , FetchingPackageCache
    , allVersionsOfFetchingPackageCache
    , fetchNextThing
    , newFetchingCache
    , rangeDictOfDepends
    , validate
    )

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Json.Decode.Field as JF
import Maybe.Extra as MaybeExtra
import Misc exposing (Error(..), InternalError(..))
import Monocle.Common
import Monocle.Compose
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import Parser as P exposing ((|.), (|=))
import RangeDict exposing (RangeDict)
import Result.Extra as ResultExtra
import Set exposing (Set)
import Version exposing (Version, VersionId, VersionRange)



-- TYPES


{-| Unvalidated cache, possibly incomplete. Used while cache is being fetched.
-}
type alias FetchingCache =
    { packages : FetchingPackageCache
    , depends : FetchingDependsCache
    }


{-|

  - `allVersions`
      - All versions known for package, with release timestamps.
      - This can include Elm 0.18 versions which must not appear in elm.json

  - `minVersion`
      - minimum version to fetch & display
      - This is used to avoid including unneeded old versions which might
        not be compatible with Elm 0.19, and so would give 404 error when
        fetching elm.json of the version.

  - `minVersionIsJsonVersion`
      - if `True` then `minVersion` is from parsed elm.json, and will not be
        lowered during fetching even if some package depends on lower versions
          - if I wanted to support downgrading packages, then this would need
            to be changed, but for now this app is for upgrading packages
      - if `False` then `minVersion` is from lowest depends seen, and can be
        lowered further if some package depends on lower versions

-}
type alias FetchingPackageCache =
    Dict String
        { allVersions : FetchedValue (List ( Version, Int ))
        , minVersion : Version
        , minVersionIsJsonVersion : Bool
        }


type alias FetchingDependsCache =
    Dict VersionId (FetchedValue (Dict String VersionRange))


{-| Validated and complete cache.
-}
type alias Cache =
    { versions : Dict String (List ( Version, Int ))
    , depends : DependsCache
    }


{-| TODO: new type, use where I can
-}
type alias DependsCache =
    Dict VersionId (Dict String VersionRange)


type FetchedValue a
    = NotFetched
    | Succeeded a
    | Failed



-- TYPES - MSG


type FetchedMsg
    = FetchedVersions String (Result Http.Error (List ( Version, Int )))
    | FetchedDepends String Version (Result Http.Error (Dict String VersionRange))



-- MISC - SMALL


newFetchingCache : FetchingCache
newFetchingCache =
    { packages = Dict.empty
    , depends = Dict.empty
    }


fetchedValueToMaybe : FetchedValue a -> Maybe a
fetchedValueToMaybe status =
    case status of
        NotFetched ->
            Nothing

        Succeeded value ->
            Just value

        Failed ->
            Nothing



-- MISC


{-| Starting from set of initial VersionId:s, recursively find all dependencies using given version
of each package, keeping track of reverse dependencies.

  - `packages`
      - items with `True` are the initial VersionId:s
      - items with `False` are the VersionId:s which can be used during recursive search

  - on success, return reverse dependencies of found VersionId:s
      - see: Version.ReverseDepends

-}
reverseDependsFromSelected :
    DependsCache
    -> Dict String ( Version, Bool )
    -> Result InternalError Version.ReverseDepends
reverseDependsFromSelected dependsCache packages =
    let
        initialVersionIds =
            packages
                |> Dict.toList
                |> List.filterMap
                    (\( name, ( version, isInitial ) ) ->
                        if isInitial then
                            Just ( name, version )

                        else
                            Nothing
                    )

        initialTodo =
            initialVersionIds
                |> List.map (\id -> ( id, 0, Set.empty ))

        initialSeen =
            initialVersionIds
                |> List.map Tuple.first
                |> Set.fromList

        step :
            Set String
            -> List ( VersionId, Int, Set VersionId )
            -> Version.ReverseDepends
            -> Result InternalError Version.ReverseDepends
        step seen todo dict =
            case todo of
                [] ->
                    Ok dict

                ( ( name, version ), depth, immediateParents ) :: restTodo ->
                    case Dict.get ( name, version ) dict of
                        Just ( prevDepth, prevImmediateParents ) ->
                            if prevDepth == depth then
                                let
                                    newImmediateParents =
                                        Set.intersect prevImmediateParents immediateParents

                                    newDict =
                                        Dict.insert
                                            ( name, version )
                                            ( depth, newImmediateParents )
                                            dict
                                in
                                step seen restTodo newDict

                            else
                                Err <|
                                    OtherInternalError 8200 <|
                                        Version.idToStr ( name, version )
                                            ++ " has conflicting prevDepth"

                        Nothing ->
                            case Dict.get ( name, version ) dependsCache of
                                Nothing ->
                                    Err <| IdNotFound 4750 ( name, version )

                                Just depends ->
                                    let
                                        newDict =
                                            Dict.insert
                                                ( name, version )
                                                ( depth, immediateParents )
                                                dict

                                        newSeen =
                                            depends
                                                |> Dict.keys
                                                |> Set.fromList
                                                |> Set.union seen

                                        rAddonTodo =
                                            depends
                                                |> Dict.keys
                                                |> List.filter (\childName -> not <| Set.member childName seen)
                                                |> List.map
                                                    (\childName ->
                                                        case Dict.get childName packages of
                                                            Just ( childVersion, _ ) ->
                                                                let
                                                                    childImmediateParents =
                                                                        Set.singleton ( name, version )
                                                                in
                                                                Ok
                                                                    ( ( childName, childVersion )
                                                                    , depth + 1
                                                                    , childImmediateParents
                                                                    )

                                                            Nothing ->
                                                                Err <| NameNotFound 3606 childName
                                                    )
                                                |> ResultExtra.combine
                                    in
                                    case rAddonTodo of
                                        Err error ->
                                            Err error

                                        Ok addonTodo ->
                                            step newSeen (restTodo ++ addonTodo) newDict
    in
    step initialSeen initialTodo Dict.empty


{-| Create RangeDict from dependencies of VersionId:s found by reverseDependsFromSelected.
-}
rangeDictOfDepends : DependsCache -> Dict String ( Version, Bool ) -> Result InternalError RangeDict
rangeDictOfDepends dependsCache packages =
    case reverseDependsFromSelected dependsCache packages of
        Err error ->
            Err error

        Ok deps ->
            let
                rVrList =
                    deps
                        |> Dict.toList
                        |> List.map
                            (\( parentId, _ ) ->
                                case Dict.get parentId dependsCache of
                                    Just depends ->
                                        Dict.toList depends
                                            |> List.map (\( name, vr ) -> ( parentId, name, vr ))
                                            |> Ok

                                    Nothing ->
                                        Err <| IdNotFound 4317 parentId
                            )
                        |> ResultExtra.combine
                        |> Result.map List.concat

                mustContain =
                    deps
                        |> Dict.toList
                        |> List.map Tuple.first
            in
            case rVrList of
                Err error ->
                    Err error

                Ok vrList ->
                    -- TODO: combine these into a single function
                    vrList
                        |> RangeDict.fromVrList
                        |> RangeDict.insertMustContain mustContain
                        |> RangeDict.setReverseDepends deps
                        |> Ok



-- VALIDATE


{-| Validate consistency of FetchingCache and create Cache from it.

    Filters
    - versions according to minVersion

    Checks that
    - ... TODO ...

-}
validate : FetchingCache -> Result (List Error) Cache
validate fetchingCache =
    let
        -- check that sets are equal, i.e. have exact same members
        -- TODO: perhaps move to some utility package
        setEqual : Set comparable -> Set comparable -> Bool
        setEqual a b =
            (Set.size a == Set.size b) && (Set.size a == (Set.size <| Set.union a b))

        newVersionsWithErrors : ( Dict String (List ( Version, Int )), List Error )
        newVersionsWithErrors =
            fetchingCache.packages
                |> Dict.toList
                |> List.map
                    (\( name, value ) ->
                        case value.allVersions of
                            Succeeded versions ->
                                ( Just
                                    ( name
                                    , versions
                                        |> List.filter (\( version, _ ) -> version >= value.minVersion)
                                    )
                                , Nothing
                                )

                            NotFetched ->
                                ( Nothing
                                , Just <| InternalError <| NameNotFetched 1326 name
                                )

                            Failed ->
                                ( Nothing
                                , Just <| FetchingFailedWithName name
                                )
                    )
                |> List.unzip
                |> Tuple.mapBoth
                    (MaybeExtra.values >> Dict.fromList)
                    MaybeExtra.values

        newDependsWithErrors : ( DependsCache, List Error )
        newDependsWithErrors =
            fetchingCache.depends
                |> Dict.toList
                |> List.map
                    (\( parentId, fetchedDict ) ->
                        case fetchedDict of
                            Succeeded dict ->
                                ( Just
                                    ( parentId
                                    , dict
                                    )
                                , Nothing
                                )

                            NotFetched ->
                                ( Nothing
                                , Just <| InternalError <| IdNotFetched 9820 parentId
                                )

                            Failed ->
                                ( Nothing
                                , Just <| FetchingFailedWithId parentId
                                )
                    )
                |> List.unzip
                |> Tuple.mapBoth
                    (MaybeExtra.values >> Dict.fromList)
                    MaybeExtra.values

        -- Check that set of `VersionId`:s of `newVersions` match exactly those of `newDepends`
        validateVersions :
            Dict String (List ( Version, Int ))
            -> DependsCache
            -> Bool
        validateVersions newVersions newDepends =
            let
                idsFromNewVersions =
                    newVersions
                        |> Dict.toList
                        |> List.concatMap
                            (\( name, versionList ) ->
                                versionList |> List.map (\( version, _ ) -> ( name, version ))
                            )
                        |> Set.fromList

                idsFromNewDepends =
                    newDepends
                        |> Dict.keys
                        |> Set.fromList
            in
            setEqual idsFromNewVersions idsFromNewDepends

        -- Check that all childNames in `newDepends` are present in `newVersions`
        validateDependsNames :
            Dict String (List ( Version, Int ))
            -> DependsCache
            -> Bool
        validateDependsNames newVersions newDepends =
            newDepends
                |> Dict.values
                |> List.map
                    (Dict.keys
                        >> List.map (\childName -> Dict.member childName newVersions)
                        >> List.all identity
                    )
                |> List.all identity
    in
    -- at first stage, return all fetching errors, if any
    case ( newVersionsWithErrors, newDependsWithErrors ) of
        ( ( newVersions, [] ), ( newDepends, [] ) ) ->
            -- everything fetched, so now do other validations
            if not <| validateVersions newVersions newDepends then
                Err [ InternalError <| OtherInternalError 3863 "Cache.validateVersions failed" ]

            else if not <| validateDependsNames newVersions newDepends then
                Err [ InternalError <| OtherInternalError 8685 "Cache.validateDependsNames failed" ]

            else
                Ok
                    { versions = newVersions
                    , depends = newDepends
                    }

        ( ( _, errorsA ), ( _, errorsB ) ) ->
            Err (errorsA ++ errorsB)



-- HTTP


{-|

    All requests are cached by a PHP script I wrote for this project.

    1) query "AUTHOR/PROJECT" returns cached version of
       https://package.elm-lang.org/packages/AUTHOR/PROJECT/releases.json

    2) query "AUTHOR/PROJECT/VERSION" returns cached version of
       https://raw.githubusercontent.com/AUTHOR/PROJECT/VERSION/elm.json

    I'm using caching here to
    - work around CORS restriction (elm-lang.org isn't sending "Access-Control-Allow-Origin: *" header)
    - speed up slow queries (elm-lang.org often takes over 500ms per request)
    - enforce caching to reduce load on backend
    - learn about HTTP caching :)

-}
cacheUrl : String
cacheUrl =
    "https://www.markuslaire.com/github/elm-dependencies-analyzer/cache.php?"


fetchVersions : String -> Cmd FetchedMsg
fetchVersions name =
    Http.get
        { url = cacheUrl ++ name
        , expect = Http.expectJson (FetchedVersions name) packageVersionsDecoder
        }


fetchDepends : String -> Version -> Cmd FetchedMsg
fetchDepends name version =
    Http.get
        { url = cacheUrl ++ name ++ "/" ++ Version.versionToStr version
        , expect = Http.expectJson (FetchedDepends name version) packageDependenciesDecoder
        }


fetchNextVersions : FetchingPackageCache -> Maybe (Cmd FetchedMsg)
fetchNextVersions packageCache =
    packageCache
        |> Dict.toList
        |> List.filter (\( _, item ) -> item.allVersions == NotFetched)
        |> List.head
        |> Maybe.map (\( name, _ ) -> fetchVersions name)


fetchNextDepends : FetchingDependsCache -> Maybe (Cmd FetchedMsg)
fetchNextDepends dependsCache =
    dependsCache
        |> Dict.toList
        |> List.filter (\( _, fetched ) -> fetched == NotFetched)
        |> List.head
        |> Maybe.map (\( ( name, version ), _ ) -> fetchDepends name version)


fetchNextThing : FetchingCache -> Maybe (Cmd FetchedMsg)
fetchNextThing fetchingCache =
    case fetchNextDepends fetchingCache.depends of
        Just cmd ->
            Just cmd

        Nothing ->
            fetchNextVersions fetchingCache.packages



-- DECODERS


{-| decodes dependencies from package elm.json
-}
packageDependenciesDecoder : JD.Decoder (Dict String VersionRange)
packageDependenciesDecoder =
    JF.require "dependencies" (JD.keyValuePairs Version.versionRangeDecoder) <|
        (Dict.fromList >> JD.succeed)


{-| decodes <https://package.elm-lang.org/packages/AUTHOR/PROJECT/releases.json>
-}
packageVersionsDecoder : JD.Decoder (List ( Version, Int ))
packageVersionsDecoder =
    JD.keyValuePairs JD.int
        |> JD.andThen
            (\list ->
                let
                    decoder list_ =
                        case list_ of
                            [] ->
                                JD.succeed []

                            ( versionStr, timestamp ) :: rest ->
                                case P.run Version.versionStrParser (String.replace "." ":" versionStr) of
                                    Ok version ->
                                        decoder rest
                                            |> JD.andThen
                                                (\restDecoded ->
                                                    JD.succeed <| ( version, timestamp ) :: restDecoded
                                                )

                                    Err _ ->
                                        JD.fail "Invalid Version"
                in
                decoder list
            )



-- MONOCLE - OF FETCHING PACKAGE CACHE


allVersionsOfFetchingPackageCache : String -> Optional FetchingPackageCache (FetchedValue (List ( Version, Int )))
allVersionsOfFetchingPackageCache name =
    Monocle.Common.dict name
        |> Monocle.Compose.optionalWithLens
            (Lens .allVersions (\b a -> { a | allVersions = b }))
