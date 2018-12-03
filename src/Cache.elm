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

  - `allVersions`
      - All versions known for package, with release timestamps.
      - This can include Elm 0.18 versions which must not appear in elm.json

  - `minVersion`
      - minimum version to fetch & display
      - This is used to avoid including unneeded old versions which might
        not be compatible with Elm 0.19, and so would give 404 error when
        fetching elm.json of the version.

-}
type alias FetchingCache =
    { packages : FetchingPackageCache
    , depends : FetchingDependsCache
    }


type alias FetchingPackageCache =
    Dict String
        { allVersions : FetchedValue (List ( Version, Int ))
        , minVersion : Version
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


type alias PackageVersion =
    { timestamp : Int
    , depends : FetchedValue (Dict String VersionRange)
    }



-- TYPES - MSG


type FetchedMsg
    = FetchedVersions String (Result Http.Error (Dict Version PackageVersion))
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

  - on success, return Dict of found packages
      - For each package return (version, minDepth, immediateParents) where
          - version is the version given in `packages`
          - minDepth is minimum depth for this VersionId
              - initial VersionId:s have minDepth of 0,
                their immediate dependencies have minDepth of 1, etc.
          - immediateParents are the immediate reverse dependencies, at (minDepth - 1)

-}
dependsOfSelectedVersions :
    DependsCache
    -> Dict String ( Version, Bool )
    -> Result String (Dict String ( Version, Int, Set VersionId ))
dependsOfSelectedVersions dependsCache packages =
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
            -> Dict String ( Version, Int, Set VersionId )
            -> Result String (Dict String ( Version, Int, Set VersionId ))
        step seen todo dict =
            case todo of
                [] ->
                    Ok dict

                ( ( name, version ), depth, immediateParents ) :: restTodo ->
                    case Dict.get name dict of
                        Just ( prevVersion, prevDepth, prevImmediateParents ) ->
                            if prevVersion == version && prevDepth == depth then
                                let
                                    newImmediateParents =
                                        Set.intersect prevImmediateParents immediateParents

                                    newDict =
                                        Dict.insert name ( version, depth, newImmediateParents ) dict
                                in
                                step seen restTodo newDict

                            else
                                Err <| name ++ " has conflicting prevVersion/prevDepth (IMPOSSIBLE)"

                        Nothing ->
                            case Dict.get ( name, version ) dependsCache of
                                Nothing ->
                                    Err <| Version.idToStr ( name, version ) ++ " is not in dependsCache (IMPOSSIBLE)"

                                Just depends ->
                                    let
                                        newDict =
                                            Dict.insert name ( version, depth, immediateParents ) dict

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
                                                                Ok
                                                                    ( ( childName, childVersion )
                                                                    , depth + 1
                                                                    , Set.singleton ( name, version )
                                                                    )

                                                            Nothing ->
                                                                Err <| name ++ " is not in packages (INTERNAL ERROR)"
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


{-| Create RangeDict from dependencies of VersionId:s found by dependsOfSelectedVersions.
-}
rangeDictOfDepends : DependsCache -> Dict String ( Version, Bool ) -> Result String RangeDict
rangeDictOfDepends dependsCache packages =
    case dependsOfSelectedVersions dependsCache packages of
        Err error ->
            Err error

        Ok deps ->
            let
                rVrList =
                    deps
                        |> Dict.toList
                        |> List.map
                            (\( parentName, ( parentVersion, _, _ ) ) ->
                                case Dict.get ( parentName, parentVersion ) dependsCache of
                                    Just depends ->
                                        Dict.toList depends
                                            |> List.map
                                                (\( name, vr ) ->
                                                    ( ( parentName, parentVersion ), name, vr )
                                                )
                                            |> Ok

                                    Nothing ->
                                        let
                                            nameVerStr =
                                                parentName ++ " " ++ Version.versionToStr parentVersion
                                        in
                                        Err <| nameVerStr ++ " not found in dependsCache. (IMPOSSIBLE)"
                            )
                        |> ResultExtra.combine
                        |> Result.map List.concat

                mustContain =
                    deps
                        |> Dict.toList
                        |> List.map (\( name, ( version, _, _ ) ) -> ( name, version ))
            in
            case rVrList of
                Err error ->
                    Err error

                Ok vrList ->
                    vrList
                        |> RangeDict.fromVrList
                        |> RangeDict.insertMustContain mustContain
                        |> Ok



-- VALIDATE


{-| Validate consistency of FetchingCache and create Cache from it.

    Filters
    - versions according to minVersion

    Checks that
    - all FetchedValue:s are Succeeded
    - all `VersionId`:s of `versions` are present in `depends`
    - all names in `depends` are present in `versions`

-}
validate : FetchingCache -> Result (List String) Cache
validate fetchingCache =
    let
        newVersionsWithErrors : ( Dict String (List ( Version, Int )), List String )
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
                                , Just (name ++ " hasn't been fetched. (IMPOSSIBLE)")
                                )

                            Failed ->
                                ( Nothing
                                , Just (name ++ " fetching failed.")
                                )
                    )
                |> List.unzip
                |> Tuple.mapBoth
                    (MaybeExtra.values >> Dict.fromList)
                    MaybeExtra.values

        newDependsWithErrors : ( DependsCache, List String )
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
                                , Just
                                    (Version.idToStr parentId ++ " hasn't been fetched. (IMPOSSIBLE)")
                                )

                            Failed ->
                                ( Nothing
                                , Just (Version.idToStr parentId ++ " fetching failed.")
                                )
                    )
                |> List.unzip
                |> Tuple.mapBoth
                    (MaybeExtra.values >> Dict.fromList)
                    MaybeExtra.values

        -- Check that all `VersionId`:s of `newVersions` are present in `newDepends`
        validateVersions :
            Dict String (List ( Version, Int ))
            -> DependsCache
            -> Bool
        validateVersions newVersions newDepends =
            newVersions
                |> Dict.toList
                |> List.map
                    (\( name, versionList ) ->
                        versionList
                            |> List.map
                                (\( version, _ ) ->
                                    Dict.member ( name, version ) newDepends
                                )
                            |> List.all identity
                    )
                |> List.all identity

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
                Err [ "IMPOSSIBLE - Cache.validateVersions failed" ]

            else if not <| validateDependsNames newVersions newDepends then
                Err [ "IMPOSSIBLE - Cache.validateDependsNames failed" ]

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
packageVersionsDecoder : JD.Decoder (Dict Version PackageVersion)
packageVersionsDecoder =
    JD.keyValuePairs JD.int
        |> JD.andThen
            (\list ->
                let
                    decoder list_ =
                        case list_ of
                            [] ->
                                JD.succeed Dict.empty

                            ( versionStr, timestamp ) :: rest ->
                                case P.run Version.versionStrParser (String.replace "." ":" versionStr) of
                                    Ok version ->
                                        decoder rest
                                            |> JD.andThen
                                                (Dict.insert
                                                    version
                                                    { timestamp = timestamp
                                                    , depends = NotFetched
                                                    }
                                                    >> JD.succeed
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
