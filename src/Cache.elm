module Cache exposing
    ( Cache
    , DependsCache
    , FetchedValue(..)
    , FetchingCache
    , FetchingDependsCache
    , FetchingPackageCache
    , newFetchingCache
    , validate
    )

import Dict exposing (Dict)
import Maybe.Extra as MaybeExtra
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



-- HELPERS


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
