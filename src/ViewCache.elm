module ViewCache exposing
    ( ViewCache
    , new
    , updateWithMouseOverVersion
    , updateWithSelectedVersions
    )

import Cache exposing (Cache)
import Compatible
import Dict exposing (Dict)
import SortableDict exposing (SortableDict)
import StepResult
import Version exposing (Version, VersionId)



-- TYPES


{-| Copied from Types.elm

    TODO: handle this better

-}
type alias Package =
    { isDirect : Bool
    , installedVersion : Version
    , selectedVersion : Version
    }


{-| Caching to make view faster.
-}
type alias ViewCache =
    { pairIsCompatible : Dict ( VersionId, VersionId ) (Maybe Bool)
    , directPackagesAreCompatible : Maybe Bool
    , isCompatibleWithDirect : Dict VersionId (Maybe Bool)
    }



-- BUILD


new : SortableDict String Package -> Cache -> ViewCache
new packages cache =
    { pairIsCompatible = Dict.empty
    , directPackagesAreCompatible = Nothing
    , isCompatibleWithDirect = Dict.empty
    }
        |> updateWithSelectedVersions packages cache



-- UPDATE


{-| This needs to be called when mouseOverVersion changes to something else than Nothing.
-}
updateWithMouseOverVersion : VersionId -> Cache -> ViewCache -> ViewCache
updateWithMouseOverVersion mouseOverVersionId cache viewCache =
    let
        newPairIsCompatible =
            let
                folder versionId accum =
                    case Dict.get ( versionId, mouseOverVersionId ) accum of
                        Just _ ->
                            accum

                        Nothing ->
                            let
                                maybeBool =
                                    [ versionId, mouseOverVersionId ]
                                        |> Compatible.initialState cache
                                        |> Compatible.stepAllState cache
                                        |> crStateToMaybeBool
                            in
                            accum
                                |> Dict.insert ( versionId, mouseOverVersionId ) maybeBool
                                |> Dict.insert ( mouseOverVersionId, versionId ) maybeBool
            in
            cache.versions
                |> Dict.toList
                |> List.concatMap
                    (\( name, versions ) ->
                        versions |> List.map (\( version, _ ) -> ( name, version ))
                    )
                |> List.foldl folder viewCache.pairIsCompatible
    in
    { viewCache | pairIsCompatible = newPairIsCompatible }


{-| This needs to be called when the set of (selected versions of isDirect packages) changes.
-}
updateWithSelectedVersions : SortableDict String Package -> Cache -> ViewCache -> ViewCache
updateWithSelectedVersions packages cache viewCache =
    let
        directPackages =
            packages
                |> SortableDict.toList
                |> List.filterMap
                    (\( nameInFilter, package ) ->
                        if package.isDirect then
                            Just ( nameInFilter, package.selectedVersion )

                        else
                            Nothing
                    )
                |> Dict.fromList

        isCompatibleWithDirect =
            cache.versions
                |> Dict.toList
                |> List.concatMap
                    (\( name, versions ) ->
                        versions
                            |> List.map
                                (\( version, _ ) ->
                                    let
                                        maybeBool =
                                            directPackages
                                                |> Dict.insert name version
                                                |> Dict.toList
                                                |> Compatible.initialState cache
                                                |> Compatible.stepAllState cache
                                                |> crStateToMaybeBool
                                    in
                                    ( ( name, version ), maybeBool )
                                )
                    )
                |> Dict.fromList
    in
    { viewCache
        | isCompatibleWithDirect = isCompatibleWithDirect
        , directPackagesAreCompatible =
            directPackages
                |> Dict.toList
                |> Compatible.initialState cache
                |> Compatible.stepAllState cache
                |> crStateToMaybeBool
    }



-- INTERNAL HELPERS


crStateToMaybeBool : Compatible.CResult Compatible.State -> Maybe Bool
crStateToMaybeBool crState =
    case crState of
        StepResult.End (Ok bool) ->
            Just bool

        _ ->
            Nothing
