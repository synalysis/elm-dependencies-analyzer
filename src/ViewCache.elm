module ViewCache exposing
    ( ViewCache
    , new
    , updateWithMouseOverVersion
    , updateWithSelectedVersions
    , validateForView
    )

import Cache exposing (Cache)
import Compatible
import Dict exposing (Dict)
import Dict.Extra as DictExtra
import Misc exposing (InternalError(..), Package)
import StepResult
import Version exposing (Version, VersionId)



-- TYPES


{-| Caching to make view faster.
-}
type alias ViewCache =
    { pairIsCompatible : Dict ( VersionId, VersionId ) (Maybe Bool)
    , selectedVersionsAreCompatible : Maybe Bool
    , isCompatibleWithSelected : Dict VersionId (Maybe Bool)
    }



-- BUILD


new : Dict String Package -> Cache -> ViewCache
new packages cache =
    { pairIsCompatible = Dict.empty
    , selectedVersionsAreCompatible = Nothing
    , isCompatibleWithSelected = Dict.empty
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
                                        |> List.map
                                            (\( name, version ) ->
                                                ( name, ( version, True ) )
                                            )
                                        |> Dict.fromList
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
updateWithSelectedVersions : Dict String Package -> Cache -> ViewCache -> ViewCache
updateWithSelectedVersions packages cache viewCache =
    let
        selectedVersions =
            packages
                |> Dict.map (\_ package -> ( package.selectedVersion, Misc.isDirect package ))

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
                                            selectedVersions
                                                |> Dict.insert name ( version, True )
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
        | isCompatibleWithSelected = isCompatibleWithDirect
        , selectedVersionsAreCompatible =
            selectedVersions
                |> Compatible.initialState cache
                |> Compatible.stepAllState cache
                |> crStateToMaybeBool
    }



-- CHECK


{-| Check that `ViewCache` has everything needed for next `view`.

This is called from `updatePreView`, when Model has been updated and
everything should be ready for next `view`.

-}
validateForView : ViewCache -> Maybe InternalError
validateForView viewCache =
    if viewCache.selectedVersionsAreCompatible == Nothing then
        Just <| OtherInternalError 3917 "selectedVersionsAreCompatible == Nothing"

    else
        Just <| OtherInternalError 1120 "TODO"



-- INTERNAL HELPERS


crStateToMaybeBool : Compatible.CResult Compatible.State -> Maybe Bool
crStateToMaybeBool crState =
    case crState of
        StepResult.End (Ok bool) ->
            Just bool

        _ ->
            Nothing
