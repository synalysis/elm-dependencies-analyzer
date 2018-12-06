module Compatible exposing
    ( CResult
    , State
    , debugHtml
    , initialState
    , initialStateVr
    , missingPackages
    , stepAllState
    , stepState
    )

import Cache exposing (Cache)
import Css as C
import Dict exposing (Dict)
import Dict.Extra as DictExtra
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import List.Extra as ListExtra
import Maybe.Extra as MaybeExtra
import RangeDict exposing (RangeDict)
import Result.Extra as ResultExtra
import Set exposing (Set)
import StepResult exposing (StepResult(..), endErr, endOk)
import Version exposing (Version, VersionId, VersionRange, VersionRangeX)



-- TYPES


type alias CResult a =
    StepResult String Bool a


{-| State in a process of solving a package compatibility problem.

    The problem is to determine whether it is possible to find a set of VersionId:s so that
      1) for each `required` package, one of its `versions` is selected
      2) all recursive dependencies of `required` packages are satisfied

  - `versions`
      - `Set Version` - possible versions of package
      - `Bool` - isRequired

  - `depends`
      - depends of VersionId:s, with possible dependsVersions

-}
type alias State =
    { versions : StateVersions
    , depends : StateDepends
    }


type alias StateVersions =
    Dict String ( Set Version, Bool )


type alias StateDepends =
    Dict VersionId (Dict String (Set Version))



-- INIT


initialState : Cache -> Dict String ( Version, Bool ) -> CResult State
initialState cache packages =
    packages
        |> Dict.map
            (\name ( version, isRequired ) ->
                ( Version.singletonRange version, isRequired )
            )
        |> initialStateVr cache


{-| Create initial state from list of packages and their possible version ranges.

    - entries with `True` are required packages
    - other entries limit possible versions of those packages

-}
initialStateVr : Cache -> Dict String ( VersionRange, Bool ) -> CResult State
initialStateVr cache packages =
    let
        emptyState =
            Continue
                { versions = Dict.empty
                , depends = Dict.empty
                }

        requiredPackages =
            packages
                |> Dict.toList
                |> List.filterMap
                    (\( name, ( versionRange, isRequired ) ) ->
                        if isRequired then
                            Just ( name, versionRange )

                        else
                            Nothing
                    )

        limitingPackages =
            packages
                |> Dict.toList
                |> List.filterMap
                    (\( name, ( versionRange, isRequired ) ) ->
                        if isRequired then
                            Nothing

                        else
                            Just ( name, versionRange )
                    )
    in
    List.foldl (addRequiredPackage cache) emptyState requiredPackages
        |> StepResult.andThen
            (\state ->
                Continue <|
                    List.foldl (limitPackageVersions cache) state limitingPackages
            )



-- STEP - HIGH LEVEL


{-| Do one step in a process of solving a package compatibility problem.

    TODO WIP

-}
stepState : Cache -> CResult State -> CResult State
stepState cache crState =
    StepResult.flippedAndThen crState <|
        \state ->
            case List.head <| missingPackages state of
                Just nextMissingPackage ->
                    Continue state
                        |> stepAddMissingPackage cache nextMissingPackage
                        |> stepPruneDependVersionsWithVersionsOfPackage nextMissingPackage
                        |> stepRemoveVersionIdsWithEmptyDependVersions

                Nothing ->
                    endErr "TODO"


{-| TODO WIP
-}
stepAllState : Cache -> CResult State -> CResult State
stepAllState cache crState =
    crState
        |> stepAddAllMissingPackages cache
        |> stepRemoveVersionIdsWithEmptyDependVersions
        |> StepResult.andThen
            (\state ->
                case missingPackages state of
                    [] ->
                        stepTraverse (Continue state)

                    _ ->
                        stepAllState cache (Continue state)
            )


{-| TODO WIP
TODO: simplify/cleanup code
-}
stepTraverse : CResult State -> CResult State
stepTraverse crState =
    let
        step : Dict String (Set Version) -> List String -> StateDepends -> Result String Bool
        step possibleVersions todo stateDepends =
            case todo of
                [] ->
                    let
                        -- check that possibleVersions has single version in each set, just to be sure
                        allHaveSingleVersion =
                            possibleVersions
                                |> Dict.values
                                |> List.all (\set -> Set.size set == 1)
                    in
                    if allHaveSingleVersion then
                        Ok True

                    else
                        Err "IMPOSSIBLE - not allHaveSingleVersion"

                nextTodo :: restTodo ->
                    case Dict.get nextTodo possibleVersions of
                        Nothing ->
                            Err "IMPOSSIBLE - nextTodo not in possibleVersions"

                        Just nextTodoVersions ->
                            let
                                tryVersion nextTodoVersion prevResult =
                                    -- keep trying possible versions until success or error
                                    case prevResult of
                                        Ok False ->
                                            case Dict.get ( nextTodo, nextTodoVersion ) stateDepends of
                                                Nothing ->
                                                    Err "IMPOSSIBLE - nextTodoId not in stateDepends"

                                                Just depends ->
                                                    let
                                                        -- only dependsName:s not yet seen should be added
                                                        newTodo =
                                                            depends
                                                                |> Dict.toList
                                                                |> List.filterMap
                                                                    (\( dependsName, _ ) ->
                                                                        if Dict.member dependsName possibleVersions then
                                                                            Nothing

                                                                        else
                                                                            Just dependsName
                                                                    )
                                                                |> List.append restTodo

                                                        crNewPossibleVersions : CResult (Dict String (Set Version))
                                                        crNewPossibleVersions =
                                                            depends
                                                                -- adding selected version as extra "depends",
                                                                -- so it doesn't need to be handled separately
                                                                |> Dict.insert nextTodo (Set.singleton nextTodoVersion)
                                                                |> Dict.foldl pvFolder (Continue possibleVersions)

                                                        pvFolder :
                                                            String
                                                            -> Set Version
                                                            -> CResult (Dict String (Set Version))
                                                            -> CResult (Dict String (Set Version))
                                                        pvFolder dependsName dependsVersions crAccum =
                                                            StepResult.flippedAndThen crAccum <|
                                                                \accum ->
                                                                    case Dict.get dependsName accum of
                                                                        Nothing ->
                                                                            Continue <| Dict.insert dependsName dependsVersions accum

                                                                        Just oldVersions ->
                                                                            let
                                                                                newVersions =
                                                                                    Set.intersect
                                                                                        oldVersions
                                                                                        dependsVersions
                                                                            in
                                                                            if Set.isEmpty newVersions then
                                                                                endOk False

                                                                            else
                                                                                Continue <| Dict.insert dependsName newVersions accum
                                                    in
                                                    case crNewPossibleVersions of
                                                        Continue newPossibleVersions ->
                                                            step newPossibleVersions newTodo stateDepends

                                                        End (Ok bool) ->
                                                            Ok bool

                                                        End (Err error) ->
                                                            Err error

                                        other ->
                                            other
                            in
                            -- try versions in reverse order, i.e. newest first
                            nextTodoVersions
                                |> Set.toList
                                |> List.reverse
                                |> List.foldl tryVersion (Ok False)
    in
    StepResult.flippedAndThen crState <|
        \state ->
            let
                possibleVersions =
                    getRequiredVersions state
                        |> Dict.fromList

                todo =
                    possibleVersions
                        |> Dict.keys
            in
            End <| step possibleVersions todo state.depends



-- STEP - MID LEVEL


stepAddAllMissingPackages : Cache -> CResult State -> CResult State
stepAddAllMissingPackages cache crState =
    StepResult.flippedAndThen crState <|
        \state ->
            case missingPackages state of
                [] ->
                    Continue state

                list ->
                    let
                        folder package =
                            stepAddMissingPackage cache package
                                >> stepPruneDependVersionsWithVersionsOfPackage package
                    in
                    List.foldl folder crState list



-- STEP - LOW LEVEL


{-| Add package found with `missingPackages`.

  - After this there might be some prunable dependVersions
      - see: stepPruneDependVersionsWithVersionsOfPackage

-}
stepAddMissingPackage : Cache -> String -> CResult State -> CResult State
stepAddMissingPackage cache missingName crState =
    StepResult.flippedAndThen crState <|
        \state ->
            let
                {- 1) For each package, get `union` of all dependVersions of missing package.
                   2) Then get intersect of those unions
                   TODO: perhaps simplify
                -}
                resultMaybeVersions : Result String (Maybe (Set Version))
                resultMaybeVersions =
                    getAllVersions state
                        |> List.map
                            (\( name, versions ) ->
                                versions
                                    |> Set.toList
                                    |> List.map
                                        (\version ->
                                            case Dict.get ( name, version ) state.depends of
                                                Nothing ->
                                                    Err "IMPOSSIBLE - addMissingPackage"

                                                Just depends ->
                                                    Ok <| Dict.get missingName depends
                                        )
                                    |> ResultExtra.combine
                                    |> Result.map (MaybeExtra.values >> ListExtra.foldl1 Set.union)
                            )
                        |> ResultExtra.combine
                        |> Result.map (MaybeExtra.values >> ListExtra.foldl1 Set.intersect)
            in
            case resultMaybeVersions of
                Err error ->
                    endErr error

                Ok Nothing ->
                    endErr "TODO - why Nothing? @ addMissingPackage"

                Ok (Just versions) ->
                    if Set.isEmpty versions then
                        endOk False

                    else
                        let
                            versionsList =
                                Set.toList versions

                            crNewVersions =
                                addToStateVersions missingName versionsList False state.versions

                            crNewDepends =
                                List.foldl
                                    (addToStateDepends cache missingName)
                                    (Continue state.depends)
                                    versionsList
                        in
                        StepResult.flippedAndThen2
                            crNewVersions
                            crNewDepends
                            (\newVersions newDepends ->
                                Continue
                                    { versions = newVersions
                                    , depends = newDepends
                                    }
                            )


{-| Prune dependVersions with possible versions of given package.

    - This can leave some VersionId:s with empty set of dependVersions.
      - see: stepRemoveVersionIdsWithEmptyDependVersions

-}
stepPruneDependVersionsWithVersionsOfPackage : String -> CResult State -> CResult State
stepPruneDependVersionsWithVersionsOfPackage pruneName crState =
    StepResult.flippedAndThen crState <|
        \state ->
            case Dict.get pruneName state.versions of
                Nothing ->
                    endErr <| pruneName ++ " is not in stateVersions"

                Just ( pruneVersions, _ ) ->
                    let
                        pruneOne : VersionId -> Dict String (Set Version) -> Dict String (Set Version)
                        pruneOne _ depends =
                            case Dict.get pruneName depends of
                                Nothing ->
                                    -- doesn't depend on pruneName -> not pruned
                                    depends

                                Just dependsVersions ->
                                    let
                                        intersectVersions =
                                            Set.intersect dependsVersions pruneVersions
                                    in
                                    if Set.size intersectVersions == Set.size dependsVersions then
                                        -- no change in versions -> not pruned
                                        depends

                                    else
                                        Dict.insert pruneName intersectVersions depends
                    in
                    Continue { state | depends = Dict.map pruneOne state.depends }


{-| Remove VersionId:s which have empty set of dependVersions for any of their depends.

    - This can leave some packages with empty set of possible versions.
      - see: TODO
    - This can leave some VersionId:s which are not needed anymore,
      i.e. not required nor depended on (recursively) by any required VersionId.
      - see: TODO

-}
stepRemoveVersionIdsWithEmptyDependVersions : CResult State -> CResult State
stepRemoveVersionIdsWithEmptyDependVersions crState =
    StepResult.flippedAndThen crState <|
        \state ->
            let
                versionIdsToRemove =
                    state.depends
                        |> Dict.toList
                        |> List.filterMap
                            (\( versionId, depends ) ->
                                if DictExtra.any (\_ versions -> Set.isEmpty versions) depends then
                                    Just versionId

                                else
                                    Nothing
                            )
                        |> Set.fromList
            in
            Continue
                { versions =
                    state.versions
                        |> Dict.map
                            (\name ( versions, isRequired ) ->
                                let
                                    newVersions =
                                        versions
                                            |> Set.filter
                                                (\version ->
                                                    not <| Set.member ( name, version ) versionIdsToRemove
                                                )
                                in
                                ( newVersions, isRequired )
                            )
                , depends =
                    state.depends
                        |> Dict.filter (\versionId _ -> not <| Set.member versionId versionIdsToRemove)
                }



-- QUERY STATE


{-| Get list of packages which are mentioned within depends, but have not been added yet.
-}
missingPackages : State -> List String
missingPackages state =
    let
        dependsPackages =
            state.depends
                |> Dict.values
                |> List.concatMap Dict.keys
                |> Set.fromList

        addedPackages =
            state.versions
                |> Dict.keys
                |> Set.fromList
    in
    Set.diff dependsPackages addedPackages
        |> Set.toList


getAllVersions : State -> List ( String, Set Version )
getAllVersions state =
    state.versions
        |> Dict.toList
        |> List.map (\( name, ( versions, _ ) ) -> ( name, versions ))


getRequiredVersions : State -> List ( String, Set Version )
getRequiredVersions state =
    state.versions
        |> Dict.toList
        |> List.filterMap
            (\( name, ( versions, isRequired ) ) ->
                if isRequired then
                    Just ( name, versions )

                else
                    Nothing
            )



-- INTERNAL - CHANGE STATE


{-| Add new required package.

NOTE: New required packages must not be added after solving has started.
TODO: enforce this

-}
addRequiredPackage : Cache -> ( String, VersionRange ) -> CResult State -> CResult State
addRequiredPackage cache ( name, versionRange ) crState =
    let
        crNewVersionList =
            versionRangeToVersionList cache name versionRange
                |> StepResult.fromResultContinue

        crNewVersions : State -> List Version -> CResult StateVersions
        crNewVersions state newVersionList =
            addToStateVersions name newVersionList True state.versions

        crNewDepends : State -> List Version -> CResult StateDepends
        crNewDepends state newVersionList =
            List.foldl
                (addToStateDepends cache name)
                (Continue state.depends)
                newVersionList
    in
    StepResult.flippedAndThen2
        crState
        crNewVersionList
        (\state newVersionList ->
            StepResult.flippedAndThen2
                (crNewVersions state newVersionList)
                (crNewDepends state newVersionList)
                (\newVersions newDepends ->
                    Continue
                        { versions = newVersions
                        , depends = newDepends
                        }
                )
        )


{-| Limit possible versions of given package with given range.

    - all required packages must be added before calling this

-}
limitPackageVersions : Cache -> ( String, VersionRange ) -> State -> State
limitPackageVersions cache ( limitName, limitVersionRange ) state =
    let
        limitVersionSet : Set Version -> Set Version
        limitVersionSet =
            Set.filter (\version -> Version.versionInRange version limitVersionRange)

        newStateVersions : StateVersions
        newStateVersions =
            case Dict.get limitName state.versions of
                Just ( oldVersions, isRequired ) ->
                    Dict.insert limitName ( limitVersionSet oldVersions, isRequired ) state.versions

                Nothing ->
                    state.versions

        newStateDepends : StateDepends
        newStateDepends =
            state.depends
                |> DictExtra.filterMap
                    (\( name, version ) depends ->
                        if name == limitName && not (Version.versionInRange version limitVersionRange) then
                            Nothing

                        else
                            Just <|
                                case Dict.get limitName depends of
                                    Just oldVersions ->
                                        Dict.insert limitName (limitVersionSet oldVersions) depends

                                    Nothing ->
                                        depends
                    )
    in
    { versions = newStateVersions
    , depends = newStateDepends
    }



-- INTERNAL - CHANGE STATE DEPENDS


{-| Add packages and its possible versions into StateVersions.
-}
addToStateVersions : String -> List Version -> Bool -> StateVersions -> CResult StateVersions
addToStateVersions name versions isRequired stateVersions =
    case Dict.get name stateVersions of
        Nothing ->
            Continue <| Dict.insert name ( Set.fromList versions, isRequired ) stateVersions

        Just _ ->
            endErr <| name ++ " is already in stateVersions"


{-| Add VersionId and its dependencies into StateDepends.

    VersionId is given as separate name/version for easier use with folding.

-}
addToStateDepends : Cache -> String -> Version -> CResult StateDepends -> CResult StateDepends
addToStateDepends cache name version crStateDepends =
    let
        versionId =
            ( name, version )
    in
    case crStateDepends of
        Continue stateDepends ->
            case Dict.get versionId stateDepends of
                Nothing ->
                    case dependsOfVersion cache versionId of
                        Ok depends ->
                            Continue <| Dict.insert versionId depends stateDepends

                        Err error ->
                            endErr error

                Just _ ->
                    endErr <| Version.idToStr versionId ++ " is already in stateDepends."

        end ->
            end



-- CONVERSIONS


dependsOfVersion : Cache -> VersionId -> Result String (Dict String (Set Version))
dependsOfVersion cache (( name, version ) as versionId) =
    case Dict.get versionId cache.depends of
        Nothing ->
            Err (Version.idToStr versionId ++ " is not in cache.depends")

        Just depends ->
            depends
                |> Dict.toList
                |> List.map
                    (\( dependsName, dependsVr ) ->
                        case versionRangeToVersionList cache dependsName dependsVr of
                            Ok versionList ->
                                Ok ( dependsName, Set.fromList versionList )

                            Err error ->
                                Err error
                    )
                |> ResultExtra.combine
                |> Result.map Dict.fromList


versionRangeToVersionList : Cache -> String -> VersionRange -> Result String (List Version)
versionRangeToVersionList cache name versionRange =
    case Dict.get name cache.versions of
        Nothing ->
            Err <| name ++ " is not in cache.versions"

        Just versions ->
            versions
                |> List.filterMap
                    (\( version, _ ) ->
                        if Version.versionInRange version versionRange then
                            Just version

                        else
                            Nothing
                    )
                |> Ok



-- DEBUG


debugHtml : CResult State -> Html msg
debugHtml crState =
    let
        nameVersionsToStr : String -> Set Version -> String
        nameVersionsToStr name versions =
            name
                ++ ": "
                ++ (if Set.isEmpty versions then
                        "NONE"

                    else
                        versions
                            |> Set.toList
                            |> List.map Version.versionToStr
                            |> String.join " "
                   )

        versionsHtml : State -> List (Html msg)
        versionsHtml state =
            state.versions
                |> Dict.toList
                |> List.map
                    (\( name, ( versions, isRequired ) ) ->
                        let
                            style =
                                if isRequired then
                                    [ A.css [ C.fontWeight C.bold ] ]

                                else
                                    []
                        in
                        H.div style [ H.text (nameVersionsToStr name versions) ]
                    )

        dependsSubHtml : Dict String (Set Version) -> List (Html msg)
        dependsSubHtml dict =
            dict
                |> Dict.toList
                |> List.map
                    (\( name, versions ) ->
                        H.li [] [ H.text (nameVersionsToStr name versions) ]
                    )

        dependsHtml state =
            state.depends
                |> Dict.toList
                |> List.map
                    (\( versionId, depends ) ->
                        H.li []
                            [ H.text <| Version.idToStr versionId
                            , H.ul [] (dependsSubHtml depends)
                            ]
                    )
    in
    case crState of
        End (Err error) ->
            H.div [] [ H.text <| "ERROR: " ++ error ]

        End (Ok True) ->
            H.div [] [ H.text <| "TRUE" ]

        End (Ok False) ->
            H.div [] [ H.text <| "FALSE" ]

        Continue state ->
            H.div []
                (versionsHtml state
                    ++ [ H.ul [] (dependsHtml state) ]
                    ++ [ H.text
                            ("Missing: "
                                ++ (missingPackages state |> String.join ", ")
                            )
                       ]
                )
