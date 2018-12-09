module Main exposing (main)

import Browser
import Cache exposing (Cache, FetchedValue(..))
import Compatible
import Css as C
import Dict exposing (Dict)
import Elm.Package
import Elm.Project
import Elm.Version
import File exposing (File)
import File.Select
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as ListExtra
import Maybe.Extra as MaybeExtra
import Misc exposing (InternalError(..), Package, PackageSolved, PackageStateSolved(..), PackageStateUnsolved(..))
import Monocle.Common
import Monocle.Compose
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import RangeDict exposing (RangeDict)
import Result.Extra as ResultExtra
import Set exposing (Set)
import StepResult
import Task
import Version exposing (Version, VersionId, VersionRange)
import ViewCache exposing (ViewCache)



-- TYPES


type alias Model =
    { state : ModelState
    , inputJson : String
    , mouseOverVersion : Maybe VersionId
    , packages : Dict String Package
    }


type ModelState
    = NothingAnalyzed
    | JsonParsingError String
    | Fetching Int Cache.FetchingCache
    | FetchingSucceeded Cache ViewCache
    | FetchingFailed (List (Html Msg))


type Msg
    = InputJsonChanged String
    | ExampleClick
    | OpenFileClick
    | FileOpened File
    | GotFileContents String
    | AnalyzeButtonClick
    | Fetched Cache.FetchedMsg
    | VersionClick String Version
    | IsDirectCheckboxClick String Bool
    | MouseOverVersion String Version
    | MouseOutVersion String Version



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view >> H.toUnstyled
        }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = NothingAnalyzed
      , inputJson = ""
      , mouseOverVersion = Nothing
      , packages = Dict.empty
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputJsonChanged inputJson ->
            ( { model | inputJson = inputJson }, Cmd.none )

        ExampleClick ->
            ( { model | inputJson = exampleJson }, Cmd.none )

        OpenFileClick ->
            ( model, File.Select.file [ "application/json" ] FileOpened )

        FileOpened file ->
            ( model, Task.perform GotFileContents (File.toString file) )

        GotFileContents contents ->
            ( { model | inputJson = contents }, Cmd.none )

        AnalyzeButtonClick ->
            updateAnalyzeButtonClick model

        Fetched fetched ->
            updateFetched model fetched

        VersionClick name version ->
            let
                newPackages =
                    model.packages
                        |> (selectedVersionOfPackages name).set version

                newState =
                    case model.state of
                        FetchingSucceeded cache viewCache ->
                            ViewCache.updateWithSelectedVersions newPackages cache viewCache
                                |> FetchingSucceeded cache

                        other ->
                            other
            in
            ( { model
                | state = newState
                , packages = newPackages
              }
            , Cmd.none
            )

        IsDirectCheckboxClick name isTestSection ->
            case model.state of
                FetchingSucceeded cache viewCache ->
                    let
                        newPackages =
                            case Dict.get name model.packages of
                                Nothing ->
                                    -- IMPOSSIBLE
                                    model.packages

                                Just package ->
                                    let
                                        newPackage =
                                            { package
                                                | state =
                                                    case ( Misc.isDirect package, isTestSection ) of
                                                        ( False, False ) ->
                                                            DirectNormal_

                                                        ( False, True ) ->
                                                            DirectTest_

                                                        ( True, _ ) ->
                                                            IndirectOrNotNeeded
                                            }
                                    in
                                    Dict.insert name newPackage model.packages

                        newState =
                            ViewCache.updateWithSelectedVersions newPackages cache viewCache
                                |> FetchingSucceeded cache
                    in
                    ( { model
                        | state = newState
                        , packages = newPackages
                      }
                    , Cmd.none
                    )

                _ ->
                    -- IMPOSSIBLE
                    ( model, Cmd.none )

        MouseOverVersion name version ->
            let
                newState =
                    case model.state of
                        FetchingSucceeded cache viewCache ->
                            ViewCache.updateWithMouseOverVersion ( name, version ) cache viewCache
                                |> FetchingSucceeded cache

                        other ->
                            other
            in
            ( { model
                | mouseOverVersion = Just ( name, version )
                , state = newState
              }
            , Cmd.none
            )

        MouseOutVersion name version ->
            if Just ( name, version ) == model.mouseOverVersion then
                ( { model | mouseOverVersion = Nothing }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


updateAnalyzeButtonClick : Model -> ( Model, Cmd Msg )
updateAnalyzeButtonClick model =
    case JD.decodeString Elm.Project.decoder model.inputJson of
        Err error ->
            ( { model
                | packages = Dict.empty
                , state = JsonParsingError (JD.errorToString error)
              }
            , Cmd.none
            )

        Ok (Elm.Project.Package _) ->
            ( { model
                | packages = Dict.empty
                , state =
                    JsonParsingError
                        ("Only application elm.json is supported, not package elm.json.\n"
                            ++ "If you would find it useful to also support package elm.json,\n"
                            ++ "please open an issue at GitHub repository."
                        )
              }
            , Cmd.none
            )

        Ok (Elm.Project.Application appInfo) ->
            let
                fromElmDeps :
                    Elm.Project.Deps Elm.Version.Version
                    -> PackageStateSolved
                    -> List ( String, Version, PackageStateSolved )
                fromElmDeps deps state =
                    deps
                        |> List.map
                            (\( name, elmVersion ) ->
                                ( Elm.Package.toString name
                                , Elm.Version.toTuple elmVersion
                                , state
                                )
                            )

                appDepends =
                    fromElmDeps appInfo.depsDirect DirectNormal
                        ++ fromElmDeps appInfo.depsIndirect IndirectNormal
                        ++ fromElmDeps appInfo.testDepsDirect DirectTest
                        ++ fromElmDeps appInfo.testDepsIndirect IndirectTest

                newPackageCache =
                    appDepends
                        |> List.map
                            (\( name, version, _ ) ->
                                ( name
                                , { allVersions = NotFetched
                                  , minVersion = version
                                  , minVersionIsJsonVersion = True
                                  }
                                )
                            )
                        |> Dict.fromList

                newPackages =
                    appDepends
                        |> List.map
                            (\( name, version, state ) ->
                                ( name
                                , { state =
                                        case state of
                                            DirectNormal ->
                                                DirectNormal_

                                            DirectTest ->
                                                DirectTest_

                                            IndirectNormal ->
                                                IndirectOrNotNeeded

                                            IndirectTest ->
                                                IndirectOrNotNeeded
                                  , selectedVersion = version
                                  , initialState =
                                        Just
                                            { state = state
                                            , version = version
                                            }
                                  }
                                )
                            )
                        |> Dict.fromList

                newFetchingCache =
                    { packages = newPackageCache
                    , depends = Dict.empty
                    }
                        |> addMissingVersionsToDependsCache

                ( newState, nextCmd ) =
                    case Cache.fetchNextThing newFetchingCache of
                        Just cmd ->
                            ( Fetching 0 newFetchingCache, cmd )

                        Nothing ->
                            case Cache.validate newFetchingCache of
                                Ok cache ->
                                    ( FetchingSucceeded cache (ViewCache.new newPackages cache)
                                    , Cmd.none
                                    )

                                Err errors ->
                                    ( errors
                                        |> List.map
                                            (\error ->
                                                H.li [] [ H.text <| Misc.errorToStr error ]
                                            )
                                        |> FetchingFailed
                                    , Cmd.none
                                    )
            in
            ( { model
                | packages = newPackages
                , state = newState
              }
            , Cmd.map Fetched nextCmd
            )


updateFetched :
    Model
    -> Cache.FetchedMsg
    -> ( Model, Cmd Msg )
updateFetched model fetched =
    case model.state of
        Fetching oldDone fetchingCache ->
            let
                newFetchingCache =
                    case fetched of
                        Cache.FetchedVersions name result ->
                            case result of
                                Err error ->
                                    { packages =
                                        fetchingCache.packages
                                            |> (Cache.allVersionsOfFetchingPackageCache name).set Failed
                                    , depends = fetchingCache.depends
                                    }

                                Ok packageVersions ->
                                    { packages =
                                        fetchingCache.packages
                                            |> (Cache.allVersionsOfFetchingPackageCache name).set
                                                (Succeeded packageVersions)
                                    , depends = fetchingCache.depends
                                    }
                                        |> addMissingVersionsToDependsCache

                        Cache.FetchedDepends name version result ->
                            case result of
                                Err error ->
                                    { packages = fetchingCache.packages
                                    , depends = Dict.insert ( name, version ) Failed fetchingCache.depends
                                    }

                                Ok versionRanges ->
                                    let
                                        addonPackageCache =
                                            versionRanges
                                                |> Dict.map
                                                    (\n ( min, max ) ->
                                                        { allVersions = NotFetched
                                                        , minVersion = min
                                                        , minVersionIsJsonVersion = False
                                                        }
                                                    )

                                        newPackageCache =
                                            Dict.merge
                                                Dict.insert
                                                (\mergeName old new ->
                                                    Dict.insert mergeName
                                                        { allVersions = old.allVersions
                                                        , minVersion =
                                                            if old.minVersionIsJsonVersion then
                                                                old.minVersion

                                                            else
                                                                min old.minVersion new.minVersion
                                                        , minVersionIsJsonVersion = old.minVersionIsJsonVersion
                                                        }
                                                )
                                                Dict.insert
                                                fetchingCache.packages
                                                addonPackageCache
                                                Dict.empty
                                    in
                                    { packages = newPackageCache
                                    , depends = Dict.insert ( name, version ) (Succeeded versionRanges) fetchingCache.depends
                                    }
                                        |> addMissingVersionsToDependsCache

                newPackages =
                    case fetched of
                        Cache.FetchedVersions name (Ok packageVersions) ->
                            if not (Dict.member name model.packages) then
                                case ListExtra.last <| List.map Tuple.first <| packageVersions of
                                    Just latestVersion ->
                                        Dict.insert
                                            name
                                            { state = IndirectOrNotNeeded
                                            , selectedVersion = latestVersion
                                            , initialState = Nothing
                                            }
                                            model.packages

                                    Nothing ->
                                        -- TODO IMPOSSIBLE
                                        model.packages

                            else
                                model.packages

                        _ ->
                            model.packages

                maybeNextCmd =
                    Cache.fetchNextThing newFetchingCache

                newDone =
                    oldDone + 1

                newState =
                    case maybeNextCmd of
                        Just _ ->
                            Fetching newDone newFetchingCache

                        Nothing ->
                            case Cache.validate newFetchingCache of
                                Ok cache ->
                                    FetchingSucceeded cache (ViewCache.new model.packages cache)

                                Err errors ->
                                    errors
                                        |> List.map
                                            (\error ->
                                                H.li [] [ H.text <| Misc.errorToStr error ]
                                            )
                                        |> FetchingFailed
            in
            ( { model
                | packages = newPackages
                , state = newState
              }
            , Cmd.map Fetched <| Maybe.withDefault Cmd.none maybeNextCmd
            )

        _ ->
            -- IMPOSSIBLE
            ( model, Cmd.none )



-- UPDATE - HELPERS


{-| Add versions present in packages but not in depends to depends as `NotFetched`, with `minVersion` filtering.
-}
addMissingVersionsToDependsCache : Cache.FetchingCache -> Cache.FetchingCache
addMissingVersionsToDependsCache fetchingCache =
    { fetchingCache
        | depends =
            fetchingCache.packages
                |> Dict.toList
                |> List.concatMap
                    (\( name, item ) ->
                        case item.allVersions of
                            Succeeded allVersions ->
                                allVersions
                                    |> List.filterMap
                                        (\( version, _ ) ->
                                            if version >= item.minVersion then
                                                Just ( ( name, version ), NotFetched )

                                            else
                                                Nothing
                                        )

                            NotFetched ->
                                []

                            Failed ->
                                []
                    )
                |> Dict.fromList
                |> Dict.union fetchingCache.depends
    }



-- VIEW


view : Model -> Html Msg
view model =
    H.table []
        [ H.tr []
            [ H.td [ A.colspan 2 ]
                [ H.text "See README at "
                , H.a
                    [ A.href "https://github.com/malaire/elm-dependencies-analyzer"
                    , A.target "_blank"
                    ]
                    [ H.text "GitHub repository" ]
                , H.text " for usage instructions."
                ]
            ]
        , H.tr []
            [ H.td [ A.css [ C.verticalAlign C.top ] ]
                (viewLeftSection model)
            , H.td
                [ A.css [ C.verticalAlign C.top ] ]
                (viewRightSection model)
            ]
        ]


viewLeftSection : Model -> List (Html Msg)
viewLeftSection model =
    [ H.textarea
        [ A.rows 40
        , A.cols 50
        , HE.onInput InputJsonChanged
        , A.property "value" (JE.string model.inputJson)
        ]
        []
    , H.br [] []
    , H.button [ HE.onClick OpenFileClick ] [ H.text "Open File ..." ]
    , H.text " "
    , H.button [ HE.onClick AnalyzeButtonClick ] [ H.text "Analyze" ]
    , H.hr [] []
    , H.button [ HE.onClick ExampleClick ] [ H.text "Load example" ]
    ]


viewRightSection : Model -> List (Html Msg)
viewRightSection model =
    case model.state of
        NothingAnalyzed ->
            [ H.text "Nothing analyzed yet." ]

        JsonParsingError error ->
            [ H.pre [] [ H.text (String.replace "\\n" "\n" error) ] ]

        Fetching done _ ->
            [ H.text ("Fetching package data ... " ++ String.fromInt done) ]

        FetchingFailed errors ->
            [ H.ul [] errors ]

        FetchingSucceeded cache viewCache ->
            viewRightSectionWhenFetchingSucceeded model cache viewCache


viewRightSectionWhenFetchingSucceeded : Model -> Cache -> ViewCache -> List (Html Msg)
viewRightSectionWhenFetchingSucceeded model cache viewCache =
    let
        rSolved : Result InternalError ( Dict String PackageSolved, RangeDict )
        rSolved =
            solveIndirectPackages cache model.packages
    in
    case rSolved of
        Err error ->
            [ H.ul [] [ H.li [] [ H.text <| Misc.internalErrorToStr error ] ] ]

        Ok ( solvedPackages, allDeps ) ->
            let
                problems =
                    RangeDict.getProblems allDeps

                isInternalInconsistency =
                    case viewCache.selectedVersionsAreCompatible of
                        Nothing ->
                            False

                        Just bool ->
                            (problems == []) /= bool

                rPackagesHtml =
                    viewPackages model.mouseOverVersion cache viewCache solvedPackages
            in
            case rPackagesHtml of
                Err error ->
                    [ H.div [] [ H.text <| Misc.internalErrorToStr error ] ]

                Ok packagesHtml ->
                    [ packagesHtml ]
                        ++ (case problems of
                                [] ->
                                    [ H.div
                                        [ A.css [ C.color (C.hex "080") ] ]
                                        [ H.text "Selected versions have no conflicts."
                                        ]
                                    ]

                                nonEmptyProblems ->
                                    [ H.div
                                        [ A.css [ C.color (C.hex "800") ] ]
                                        [ H.text "Selected versions have dependency conflicts:"
                                        , H.ul [] nonEmptyProblems
                                        ]
                                    ]
                           )
                        ++ (if isInternalInconsistency then
                                [ H.div [] [ H.text "ERROR: Internal inconsistency detected." ] ]

                            else
                                []
                           )


viewPackages :
    Maybe VersionId
    -> Cache
    -> ViewCache
    -> Dict String PackageSolved
    -> Result InternalError (Html Msg)
viewPackages mouseOverVersion cache viewCache packagesSolved =
    let
        allPackagesToShow =
            packagesSolved
                |> Dict.toList
                |> List.filterMap
                    (\( name, package ) ->
                        -- packages of parsed elm.json are shown in sections according to
                        -- initialState ; new packages according to current state
                        case package.initialState of
                            Just initialState ->
                                Just ( name, package, initialState.state )

                            Nothing ->
                                case package.state of
                                    Just state ->
                                        Just ( name, package, state )

                                    Nothing ->
                                        Nothing
                    )
                |> List.sortWith
                    (\( nameA, packageA, _ ) ( nameB, packageB, _ ) ->
                        -- sort new packages last
                        case ( packageA.initialState, packageB.initialState ) of
                            ( Just _, Nothing ) ->
                                LT

                            ( Nothing, Just _ ) ->
                                GT

                            ( Just _, Just _ ) ->
                                compare nameA nameB

                            ( Nothing, Nothing ) ->
                                compare nameA nameB
                    )

        showSection : PackageStateSolved -> List (Result InternalError (Html Msg))
        showSection section =
            (allPackagesToShow
                |> List.filter (\( _, _, section_ ) -> section_ == section)
                |> List.map
                    (\( name, package, _ ) ->
                        viewPackage
                            { mouseOverVersion = mouseOverVersion
                            , cache = cache
                            , viewCache = viewCache
                            , isTestSection =
                                section == DirectTest || section == IndirectTest
                            , package = package
                            , name = name
                            }
                    )
            )
                ++ [ Ok <| H.tr [] [ H.td [ A.colspan 4 ] [ H.hr [] [] ] ] ]

        rHtml : Result InternalError (List (Html Msg))
        rHtml =
            showSection DirectNormal
                ++ showSection IndirectNormal
                ++ showSection DirectTest
                ++ showSection IndirectTest
                |> ResultExtra.combine
    in
    case rHtml of
        Err error ->
            Err error

        Ok html ->
            Ok <| H.table [] html


viewPackage :
    { mouseOverVersion : Maybe VersionId
    , cache : Cache
    , viewCache : ViewCache
    , isTestSection : Bool
    , package : PackageSolved
    , name : String
    }
    -> Result InternalError (Html Msg)
viewPackage { mouseOverVersion, cache, viewCache, isTestSection, package, name } =
    let
        packageLink =
            if String.left 8 name == "example/" then
                H.span [] [ H.text "src" ]

            else
                H.a
                    [ A.href ("https://github.com/" ++ name)
                    , A.target "_blank"
                    , A.css [ C.textDecoration C.none ]
                    ]
                    [ H.text "src" ]

        isTestHtml =
            if package.state == Just DirectTest || package.state == Just IndirectTest then
                H.span [ A.css [ C.color (C.hex "00A"), C.fontSize C.smaller ] ]
                    [ H.text "TEST" ]

            else
                H.span [] []

        isDirect =
            package.state == Just DirectNormal || package.state == Just DirectTest

        nameStyleColor =
            if isDirect then
                []

            else
                [ C.color (C.hex "#888") ]

        nameStyleStrike =
            if package.state == Nothing then
                [ C.textDecoration3 C.lineThrough C.solid (C.hex "#000") ]

            else
                []

        nameStyle =
            nameStyleColor ++ nameStyleStrike

        rPackageVersionsHtml =
            viewPackageVersions
                { mouseOverVersion = mouseOverVersion
                , cache = cache
                , viewCache = viewCache
                , package = package
                , name = name
                }
    in
    case rPackageVersionsHtml of
        Err error ->
            Err error

        Ok packageVersionsHtml ->
            Ok <|
                H.tr []
                    ([ H.td [] [ packageLink ]
                     , H.td []
                        [ H.input
                            [ A.type_ "checkbox"
                            , A.checked isDirect
                            , HE.on "change" (JD.succeed (IsDirectCheckboxClick name isTestSection))
                            ]
                            []
                        ]
                     , H.td [] [ isTestHtml ]
                     , H.td []
                        ((if package.initialState == Nothing then
                            [ H.span [ A.css [ C.color (C.hex "00A"), C.fontSize C.smaller ] ]
                                [ H.text "NEW " ]
                            ]

                          else
                            []
                         )
                            ++ [ H.span [ A.css nameStyle ] [ H.text name ] ]
                        )
                     ]
                        ++ packageVersionsHtml
                    )


viewPackageVersions :
    { mouseOverVersion : Maybe VersionId
    , cache : Cache
    , viewCache : ViewCache
    , package : PackageSolved
    , name : String
    }
    -> Result InternalError (List (Html Msg))
viewPackageVersions { mouseOverVersion, cache, viewCache, package, name } =
    case Dict.get name cache.versions of
        Just versions ->
            versions
                |> List.map
                    (\( version, _ ) ->
                        viewVersion
                            { mouseOverVersion = mouseOverVersion
                            , cache = cache
                            , viewCache = viewCache
                            , package = package
                            , name = name
                            , version = version
                            }
                    )
                |> ResultExtra.combine

        Nothing ->
            Err <| NameNotFound 6754 name


viewVersion :
    { mouseOverVersion : Maybe VersionId
    , cache : Cache
    , viewCache : ViewCache
    , package : PackageSolved
    , name : String
    , version : Version
    }
    -> Result InternalError (Html Msg)
viewVersion { mouseOverVersion, cache, viewCache, package, name, version } =
    let
        versionHasMouseOver =
            mouseOverVersion == Just ( name, version )

        packageHasMouseOver =
            case mouseOverVersion of
                Just ( moName, _ ) ->
                    moName == name

                Nothing ->
                    False

        isCompatibleWithMouseOver =
            case mouseOverVersion of
                Just moVersionId ->
                    Dict.get ( ( name, version ), moVersionId ) viewCache.pairIsCompatible
                        |> MaybeExtra.join

                Nothing ->
                    Nothing

        selectedVersionsAreCompatible =
            viewCache.selectedVersionsAreCompatible

        isCompatibleWithSelected =
            Dict.get ( name, version ) viewCache.isCompatibleWithSelected
                |> MaybeExtra.join

        isDirect =
            package.state == Just DirectNormal || package.state == Just DirectTest

        styleBase =
            [ C.borderRadius (C.em 0.2)
            ]

        styleColor =
            if isDirect then
                [ C.color (C.hex "000") ]

            else
                [ C.color (C.hex "888") ]

        styleBorder =
            if versionHasMouseOver then
                [ C.border3 (C.px 2) C.solid (C.hex "#00A") ]
                --else if not packageHasMouseOver && isCompatibleWithMouseOver == Ok True then
                --    [ C.border3 (C.px 2) C.solid (C.hex "#0F0") ]

            else if not packageHasMouseOver && isCompatibleWithMouseOver == Just False then
                [ C.border3 (C.px 2) C.solid (C.hex "#F00") ]

            else
                [ C.border3 (C.px 2) C.solid (C.hex "#0000") ]

        styleBgColor =
            if version == package.selectedVersion && package.state /= Nothing then
                [ C.backgroundColor (C.hex "CCE") ]

            else if selectedVersionsAreCompatible == Just True && isCompatibleWithSelected == Just False then
                [ C.backgroundColor (C.hex "FBB") ]

            else
                []

        style =
            styleBase ++ styleColor ++ styleBorder ++ styleBgColor

        hasError =
            ((mouseOverVersion /= Nothing)
                && not packageHasMouseOver
                && (isCompatibleWithMouseOver == Nothing)
            )
                || (isCompatibleWithSelected == Nothing)
                || (selectedVersionsAreCompatible == Nothing)
    in
    if hasError then
        Err <|
            OtherInternalError 3073 <|
                "Check in viewVersion failed for "
                    ++ Version.idToStr ( name, version )

    else
        Ok <|
            H.td [ A.css [ C.textAlign C.right ] ]
                [ H.span
                    ([ A.css style
                     , HE.onMouseOver (MouseOverVersion name version)
                     , HE.onMouseOut (MouseOutVersion name version)
                     ]
                        ++ (if package.state /= Nothing then
                                [ HE.onClick (VersionClick name version) ]

                            else
                                []
                           )
                    )
                    [ H.text <| Version.versionToStr version
                    ]
                ]



-- VIEW - HELPERS


{-| TODO WIP
-}
solveIndirectPackages :
    Cache
    -> Dict String Package
    -> Result InternalError ( Dict String PackageSolved, RangeDict )
solveIndirectPackages cache packages =
    let
        rAllDeps =
            packages
                |> Dict.map (\_ package -> ( package.selectedVersion, Misc.isDirect package ))
                |> Cache.rangeDictOfDepends cache.depends

        rNonTestDeps =
            packages
                |> Dict.map (\_ package -> ( package.selectedVersion, package.state == DirectNormal_ ))
                |> Cache.rangeDictOfDepends cache.depends
    in
    case ( rAllDeps, rNonTestDeps ) of
        ( Ok allDeps, Ok nonTestDeps ) ->
            let
                neededPackages =
                    allDeps |> RangeDict.ranges |> Dict.keys |> Set.fromList

                indirectTestPackages =
                    Set.diff
                        neededPackages
                        (nonTestDeps |> RangeDict.ranges |> Dict.keys |> Set.fromList)

                solvedPackages =
                    packages
                        |> Dict.map
                            (\name package ->
                                let
                                    stateSolved =
                                        if package.state == DirectNormal_ then
                                            Just DirectNormal

                                        else if package.state == DirectTest_ then
                                            Just DirectTest

                                        else if Set.member name indirectTestPackages then
                                            Just IndirectTest

                                        else if Set.member name neededPackages then
                                            Just IndirectNormal

                                        else
                                            Nothing
                                in
                                { state = stateSolved
                                , selectedVersion = package.selectedVersion
                                , initialState = package.initialState
                                }
                            )
            in
            Ok ( solvedPackages, allDeps )

        ( Err error, _ ) ->
            Err error

        ( _, Err error ) ->
            Err error



-- MONOCLE - OF PACKAGES


selectedVersionOfPackages : String -> Optional (Dict String Package) Version
selectedVersionOfPackages name =
    Monocle.Common.dict name
        |> Monocle.Compose.optionalWithLens
            (Lens .selectedVersion (\b a -> { a | selectedVersion = b }))



-- EXAMPLE JSON


exampleJson =
    """
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.0",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.0",
            "elm/core": "1.0.0",
            "elm/html": "1.0.0",
            "elm/http": "1.0.0",
            "elm/json": "1.0.0",
            "krisajenkins/remotedata": "5.0.0",
            "rtfeldman/elm-css": "15.0.0"
        },
        "indirect": {
            "Skinney/murmur3": "2.0.7",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.0",
            "rtfeldman/elm-hex": "1.0.0"
        }
    },
    "test-dependencies": {
        "direct": {
            "elm-explorations/test" : "1.0.0",
            "simonh1000/elm-jwt": "6.0.0"
        },
        "indirect": {
            "elm/random" : "1.0.0",
            "elm/regex": "1.0.0",
            "truqu/elm-base64": "2.0.4"
        }
    }
}
"""
