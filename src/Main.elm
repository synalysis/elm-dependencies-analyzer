module Main exposing (main)

import Browser
import Cache exposing (Cache, FetchedValue(..))
import Compatible
import Css as C
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as HE
import Json.Decode as JD
import Json.Decode.Field as JF
import Json.Encode as JE
import List.Extra as ListExtra
import Maybe.Extra as MaybeExtra
import Monocle.Common
import Monocle.Compose
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import Package exposing (Package)
import RangeDict exposing (RangeDict)
import SortableDict exposing (SortableDict)
import StepResult
import Task
import Version exposing (Version, VersionId, VersionRange)
import ViewCache exposing (ViewCache)



-- TYPES


type alias Model =
    { state : ModelState
    , inputJson : String
    , mouseOverVersion : Maybe VersionId
    , packages : SortableDict String Package

    -- WIP
    , fetchingCache : Cache.FetchingCache
    }


type ModelState
    = NothingAnalyzed
    | JsonParsingError String
    | Fetching Int
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
    | IsDirectCheckboxClick String
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
      , packages = SortableDict.empty
      , fetchingCache = Cache.newFetchingCache
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

        IsDirectCheckboxClick name ->
            let
                newPackages =
                    model.packages
                        |> modifyIsDirectOfPackages name not

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
    case JD.decodeString applicationDependenciesDecoder model.inputJson of
        Err error ->
            ( { model
                | packages = SortableDict.empty
                , state = JsonParsingError (JD.errorToString error)
              }
            , Cmd.none
            )

        Ok appDepends ->
            let
                addonPackageCache =
                    appDepends
                        |> List.map
                            (\( name, version, isDirect ) ->
                                ( name
                                , { allVersions = NotFetched
                                  , minVersion = version
                                  }
                                )
                            )
                        |> Dict.fromList

                newPackages =
                    appDepends
                        |> List.map
                            (\( name, version, isDirect ) ->
                                ( name
                                , { isDirect = isDirect
                                  , jsonVersion = Just version
                                  , selectedVersion = version
                                  }
                                )
                            )
                        |> SortableDict.fromList

                newPackageCache =
                    Dict.merge
                        Dict.insert
                        (\name old new ->
                            Dict.insert name
                                { allVersions = old.allVersions
                                , minVersion = new.minVersion
                                }
                        )
                        Dict.insert
                        model.fetchingCache.packages
                        addonPackageCache
                        Dict.empty

                newFetchingCache =
                    { packages = newPackageCache
                    , depends = model.fetchingCache.depends
                    }
                        |> addMissingVersionsToDependsCache

                ( newState, nextCmd ) =
                    case Cache.fetchNextThing newFetchingCache of
                        Just cmd ->
                            ( Fetching 0, cmd )

                        Nothing ->
                            case Cache.validate newFetchingCache of
                                Ok cache ->
                                    ( FetchingSucceeded cache (ViewCache.new newPackages cache)
                                    , Cmd.none
                                    )

                                Err errors ->
                                    ( errors
                                        |> List.map (\error -> H.li [] [ H.text error ])
                                        |> FetchingFailed
                                    , Cmd.none
                                    )
            in
            ( { model
                | packages = newPackages
                , fetchingCache = newFetchingCache
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
        Fetching oldDone ->
            let
                newFetchingCache =
                    case fetched of
                        Cache.FetchedVersions name result ->
                            case result of
                                Err error ->
                                    { packages =
                                        model.fetchingCache.packages
                                            |> (Cache.allVersionsOfFetchingPackageCache name).set Failed
                                    , depends = model.fetchingCache.depends
                                    }

                                Ok packageVersions ->
                                    { packages =
                                        model.fetchingCache.packages
                                            |> (Cache.allVersionsOfFetchingPackageCache name).set
                                                (Succeeded packageVersions)
                                    , depends = model.fetchingCache.depends
                                    }
                                        |> addMissingVersionsToDependsCache

                        Cache.FetchedDepends name version result ->
                            case result of
                                Err error ->
                                    { packages = model.fetchingCache.packages
                                    , depends = Dict.insert ( name, version ) Failed model.fetchingCache.depends
                                    }

                                Ok versionRanges ->
                                    { packages =
                                        Dict.union
                                            model.fetchingCache.packages
                                            (versionRanges
                                                |> Dict.map
                                                    (\n ( min, max ) ->
                                                        { allVersions = NotFetched
                                                        , minVersion = min
                                                        }
                                                    )
                                            )
                                    , depends = Dict.insert ( name, version ) (Succeeded versionRanges) model.fetchingCache.depends
                                    }

                newPackages =
                    case fetched of
                        Cache.FetchedVersions name (Ok packageVersions) ->
                            if not (SortableDict.member name model.packages) then
                                case ListExtra.last <| List.map Tuple.first <| packageVersions of
                                    Just latestVersion ->
                                        SortableDict.insert
                                            name
                                            { isDirect = False
                                            , jsonVersion = Nothing
                                            , selectedVersion = latestVersion
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
                            Fetching newDone

                        Nothing ->
                            case Cache.validate newFetchingCache of
                                Ok cache ->
                                    FetchingSucceeded cache (ViewCache.new model.packages cache)

                                Err errors ->
                                    errors
                                        |> List.map (\error -> H.li [] [ H.text error ])
                                        |> FetchingFailed
            in
            ( { model
                | fetchingCache = newFetchingCache
                , packages = newPackages
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
            [ H.text "Nothing analyzed so far." ]

        JsonParsingError error ->
            [ H.pre [] [ H.text (String.replace "\\n" "\n" error) ] ]

        Fetching done ->
            [ H.text ("Fetching package data ... " ++ String.fromInt done) ]

        FetchingFailed errors ->
            [ H.ul [] errors ]

        FetchingSucceeded cache viewCache ->
            viewRightSectionWhenFetchingSucceeded model cache viewCache


viewRightSectionWhenFetchingSucceeded : Model -> Cache -> ViewCache -> List (Html Msg)
viewRightSectionWhenFetchingSucceeded model cache viewCache =
    let
        -- TODO: maybe merge into rangeDictOfDepends
        allPackages : Dict String ( Version, Bool )
        allPackages =
            model.packages
                -- TODO: I need SortableDict.toDict
                |> SortableDict.toList
                |> Dict.fromList
                |> Dict.map (\_ package -> ( package.selectedVersion, package.isDirect ))
    in
    case Cache.rangeDictOfDepends cache.depends allPackages of
        Err error ->
            [ H.ul [] [ H.li [] [ H.text error ] ] ]

        Ok deps ->
            let
                problems =
                    RangeDict.getProblems deps

                isInternalInconsistency =
                    case viewCache.directPackagesAreCompatible of
                        Nothing ->
                            False

                        Just bool ->
                            (problems == []) /= bool
            in
            viewPackages model cache viewCache deps
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


viewPackages : Model -> Cache -> ViewCache -> RangeDict -> List (Html Msg)
viewPackages model cache viewCache deps =
    let
        basePackagesToShow =
            model.packages
                |> SortableDict.toList
                |> List.filterMap
                    (\( name, package ) ->
                        case package.jsonVersion of
                            Just jsonVersion ->
                                Just ( name, package )

                            Nothing ->
                                Nothing
                    )

        extraPackagesToShow =
            Dict.diff
                (RangeDict.ranges deps)
                (Dict.fromList basePackagesToShow)
                |> Dict.toList
    in
    [ H.table []
        ((basePackagesToShow
            |> List.concatMap
                (\( name, package ) ->
                    viewPackage
                        { model = model
                        , cache = cache
                        , viewCache = viewCache
                        , deps = deps
                        , package = package
                        , name = name
                        }
                )
         )
            ++ [ H.tr [] [ H.td [] [ H.hr [] [] ] ] ]
            ++ (extraPackagesToShow
                    |> List.concatMap
                        (\( name, _ ) ->
                            case SortableDict.get name model.packages of
                                Just package ->
                                    viewPackage
                                        { model = model
                                        , cache = cache
                                        , viewCache = viewCache
                                        , deps = deps
                                        , package = package
                                        , name = name
                                        }

                                Nothing ->
                                    -- TODO IMPOSSIBLE
                                    [ H.text "ERROR" ]
                        )
               )
            ++ (if not <| List.isEmpty extraPackagesToShow then
                    [ H.tr [] [ H.td [] [ H.hr [] [] ] ] ]

                else
                    []
               )
        )
    ]


viewPackage :
    { model : Model
    , cache : Cache
    , viewCache : ViewCache
    , deps : RangeDict
    , package : Package
    , name : String
    }
    -> List (Html Msg)
viewPackage { model, cache, viewCache, deps, package, name } =
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

        packageNeeded =
            package.isDirect || RangeDict.hasRange name deps

        nameStyleColor =
            if package.isDirect then
                []

            else
                [ C.color (C.hex "#888") ]

        nameStyleStrike =
            if not packageNeeded then
                [ C.textDecoration3 C.lineThrough C.solid (C.hex "#000") ]

            else
                []

        nameStyle =
            nameStyleColor ++ nameStyleStrike
    in
    [ H.tr []
        ([ H.td []
            [ packageLink
            , H.input
                [ A.type_ "checkbox"
                , A.checked package.isDirect
                , A.disabled (package.jsonVersion == Nothing)
                , HE.on "change" (JD.succeed (IsDirectCheckboxClick name))
                ]
                []
            , H.span [ A.css nameStyle ] [ H.text name ]
            ]
         ]
            ++ viewPackageVersions
                { model = model
                , cache = cache
                , viewCache = viewCache
                , package = package
                , packageNeeded = packageNeeded
                , name = name
                }
        )
    ]


viewPackageVersions :
    { model : Model
    , cache : Cache
    , viewCache : ViewCache
    , package : Package
    , packageNeeded : Bool
    , name : String
    }
    -> List (Html Msg)
viewPackageVersions { model, cache, viewCache, package, packageNeeded, name } =
    case Dict.get name cache.versions of
        Just versions ->
            versions
                |> List.map
                    (\( version, _ ) ->
                        viewVersion
                            { model = model
                            , cache = cache
                            , viewCache = viewCache
                            , package = package
                            , packageNeeded = packageNeeded
                            , name = name
                            , version = version
                            }
                    )

        Nothing ->
            [ H.text "ERROR" ]


viewVersion :
    { model : Model
    , cache : Cache
    , viewCache : ViewCache
    , package : Package
    , packageNeeded : Bool
    , name : String
    , version : Version
    }
    -> Html Msg
viewVersion { model, cache, viewCache, package, packageNeeded, name, version } =
    let
        versionHasMouseOver =
            model.mouseOverVersion == Just ( name, version )

        packageHasMouseOver =
            case model.mouseOverVersion of
                Just ( moName, _ ) ->
                    moName == name

                Nothing ->
                    False

        isCompatibleWithMouseOver =
            case model.mouseOverVersion of
                Just moVersionId ->
                    Dict.get ( ( name, version ), moVersionId ) viewCache.pairIsCompatible
                        |> MaybeExtra.join

                Nothing ->
                    Nothing

        directPackagesAreCompatible =
            viewCache.directPackagesAreCompatible

        isCompatibleWithDirect =
            Dict.get ( name, version ) viewCache.isCompatibleWithDirect
                |> MaybeExtra.join

        styleBase =
            [ C.borderRadius (C.em 0.2)
            ]

        styleColor =
            if package.isDirect then
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
            if version == package.selectedVersion && packageNeeded then
                [ C.backgroundColor (C.hex "CCE") ]

            else if directPackagesAreCompatible == Just True && isCompatibleWithDirect == Just False then
                [ C.backgroundColor (C.hex "FBB") ]

            else
                []

        style =
            styleBase ++ styleColor ++ styleBorder ++ styleBgColor

        text =
            if
                ((model.mouseOverVersion /= Nothing)
                    && not packageHasMouseOver
                    && (isCompatibleWithMouseOver == Nothing)
                )
                    || (isCompatibleWithDirect == Nothing)
                    || (directPackagesAreCompatible == Nothing)
            then
                "error"

            else
                Version.versionToStr version
    in
    H.td [ A.css [ C.textAlign C.right ] ]
        [ H.span
            ([ A.css style
             , HE.onMouseOver (MouseOverVersion name version)
             , HE.onMouseOut (MouseOutVersion name version)
             ]
                ++ (if packageNeeded then
                        [ HE.onClick (VersionClick name version) ]

                    else
                        []
                   )
            )
            [ H.text text
            ]
        ]



-- DECODERS


{-| decodes dependencies from application elm.json
-}
applicationDependenciesDecoder : JD.Decoder (List ( String, Version, Bool ))
applicationDependenciesDecoder =
    let
        helper : Bool -> List VersionId -> List ( String, Version, Bool )
        helper isDirect =
            List.map (\( name, version ) -> ( name, version, isDirect ))
    in
    JF.requireAt [ "dependencies", "direct" ] (JD.keyValuePairs Version.versionDecoder) <|
        \direct ->
            JF.requireAt [ "dependencies", "indirect" ] (JD.keyValuePairs Version.versionDecoder) <|
                \indirect ->
                    JD.succeed
                        (helper True direct ++ helper False indirect)



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
            "rtfeldman/elm-css": "15.0.0",
            "simonh1000/elm-jwt": "6.0.0"
        },
        "indirect": {
            "Skinney/murmur3": "2.0.7",
            "elm/regex": "1.0.0",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.0",
            "rtfeldman/elm-hex": "1.0.0",
            "truqu/elm-base64": "2.0.4"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
"""
