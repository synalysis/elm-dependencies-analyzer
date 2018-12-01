module Main exposing (main)

import Browser
import Cache exposing (Cache, DependsCache, FetchedValue(..), FetchingDependsCache, FetchingPackageCache)
import Compatible
import Css as C
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as HE
import Http
import Json.Decode as JD
import Json.Decode.Field as JF
import Json.Encode as JE
import List.Extra as ListExtra
import MainTypes exposing (..)
import Maybe.Extra as MaybeExtra
import Package exposing (Package, PackageType(..), PackageVersion)
import Parser as P exposing ((|.), (|=))
import RangeDict exposing (RangeDict)
import SortableDict exposing (SortableDict)
import StepResult
import Task
import Version exposing (Version, VersionId, VersionRange)
import ViewCache exposing (ViewCache)



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
      , extraPackages = Dict.empty
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

        VersionClick packageType name version ->
            let
                ( newPackages, newExtraPackages ) =
                    case packageType of
                        Direct ->
                            ( model.packages
                                |> (selectedVersionOfPackages name).set version
                            , model.extraPackages
                            )

                        Indirect ->
                            ( model.packages
                                |> (selectedVersionOfPackages name).set version
                            , model.extraPackages
                            )

                        Extra ->
                            ( model.packages
                            , model.extraPackages
                                |> (selectedVersionOfExtraPackages name).set version
                            )

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
                , extraPackages = newExtraPackages
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

        Ok packages ->
            let
                addonPackageCache =
                    packages
                        |> SortableDict.toList
                        |> List.map
                            (\( name, package ) ->
                                ( name
                                , { allVersions = NotFetched
                                  , minVersion = package.installedVersion
                                  }
                                )
                            )
                        |> Dict.fromList

                newPackageCache =
                    Dict.merge
                        (\name old accum -> Dict.insert name old accum)
                        (\name old new accum ->
                            let
                                mix =
                                    { allVersions = old.allVersions
                                    , minVersion = new.minVersion
                                    }
                            in
                            Dict.insert name mix accum
                        )
                        (\name new accum -> Dict.insert name new accum)
                        model.fetchingCache.packages
                        addonPackageCache
                        Dict.empty

                newFetchingCache =
                    { packages = newPackageCache
                    , depends = addMissingVersionsToDependsCache newPackageCache model.fetchingCache.depends
                    }

                ( newState, nextCmd ) =
                    case fetchNextThing newFetchingCache of
                        Just cmd ->
                            ( Fetching 0, cmd )

                        Nothing ->
                            case Cache.validate newFetchingCache of
                                Ok cache ->
                                    ( FetchingSucceeded cache (ViewCache.new packages cache)
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
                | packages = packages
                , fetchingCache = newFetchingCache
                , state = newState
              }
            , nextCmd
            )


updateFetched :
    Model
    -> Fetched
    -> ( Model, Cmd Msg )
updateFetched model fetched =
    case model.state of
        Fetching oldDone ->
            let
                newFetchingCache =
                    case fetched of
                        FetchedVersions name result ->
                            case result of
                                Err error ->
                                    { packages =
                                        model.fetchingCache.packages
                                            |> (Cache.allVersionsOfFetchingPackageCache name).set Failed
                                    , depends = model.fetchingCache.depends
                                    }

                                Ok packageVersions ->
                                    let
                                        subNewPackageCache =
                                            model.fetchingCache.packages
                                                |> (Cache.allVersionsOfFetchingPackageCache name).set
                                                    (packageVersions
                                                        |> Dict.toList
                                                        |> List.map (\( version, pv ) -> ( version, pv.timestamp ))
                                                        |> Succeeded
                                                    )
                                    in
                                    { packages = subNewPackageCache
                                    , depends = addMissingVersionsToDependsCache subNewPackageCache model.fetchingCache.depends
                                    }

                        FetchedDepends name version result ->
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

                newExtraPackages =
                    case fetched of
                        FetchedVersions name (Ok packageVersions) ->
                            if not (SortableDict.member name model.packages) then
                                if not (Dict.member name model.extraPackages) then
                                    case ListExtra.last <| Dict.keys <| packageVersions of
                                        Just lastestVersion ->
                                            Dict.insert
                                                name
                                                { selectedVersion = lastestVersion }
                                                model.extraPackages

                                        Nothing ->
                                            -- IMPOSSIBLE
                                            model.extraPackages

                                else
                                    model.extraPackages

                            else
                                model.extraPackages

                        _ ->
                            model.extraPackages

                maybeNextCmd =
                    fetchNextThing newFetchingCache

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
                , extraPackages = newExtraPackages
                , state = newState
              }
            , Maybe.withDefault Cmd.none maybeNextCmd
            )

        _ ->
            -- IMPOSSIBLE
            ( model, Cmd.none )



-- UPDATE - HELPERS


{-| Add versions present in FetchingPackageCache but not in FetchingDependsCache
to FetchingDependsCache as `NotFetched`, with `minVersion` filtering.
-}
addMissingVersionsToDependsCache : FetchingPackageCache -> FetchingDependsCache -> FetchingDependsCache
addMissingVersionsToDependsCache packageCache dependsCache =
    packageCache
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
        |> Dict.union dependsCache



-- VIEW


view : Model -> Html Msg
view model =
    H.table []
        [ H.tr []
            [ H.td [ A.colspan 2 ]
                [ H.text "Open/paste application elm.json here and then press Analyze."
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
    , H.button [ HE.onClick ExampleClick ] [ H.text "Load example" ]
    , H.text " "
    , H.button [ HE.onClick OpenFileClick ] [ H.text "Open File ..." ]
    , H.text " "
    , H.button [ HE.onClick AnalyzeButtonClick ] [ H.text "Analyze" ]
    , H.br [] []
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
    case rangeDictOfDepends cache.depends (allPackages model) of
        Err errors ->
            [ H.ul []
                (errors
                    |> List.map (\error -> H.li [] [ H.text error ])
                )
            ]

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
        extraPackagesToShow =
            Dict.diff
                (RangeDict.ranges deps)
                (model.packages |> SortableDict.toList |> Dict.fromList)
                |> Dict.toList
    in
    [ H.table []
        ((model.packages
            |> SortableDict.toList
            |> List.concatMap
                (\( name, package ) ->
                    let
                        packageType =
                            if package.isDirect then
                                Direct

                            else
                                Indirect
                    in
                    viewPackage
                        { model = model
                        , cache = cache
                        , viewCache = viewCache
                        , deps = deps
                        , packageType = packageType
                        , selectedVersion = package.selectedVersion
                        , name = name
                        }
                )
         )
            ++ [ H.tr [] [ H.td [] [ H.hr [] [] ] ] ]
            ++ (extraPackagesToShow
                    |> List.concatMap
                        (\( name, _ ) ->
                            case Dict.get name model.extraPackages of
                                Just extraPackage ->
                                    viewPackage
                                        { model = model
                                        , cache = cache
                                        , viewCache = viewCache
                                        , deps = deps
                                        , packageType = Extra
                                        , selectedVersion = extraPackage.selectedVersion
                                        , name = name
                                        }

                                Nothing ->
                                    -- IMPOSSIBLE
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
    , packageType : PackageType
    , selectedVersion : Version
    , name : String
    }
    -> List (Html Msg)
viewPackage { model, cache, viewCache, deps, packageType, selectedVersion, name } =
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
            packageType == Direct || RangeDict.hasRange name deps

        nameStyleColor =
            if packageType == Direct then
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
                , A.checked (packageType == Direct)
                , A.disabled (packageType == Extra)
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
                , packageType = packageType
                , selectedVersion = selectedVersion
                , packageNeeded = packageNeeded
                , name = name
                }
        )
    ]


viewPackageVersions :
    { model : Model
    , cache : Cache
    , viewCache : ViewCache
    , packageType : PackageType
    , selectedVersion : Version
    , packageNeeded : Bool
    , name : String
    }
    -> List (Html Msg)
viewPackageVersions { model, cache, viewCache, packageType, selectedVersion, packageNeeded, name } =
    case Dict.get name cache.versions of
        Just versions ->
            versions
                |> List.map
                    (\( version, _ ) ->
                        viewVersion
                            { model = model
                            , cache = cache
                            , viewCache = viewCache
                            , packageType = packageType
                            , selectedVersion = selectedVersion
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
    , packageType : PackageType
    , selectedVersion : Version
    , packageNeeded : Bool
    , name : String
    , version : Version
    }
    -> Html Msg
viewVersion { model, cache, viewCache, packageType, selectedVersion, packageNeeded, name, version } =
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
            if packageType /= Direct then
                [ C.color (C.hex "888") ]

            else
                [ C.color (C.hex "000") ]

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
            if version == selectedVersion && packageNeeded then
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
                        [ HE.onClick (VersionClick packageType name version) ]

                    else
                        []
                   )
            )
            [ H.text text
            ]
        ]



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


fetchVersions : String -> Cmd Msg
fetchVersions name =
    Http.get
        { url = cacheUrl ++ name
        , expect = Http.expectJson (\a -> Fetched (FetchedVersions name a)) packageVersionsDecoder
        }


fetchDepends : String -> Version -> Cmd Msg
fetchDepends name version =
    Http.get
        { url = cacheUrl ++ name ++ "/" ++ Version.versionToStr version
        , expect = Http.expectJson (\a -> Fetched (FetchedDepends name version a)) packageDependenciesDecoder
        }


fetchNextVersions : FetchingPackageCache -> Maybe (Cmd Msg)
fetchNextVersions packageCache =
    packageCache
        |> Dict.toList
        |> List.filter (\( _, item ) -> item.allVersions == NotFetched)
        |> List.head
        |> Maybe.map (\( name, _ ) -> fetchVersions name)


fetchNextDepends : FetchingDependsCache -> Maybe (Cmd Msg)
fetchNextDepends dependsCache =
    dependsCache
        |> Dict.toList
        |> List.filter (\( _, fetched ) -> fetched == NotFetched)
        |> List.head
        |> Maybe.map (\( ( name, version ), _ ) -> fetchDepends name version)


fetchNextThing : Cache.FetchingCache -> Maybe (Cmd Msg)
fetchNextThing fetchingCache =
    case fetchNextDepends fetchingCache.depends of
        Just cmd ->
            Just cmd

        Nothing ->
            fetchNextVersions fetchingCache.packages



-- DECODERS


{-| decodes dependencies from application elm.json
-}
applicationDependenciesDecoder : JD.Decoder (SortableDict String Package)
applicationDependenciesDecoder =
    let
        helper : Bool -> List VersionId -> List ( String, Package )
        helper isDirect =
            List.map
                (\( name, version ) ->
                    ( name
                    , { isDirect = isDirect
                      , installedVersion = version
                      , selectedVersion = version
                      }
                    )
                )
    in
    JF.requireAt [ "dependencies", "direct" ] (JD.keyValuePairs Version.versionDecoder) <|
        \direct ->
            JF.requireAt [ "dependencies", "indirect" ] (JD.keyValuePairs Version.versionDecoder) <|
                \indirect ->
                    JD.succeed
                        (helper True direct
                            ++ helper False indirect
                            |> SortableDict.fromList
                        )


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
