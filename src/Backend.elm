module Backend exposing
    ( FetchedMsg(..)
    , fetchDepends
    , fetchVersions
    , reportInternalError
    )

import Dict exposing (Dict)
import Elm.Project
import Http
import Json.Decode as JD
import Json.Encode as JE
import Misc exposing (InternalError, Package, PackageStateUnsolved(..))
import Parser as P
import Task
import Version exposing (Version, VersionId, VersionRange)



-- TYPES


type FetchedMsg
    = FetchedVersions String (Result Http.Error (List ( Version, Int )))
    | FetchedDepends String Version (Result Http.Error (Dict String VersionRange))



-- CONFIG


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
    "https://www.elm-dependencies-analyzer.net/backend/cache.php?"


{-| URL for automatic reporting of internal errors.

    Set to `Nothing` to disable automatic error reporting.

-}
logErrorUrl : Maybe String
logErrorUrl =
    Just "https://www.elm-dependencies-analyzer.net/backend/logerror.php"



-- HTTP


hardCodedVersions : Dict String (List ( Version, Int ))
hardCodedVersions =
    [ ( "lamdera/core", [ ( ( 1, 0, 0 ), 0 ) ] )
    , ( "lamdera/codecs", [ ( ( 1, 0, 0 ), 0 ) ] )
    , ( "lamdera/program-test", [ ( ( 1, 0, 0 ), 0 ), ( ( 2, 0, 0 ), 0 ) ] )
    ]
        |> Dict.fromList


hardCodedDependencies : Dict ( String, Version ) (Dict String VersionRange)
hardCodedDependencies =
    [ ( ( "lamdera/core", ( 1, 0, 0 ) )
      , [ ( "elm/browser", ( ( 1, 0, 2 ), ( 2, 0, 0 ) ) )
        , ( "elm/bytes", ( ( 1, 0, 8 ), ( 2, 0, 0 ) ) )
        , ( "elm/core", ( ( 1, 0, 5 ), ( 2, 0, 0 ) ) )
        , ( "elm/html", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/http", ( ( 2, 0, 0 ), ( 3, 0, 0 ) ) )
        , ( "elm/json", ( ( 1, 1, 3 ), ( 2, 0, 0 ) ) )
        , ( "elm/time", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/url", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "lamdera/codecs", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        ]
            |> Dict.fromList
      )
    , ( ( "lamdera/codecs", ( 1, 0, 0 ) )
      , [ ( "elm/bytes", ( ( 1, 0, 8 ), ( 2, 0, 0 ) ) )
        , ( "elm/core", ( ( 1, 0, 5 ), ( 2, 0, 0 ) ) )
        ]
            |> Dict.fromList
      )
    , ( ( "lamdera/program-test", ( 1, 0, 0 ) )
      , [ ( "danfishgold/base64-bytes", ( ( 1, 1, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/browser", ( ( 1, 0, 2 ), ( 2, 0, 0 ) ) )
        , ( "elm/bytes", ( ( 1, 0, 8 ), ( 2, 0, 0 ) ) )
        , ( "elm/core", ( ( 1, 0, 5 ), ( 2, 0, 0 ) ) )
        , ( "elm/file", ( ( 1, 0, 5 ), ( 2, 0, 0 ) ) )
        , ( "elm/html", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/http", ( ( 2, 0, 0 ), ( 3, 0, 0 ) ) )
        , ( "elm/json", ( ( 1, 1, 3 ), ( 2, 0, 0 ) ) )
        , ( "elm/parser", ( ( 1, 1, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/time", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/url", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/virtual-dom", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm-explorations/test", ( ( 1, 2, 2 ), ( 2, 0, 0 ) ) )
        , ( "elm-explorations/webgl", ( ( 1, 1, 3 ), ( 2, 0, 0 ) ) )
        , ( "folkertdev/elm-sha2", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "ianmackenzie/elm-units", ( ( 2, 9, 0 ), ( 3, 0, 0 ) ) )
        , ( "lamdera/codecs", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "lamdera/core", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "mgold/elm-nonempty-list", ( ( 4, 2, 0 ), ( 5, 0, 0 ) ) )
        , ( "pzp1997/assoc-list", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        ]
            |> Dict.fromList
      )
    , ( ( "lamdera/program-test", ( 2, 0, 0 ) )
      , [ ( "danfishgold/base64-bytes", ( ( 1, 1, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/browser", ( ( 1, 0, 2 ), ( 2, 0, 0 ) ) )
        , ( "elm/bytes", ( ( 1, 0, 8 ), ( 2, 0, 0 ) ) )
        , ( "elm/core", ( ( 1, 0, 5 ), ( 2, 0, 0 ) ) )
        , ( "elm/file", ( ( 1, 0, 5 ), ( 2, 0, 0 ) ) )
        , ( "elm/html", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/http", ( ( 2, 0, 0 ), ( 3, 0, 0 ) ) )
        , ( "elm/json", ( ( 1, 1, 3 ), ( 2, 0, 0 ) ) )
        , ( "elm/parser", ( ( 1, 1, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/time", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/url", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm/virtual-dom", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "elm-explorations/test", ( ( 2, 0, 0 ), ( 3, 0, 0 ) ) )
        , ( "elm-explorations/webgl", ( ( 1, 1, 3 ), ( 2, 0, 0 ) ) )
        , ( "folkertdev/elm-sha2", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "ianmackenzie/elm-units", ( ( 2, 9, 0 ), ( 3, 0, 0 ) ) )
        , ( "lamdera/codecs", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "lamdera/core", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        , ( "mgold/elm-nonempty-list", ( ( 4, 2, 0 ), ( 5, 0, 0 ) ) )
        , ( "pzp1997/assoc-list", ( ( 1, 0, 0 ), ( 2, 0, 0 ) ) )
        ]
            |> Dict.fromList
      )
    ]
        |> Dict.fromList


fetchVersions : String -> Cmd FetchedMsg
fetchVersions name =
    case Dict.get name hardCodedVersions of
        Just hardCodedPackage ->
            Task.succeed (Ok hardCodedPackage) |> Task.perform (FetchedVersions name)

        Nothing ->
            Http.get
                { url = cacheUrl ++ name
                , expect = Http.expectJson (FetchedVersions name) packageVersionsDecoder
                }


fetchDepends : String -> Version -> Cmd FetchedMsg
fetchDepends name version =
    case Dict.get ( name, version ) hardCodedDependencies of
        Just hardCodedPackage ->
            Task.succeed (Ok hardCodedPackage) |> Task.perform (FetchedDepends name version)

        Nothing ->
            Http.get
                { url = cacheUrl ++ name ++ "/" ++ Version.versionToStr version
                , expect = Http.expectJson (FetchedDepends name version) packageDependenciesDecoder
                }


reportInternalError :
    noOpMsg
    -> InternalError
    -> Elm.Project.ApplicationInfo
    -> Dict String Package
    -> Maybe VersionId
    -> Cmd noOpMsg
reportInternalError noOpMsg internalError elmJson packages mouseOverVersion =
    case logErrorUrl of
        Just logErrorUrl_ ->
            Http.post
                { url = logErrorUrl_
                , body =
                    JE.object
                        [ ( "tag", JE.int <| Misc.internalErrorTag internalError )
                        , ( "error", JE.string <| Misc.internalErrorToStr internalError )
                        , ( "elmJson"
                          , { elmJson | dirs = [] }
                                |> Elm.Project.Application
                                |> Elm.Project.encode
                          )
                        , ( "packages"
                          , packages
                                |> Dict.toList
                                |> JE.list
                                    (\( name, package ) ->
                                        JE.object
                                            [ ( "package", JE.string name )
                                            , ( "isDirect"
                                              , case package.state of
                                                    DirectNormal_ ->
                                                        JE.bool True

                                                    DirectTest_ ->
                                                        JE.bool True

                                                    IndirectOrNotNeeded ->
                                                        JE.bool False
                                              )
                                            , ( "selectedVersion"
                                              , JE.string <| Version.versionToStr package.selectedVersion
                                              )
                                            ]
                                    )
                          )
                        , ( "mouseOverVersion"
                          , case mouseOverVersion of
                                Just ( name, version ) ->
                                    JE.object
                                        [ ( "package", JE.string name )
                                        , ( "version", JE.string <| Version.versionToStr version )
                                        ]

                                Nothing ->
                                    JE.null
                          )
                        ]
                        |> JE.encode 4
                        |> Http.stringBody "application/json"
                , expect = Http.expectWhatever (\_ -> noOpMsg)
                }

        Nothing ->
            Cmd.none



-- DECODERS


{-| decodes dependencies from package elm.json
-}
packageDependenciesDecoder : JD.Decoder (Dict String VersionRange)
packageDependenciesDecoder =
    JD.map Dict.fromList
        (JD.field "dependencies" (JD.keyValuePairs Version.versionRangeDecoder))


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
