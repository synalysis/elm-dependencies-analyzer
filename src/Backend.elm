module Backend exposing
    ( FetchedMsg(..)
    , cacheUrl
    , fetchDepends
    , fetchVersions
    )

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Parser as P
import Version exposing (Version, VersionRange)



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
    "https://www.markuslaire.com/github/elm-dependencies-analyzer/backend/cache.php?"



-- HTTP


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
