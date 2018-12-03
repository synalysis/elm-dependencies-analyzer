module Version exposing
    ( ReverseDepends
    , Version
    , VersionId
    , VersionRange
    , VersionRangeX(..)
    , idToStr
    , intersectRange
    , intersectRangeX
    , singletonRange
    , versionDecoder
    , versionInRange
    , versionInRangeX
    , versionRangeDecoder
    , versionRangeStrParser
    , versionRangeToStr
    , versionRangexToStr
    , versionStrParser
    , versionToStr
    )

import Dict exposing (Dict)
import Json.Decode as JD
import Parser as P exposing ((|.), (|=))
import Set exposing (Set)



-- TYPES


{-| major, minor, patch
-}
type alias Version =
    ( Int, Int, Int )


{-| min <= v < max
-}
type alias VersionRange =
    ( Version, Version )


{-| WIP
-}
type VersionRangeX
    = Infinite
    | Finite VersionRange
    | Empty


type alias VersionId =
    ( String, Version )


{-| see Cache.reverseDependsFromSelected
-}
type alias ReverseDepends =
    Dict VersionId ( Int, Set VersionId )



-- RANGE CREATE


singletonRange : Version -> VersionRange
singletonRange ( major, minor, patch ) =
    ( ( major, minor, patch ), ( major, minor, patch + 1 ) )



-- RANGE MATH


versionInRange : Version -> VersionRange -> Bool
versionInRange version ( min, max ) =
    min <= version && version < max


versionInRangeX : Version -> VersionRangeX -> Bool
versionInRangeX version vrx =
    case vrx of
        Finite vr ->
            versionInRange version vr

        Infinite ->
            True

        Empty ->
            False


intersectRange : VersionRange -> VersionRange -> Maybe VersionRange
intersectRange ( minA, maxA ) ( minB, maxB ) =
    if maxA <= minB || maxB <= minA then
        Nothing

    else
        Just ( max minA minB, min maxA maxB )


intersectRangeX : VersionRangeX -> VersionRangeX -> VersionRangeX
intersectRangeX a b =
    case ( a, b ) of
        ( Finite vrA, Finite vrB ) ->
            case intersectRange vrA vrB of
                Just vr ->
                    Finite vr

                Nothing ->
                    Empty

        ( Infinite, any ) ->
            any

        ( any, Infinite ) ->
            any

        ( Empty, _ ) ->
            Empty

        ( _, Empty ) ->
            Empty



-- TO STRING


versionToStr : Version -> String
versionToStr ( major, minor, patch ) =
    String.fromInt major
        ++ "."
        ++ String.fromInt minor
        ++ "."
        ++ String.fromInt patch


versionRangeToStr : String -> VersionRange -> String
versionRangeToStr separator ( min, max ) =
    versionToStr min
        ++ separator
        ++ versionToStr max


versionRangexToStr : String -> VersionRangeX -> String
versionRangexToStr separator versionRangeX =
    case versionRangeX of
        Infinite ->
            "Infinite"

        Finite vr ->
            versionRangeToStr separator vr

        Empty ->
            "Empty"


idToStr : VersionId -> String
idToStr ( name, version ) =
    name ++ " " ++ versionToStr version



-- DECODERS


{-| Decode string like "1.2.3"
-}
versionDecoder : JD.Decoder Version
versionDecoder =
    JD.string
        |> JD.andThen
            (\versionStr ->
                case P.run versionStrParser (String.replace "." ":" versionStr) of
                    Ok version ->
                        JD.succeed version

                    Err _ ->
                        JD.fail "Invalid Version"
            )


{-| Decode string like "1.2.3 <= v < 2.0.0"
-}
versionRangeDecoder : JD.Decoder VersionRange
versionRangeDecoder =
    JD.string
        |> JD.andThen
            (\versionRangeStr ->
                case P.run versionRangeStrParser (String.replace "." ":" versionRangeStr) of
                    Ok versionRange ->
                        JD.succeed versionRange

                    Err _ ->
                        JD.fail "Invalid VersionRange"
            )



-- PARSERS


{-| Parse version like "1:2:3".

`int` parser doesn't allow trailing period, so here I expect that
periods have been changed to colons before parsing, to make the parser simpler.

-}
versionParser : P.Parser Version
versionParser =
    P.succeed (\a b c -> ( a, b, c ))
        |= P.int
        |. P.symbol ":"
        |= P.int
        |. P.symbol ":"
        |= P.int


{-| Parse string containing only Version like "1:2:3"
-}
versionStrParser : P.Parser Version
versionStrParser =
    P.succeed identity
        |= versionParser
        |. P.end


{-| Parse string containing only VersionRange like "1:2:3 <= v < 2:0:0"
-}
versionRangeStrParser : P.Parser VersionRange
versionRangeStrParser =
    (P.succeed Tuple.pair
        |= versionParser
        |. P.spaces
        |. P.symbol "<="
        |. P.spaces
        |. P.symbol "v"
        |. P.spaces
        |. P.symbol "<"
        |. P.spaces
        |= versionParser
        |. P.end
    )
        |> P.andThen
            (\( min, max ) ->
                if min < max then
                    P.succeed ( min, max )

                else
                    P.problem "min >= max"
            )
