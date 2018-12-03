module RangeDict exposing
    ( RangeDict
    , debugHtml
    , empty
    , fromVrList
    , getProblems
    , hasRange
    , insertMustContain
    , ranges
    , setReverseDepends
    )

import Dict exposing (Dict)
import Html.Styled as H exposing (Html)
import Range exposing (Range)
import Set exposing (Set)
import Version exposing (Version, VersionId, VersionRange)


{-| Dictionary of Range:s.
Conceptually has range for any name, names not explicitly listed have infinite range.

TODO: using VersionRangeX would allow combining ranges & mustContain

-}
type RangeDict
    = RangeDict
        { ranges : Dict String Range
        , mustContain : Dict String Version

        -- TODO WIP
        , reverseDepends : Version.ReverseDepends
        }



-- BUILD


{-| Create empty RangeDict, i.e. one which has infinite range for all names.
-}
empty : RangeDict
empty =
    RangeDict
        { ranges = Dict.empty
        , mustContain = Dict.empty
        , reverseDepends = Dict.empty
        }


{-| Add new parentIds which must be contained within ranges.

    This needs to be done separately from `fromVrList` as that function can't be given
    parentIds which have no dependencies.
        - TODO: VersionRangeX would fix this

    If more than one version per range is inserted, latest is kept.

-}
insertMustContain : List VersionId -> RangeDict -> RangeDict
insertMustContain parentIds (RangeDict rangeDict) =
    RangeDict
        { rangeDict
            | mustContain = Dict.union (Dict.fromList parentIds) rangeDict.mustContain
        }


setReverseDepends : Version.ReverseDepends -> RangeDict -> RangeDict
setReverseDepends reverseDepends (RangeDict rangeDict) =
    RangeDict { rangeDict | reverseDepends = reverseDepends }


{-| Create new RangeDict from association list of (parentId, name, VersionRange) tuples.

  - for each different name, calculate combined intersection of all VersionRange:s with that name

-}
fromVrList : List ( VersionId, String, VersionRange ) -> RangeDict
fromVrList list =
    let
        folder : ( VersionId, String, VersionRange ) -> RangeDict -> RangeDict
        folder ( parentId, name, vr ) (RangeDict rangeDict) =
            case Dict.get name rangeDict.ranges of
                Nothing ->
                    -- no range yet - initialize with newRange
                    RangeDict
                        { rangeDict
                            | ranges = Dict.insert name (Range.fromList [ ( parentId, vr ) ]) rangeDict.ranges
                        }

                Just range ->
                    -- has range - update with intersection
                    RangeDict
                        { rangeDict
                            | ranges = Dict.insert name (Range.intersectVr ( parentId, vr ) range) rangeDict.ranges
                        }
    in
    List.foldl folder empty list



-- QUERY


{-| Check if given name has explicit Range in RangeDict. Range could be Nothing.
-}
hasRange : String -> RangeDict -> Bool
hasRange name (RangeDict rangeDict) =
    Dict.member name rangeDict.ranges


ranges : RangeDict -> Dict String Range
ranges (RangeDict rangeDict) =
    rangeDict.ranges



-- PROBLEMS


{-| Check that none of the ranges is Nothing and all mustContain versions are contained within ranges.

    Return (possibly empty) list of problems. Each item is H.li element.

-}
getProblems : RangeDict -> List (Html msg)
getProblems (RangeDict rangeDict) =
    -- take all ranges and map them to mustContain
    -- note that any mustContain:s outside ranges is implicitly contained in infinite range
    rangeDict.ranges
        |> Dict.toList
        |> List.concatMap
            (\( name, range ) ->
                let
                    maybeMustContainVersion =
                        Dict.get name rangeDict.mustContain
                in
                Range.getVersionInRangeProblem rangeDict.reverseDepends name maybeMustContainVersion range
            )



-- DEBUG


debugHtml : RangeDict -> Html msg
debugHtml (RangeDict rangeDict) =
    H.div []
        (Dict.merge
            (\n r x -> ( n, Just r, Nothing ) :: x)
            (\n r v x -> ( n, Just r, Just v ) :: x)
            (\n v x -> ( n, Nothing, Just v ) :: x)
            rangeDict.ranges
            rangeDict.mustContain
            []
            |> List.reverse
            |> List.concatMap
                (\( name, maybeRange, maybeMustContain ) ->
                    [ H.text
                        (name
                            ++ ": "
                            ++ (case maybeMustContain of
                                    Just mustContain ->
                                        "[" ++ Version.versionToStr mustContain ++ "] "

                                    Nothing ->
                                        ""
                               )
                        )
                    ]
                        ++ (case maybeRange of
                                Just range ->
                                    [ Range.debugHtml range ]

                                Nothing ->
                                    [ H.text "Infinite" ]
                           )
                        ++ [ H.br [] [] ]
                )
        )
