module Range exposing
    ( Range
    , debugHtml
    , fromList
    , getVersionInRangeProblem
    , intersectVr
    , versionInRange
    )

import Dict exposing (Dict)
import Html.Styled as H exposing (Html)
import Set exposing (Set)
import Version exposing (Version, VersionId, VersionRange, VersionRangeX)



-- TYPES


{-| VersionRangeX with history of how it came to be.

    Whenever VersionRange is used to alter Range, an accompanying parentId must be given
    which will be saved in the history of the Range.

-}
type Range
    = Range
        { versionRangeX : VersionRangeX
        , intersectSources : Dict VersionRange (Set VersionId)
        }



-- BUILD


newInfinite : Range
newInfinite =
    Range
        { versionRangeX = Version.Infinite
        , intersectSources = Dict.empty
        }


{-| Create Range from list of (parentId, VersionRange):s.

    If list is empty, Range is Infinite.

-}
fromList : List ( VersionId, VersionRange ) -> Range
fromList list =
    List.foldl intersectVr newInfinite list



-- QUERY


{-| Check whether version is within range
-}
versionInRange : Version -> Range -> Bool
versionInRange version (Range range) =
    Version.versionInRangeX version range.versionRangeX



-- COMBINE


intersectVr : ( VersionId, VersionRange ) -> Range -> Range
intersectVr ( parentId, vr ) (Range range) =
    let
        newVersionRangeX =
            Version.intersectRangeX range.versionRangeX (Version.Finite vr)

        newIntersectSources =
            case Dict.get vr range.intersectSources of
                Nothing ->
                    Dict.insert vr (Set.singleton parentId) range.intersectSources

                Just parentIds ->
                    Dict.insert vr (Set.insert parentId parentIds) range.intersectSources
    in
    Range
        { versionRangeX = newVersionRangeX
        , intersectSources = newIntersectSources
        }



-- PROBLEMS


{-| Check that given Version (if any) is within range.

    Return (possibly empty) list of problems. Each item is H.li element.

    TODO: Try to change ínput to `Version` instead of `Maybe Version`

-}
getVersionInRangeProblem : String -> Maybe Version -> Range -> List (Html msg)
getVersionInRangeProblem name maybeVersion (Range range) =
    case range.versionRangeX of
        Version.Empty ->
            [ H.li []
                [ H.text name
                , H.ul []
                    (describeNonIntersectingParents range.intersectSources)
                ]
            ]

        Version.Infinite ->
            []

        Version.Finite vr ->
            case maybeVersion of
                Nothing ->
                    []

                Just version ->
                    if Version.versionInRange version vr then
                        []

                    else
                        [ H.li []
                            [ H.text
                                (name
                                    ++ " "
                                    ++ Version.versionToStr version
                                )
                            , H.ul []
                                (range.intersectSources
                                    |> Dict.toList
                                    |> List.filter
                                        (\( parentVr, _ ) ->
                                            not (Version.versionInRange version parentVr)
                                        )
                                    |> List.map
                                        (\( parentVr, parentIds ) ->
                                            H.li []
                                                [ H.text
                                                    (Version.versionRangeToStr " <= v < " parentVr
                                                        ++ " needed by "
                                                        ++ describeParentIds parentIds
                                                    )
                                                ]
                                        )
                                )
                            ]
                        ]



-- PROBLEMS - INTERNAL HELPERS


describeNonIntersectingParents : Dict VersionRange (Set VersionId) -> List (Html msg)
describeNonIntersectingParents dict =
    let
        dictAsList =
            Dict.toList dict
    in
    dictAsList
        |> List.filterMap
            (\( vrA, idsA ) ->
                let
                    sublist =
                        dictAsList
                            |> List.filterMap
                                (\( vrB, idsB ) ->
                                    if vrA < vrB && Version.intersectRange vrA vrB == Nothing then
                                        Just ( vrB, idsB )

                                    else
                                        Nothing
                                )
                in
                case sublist of
                    [] ->
                        Nothing

                    _ ->
                        Just ( vrA, idsA, sublist )
            )
        |> List.map
            (\( vrA, idsA, sublist ) ->
                H.li []
                    [ H.text
                        (Version.versionRangeToStr " <= v < " vrA
                            ++ " needed by "
                            ++ describeParentIds idsA
                        )
                    , H.ul []
                        (sublist
                            |> List.map
                                (\( vrB, idsB ) ->
                                    H.li []
                                        [ H.text
                                            (Version.versionRangeToStr " <= v < " vrB
                                                ++ " needed by "
                                                ++ describeParentIds idsB
                                            )
                                        ]
                                )
                        )
                    ]
            )


describeParentIds : Set VersionId -> String
describeParentIds parentIds =
    case List.head (Set.toList parentIds) of
        Just ( parentName, parentVersion ) ->
            let
                parentIdCount =
                    Set.size parentIds
            in
            parentName
                ++ " "
                ++ Version.versionToStr parentVersion
                ++ (if parentIdCount > 1 then
                        " (and "
                            ++ String.fromInt (parentIdCount - 1)
                            ++ " other"
                            ++ (if parentIdCount > 2 then
                                    "s"

                                else
                                    ""
                               )
                            ++ ")"

                    else
                        ""
                   )

        Nothing ->
            -- IMPOSSIBLE
            "ERROR"



-- DEBUG


debugHtml : Range -> Html msg
debugHtml (Range range) =
    let
        maybeMin maybeVr =
            case maybeVr of
                Just ( min, max ) ->
                    Just min

                Nothing ->
                    Nothing

        maybeMax maybeVr =
            case maybeVr of
                Just ( min, max ) ->
                    Just max

                Nothing ->
                    Nothing

        strIfDiff maybeTargetVr version =
            if maybeTargetVr /= Just version then
                Version.versionToStr version

            else
                ""

        sources : Maybe VersionRange -> List (Html msg)
        sources maybeTargetVr =
            range.intersectSources
                |> Dict.toList
                |> List.map
                    (\( ( min, max ), parentIds ) ->
                        H.text
                            (strIfDiff (maybeMin maybeTargetVr) min
                                ++ "~"
                                ++ strIfDiff (maybeMax maybeTargetVr) max
                                ++ " x"
                                ++ String.fromInt (Set.size parentIds)
                                ++ " "
                            )
                    )
    in
    H.span []
        ([ H.text (Version.versionRangexToStr "~" range.versionRangeX)
         , H.text " ( "
         ]
            ++ (case range.versionRangeX of
                    Version.Finite vr ->
                        sources (Just vr)

                    _ ->
                        sources Nothing
               )
            ++ [ H.text ")"
               ]
        )
