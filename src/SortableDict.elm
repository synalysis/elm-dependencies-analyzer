module SortableDict exposing
    ( SortableDict
    , empty
    , fromList
    , get
    , insert
    , isEmpty
    , keys
    , member
    , remove
    , toList
    , update
    )

import Dict exposing (Dict)


{-| TODO: better implementation
-}



-- TYPE


type SortableDict comparable v
    = SortableDict Int (Dict Int ( comparable, v )) (Dict comparable Int) (Dict comparable v)



-- BUILD


{-| Create empty dictionary.
-}
empty : SortableDict comparable v
empty =
    SortableDict 0 Dict.empty Dict.empty Dict.empty


{-| Insert key-value pair into dictionary, ordered last. On collision replace value and keep current position.
-}
insert : comparable -> v -> SortableDict comparable v -> SortableDict comparable v
insert key value (SortableDict nextPos posToKV keyToPos keyToValue) =
    case Dict.get key keyToPos of
        Just pos ->
            SortableDict nextPos
                (Dict.insert pos ( key, value ) posToKV)
                keyToPos
                (Dict.insert key value keyToValue)

        Nothing ->
            let
                newPos =
                    nextPos

                newNextPos =
                    nextPos + 1
            in
            SortableDict newNextPos
                (Dict.insert newPos ( key, value ) posToKV)
                (Dict.insert key newPos keyToPos)
                (Dict.insert key value keyToValue)


{-| Update value at speficic key with given function
-}
update : comparable -> (Maybe v -> Maybe v) -> SortableDict comparable v -> SortableDict comparable v
update key fn sortableDict =
    case fn (get key sortableDict) of
        Just newValue ->
            insert key newValue sortableDict

        Nothing ->
            remove key sortableDict


{-| Remove key and associated value from dictionary. If key doesn't exist, no changes are made
-}
remove : comparable -> SortableDict comparable v -> SortableDict comparable v
remove key ((SortableDict nextPos posToKV keyToPos keyToValue) as sortableDict) =
    case Dict.get key keyToPos of
        Just pos ->
            SortableDict
                nextPos
                (Dict.remove pos posToKV)
                (Dict.remove key keyToPos)
                (Dict.remove key keyToValue)

        Nothing ->
            sortableDict



-- QUERY


{-| Determine if dictionary is empty.
-}
isEmpty : SortableDict comparable v -> Bool
isEmpty (SortableDict nextPos posToKV keyToPos keyToValue) =
    Dict.isEmpty keyToValue


{-| Determine if key is in dictionary.
-}
member : comparable -> SortableDict comparable v -> Bool
member key (SortableDict nextPos posToKV keyToPos keyToValue) =
    Dict.member key keyToValue


{-| Get value at specific key.
-}
get : comparable -> SortableDict comparable v -> Maybe v
get key (SortableDict nextPos posToKV keyToPos keyToValue) =
    Dict.get key keyToValue



-- LISTS


{-| Get all keys in current order.
-}
keys : SortableDict comparable v -> List comparable
keys (SortableDict nextPos posToKV keyToPos keyToValue) =
    Dict.values posToKV
        |> List.map Tuple.first


{-| Get all `(key, value)` tuples in current order.
-}
toList : SortableDict comparable v -> List ( comparable, v )
toList (SortableDict nextPos posToKV keyToPos keyToValue) =
    Dict.values posToKV


{-| Create dictionary from `List (key, value)`, remembering the order.
-}
fromList : List ( comparable, v ) -> SortableDict comparable v
fromList list =
    SortableDict
        (List.length list)
        (Dict.fromList <| List.indexedMap (\p kv -> ( p, kv )) list)
        (Dict.fromList <| List.indexedMap (\p ( k, v ) -> ( k, p )) list)
        (Dict.fromList list)
