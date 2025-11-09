module MultiDict exposing (..)

import Dict exposing (Dict)


type alias MultiDict k v =
    Dict k (Dict k v)


get : comparable -> comparable -> MultiDict comparable v -> Maybe v
get key1 key2 multiDict =
    Dict.get key1 multiDict
        |> Maybe.andThen (Dict.get key2)


insert : comparable -> comparable -> v -> MultiDict comparable v -> MultiDict comparable v
insert key1 key2 value multiDict =
    Dict.update
        key1
        (\maybeInnerDict ->
            maybeInnerDict
                |> Maybe.withDefault Dict.empty
                |> Dict.insert key2 value
                |> Just
        )
        multiDict


map : (a -> b) -> MultiDict comparable a -> MultiDict comparable b
map f multiDict =
    Dict.map
        (\_ innerDict ->
            Dict.map (\_ v -> f v) innerDict
        )
        multiDict


union : MultiDict comparable v -> MultiDict comparable v -> MultiDict comparable v
union dict1 dict2 =
    Dict.merge
        (\key val acc ->
            Dict.insert key val acc
        )
        (\key val1 val2 acc ->
            Dict.insert key (Dict.union val1 val2) acc
        )
        (\key val acc ->
            Dict.insert key val acc
        )
        dict1
        dict2
        Dict.empty
