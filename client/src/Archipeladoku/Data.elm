module Archipeladoku.Data exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Dict exposing (Dict)


type alias Board =
    { blockSize : Int
    , givens : Dict ( Int, Int ) Int
    , puzzleAreas : PuzzleAreas
    , solution : Dict ( Int, Int ) Int
    , unlockOrder : List ( Int, Int )
    }


type alias Area =
    { startRow : Int
    , startCol : Int
    , endRow : Int
    , endCol : Int
    }


type alias PuzzleAreas =
    { blocks : List Area
    , boards : List Area
    , rows : List Area
    , cols : List Area
    }


type alias GenerateArgs =
    { blockSize : Int
    , difficulty : Int
    , numberOfBoards : Int
    , seed : Int
    }


blockSizeToDimensions : Int -> ( Int, Int )
blockSizeToDimensions blockSize =
    case blockSize of
        4 ->
            ( 2, 2 )

        6 ->
            ( 2, 3 )

        8 ->
            ( 2, 4 )

        9 ->
            ( 3, 3 )

        12 ->
            ( 3, 4 )

        16 ->
            ( 4, 4 )

        _ ->
            ( 1, 1 )


buildCellAreasMap : List Area -> Dict ( Int, Int ) (List Area)
buildCellAreasMap areas =
    List.foldl
        (\area acc ->
            let
                areaCells : List ( Int, Int )
                areaCells =
                    getAreaCells area
            in
            List.foldl
                (\cell acc2 ->
                    let
                        existingAreas : List Area
                        existingAreas =
                            Dict.get cell acc2
                                |> Maybe.withDefault []
                    in
                    Dict.insert cell (area :: existingAreas) acc2
                )
                acc
                areaCells
        )
        Dict.empty
        areas


boardDecoder : Decode.Decoder Board
boardDecoder =
    Decode.map5 Board
        (Decode.field "blockSize" Decode.int)
        (Decode.field "givens" (cellsDictDecoder Decode.int))
        (Decode.field "puzzleAreas" puzzleAreasDecoder)
        (Decode.field "solution" (cellsDictDecoder Decode.int))
        (Decode.field "unlockOrder" (Decode.list (tupleDecoder Decode.int Decode.int)))


cellsDictDecoder : Decode.Decoder a -> Decode.Decoder (Dict ( Int, Int ) a)
cellsDictDecoder valueDecoder =
    Decode.list
        (Decode.map3
            (\row col value ->
                ( ( row, col ), value )
            )
            (Decode.index 0 Decode.int)
            (Decode.index 1 Decode.int)
            (Decode.index 2 valueDecoder)
        )
        |> Decode.map Dict.fromList


areaDecoder : Decode.Decoder Area
areaDecoder =
    Decode.map4 Area
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Decode.int)
        (Decode.index 3 Decode.int)


puzzleAreasDecoder : Decode.Decoder PuzzleAreas
puzzleAreasDecoder =
    Decode.map4 PuzzleAreas
        (Decode.field "blocks" (Decode.list areaDecoder))
        (Decode.field "boards" (Decode.list areaDecoder))
        (Decode.field "rows" (Decode.list areaDecoder))
        (Decode.field "cols" (Decode.list areaDecoder))


tupleDecoder : Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder ( a, b )
tupleDecoder decodeA decodeB =
    Decode.map2 Tuple.pair
        (Decode.index 0 decodeA)
        (Decode.index 1 decodeB)


encodeGenerateArgs : GenerateArgs -> Encode.Value
encodeGenerateArgs args =
    Encode.object
        [ ( "blockSize", Encode.int args.blockSize )
        , ( "difficulty", Encode.int args.difficulty )
        , ( "numberOfBoards", Encode.int args.numberOfBoards )
        , ( "seed", Encode.int args.seed )
        ]


getAreaCells : Area -> List ( Int, Int )
getAreaCells area =
    List.concatMap
        (\row ->
            List.map
                (Tuple.pair row)
                (List.range area.startCol area.endCol)
        )
        (List.range area.startRow area.endRow)


