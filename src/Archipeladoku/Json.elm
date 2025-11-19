module Archipeladoku.Json exposing (..)

import Archipeladoku.Engine as Engine
import Json.Decode as Decode
import Json.Encode as Encode
import Dict exposing (Dict)


encodeBoard : Engine.Board -> Encode.Value
encodeBoard board =
    Encode.object
        [ ( "blockSize", Encode.int board.blockSize )
        , ( "cellBlocks", encodeCellsDict (Encode.list encodeArea) board.cellBlocks )
        , ( "givens", encodeCellsDict Encode.int board.givens )
        , ( "solution", encodeCellsDict Encode.int board.solution )
        ]


boardDecoder : Decode.Decoder Engine.Board
boardDecoder =
    Decode.map4 Engine.Board
        (Decode.field "blockSize" Decode.int)
        (Decode.field "cellBlocks" (cellsDictDecoder (Decode.list areaDecoder)))
        (Decode.field "givens" (cellsDictDecoder Decode.int))
        (Decode.field "solution" (cellsDictDecoder Decode.int))


encodeCellsDict : (a -> Encode.Value) -> Dict ( Int, Int ) a -> Encode.Value
encodeCellsDict encodeValue dict =
    Dict.toList dict
        |> Encode.list
            (\( ( row, col ), value ) ->
                [ Encode.int row
                , Encode.int col
                , encodeValue value
                ]
                    |> Encode.list identity
            )


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


encodeArea : Engine.Area -> Encode.Value
encodeArea area =
    Encode.list Encode.int
        [ area.startRow
        , area.startCol
        , area.endRow
        , area.endCol
        ]


areaDecoder : Decode.Decoder Engine.Area
areaDecoder =
    Decode.map4 Engine.Area
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Decode.int)
        (Decode.index 3 Decode.int)


encodeGenerateArgs : Engine.GenerateArgs -> Encode.Value
encodeGenerateArgs args =
    Encode.object
        [ ( "blockSize", Encode.int args.blockSize )
        , ( "overlap", Encode.int args.overlap )
        , ( "numberOfBoards", Encode.int args.numberOfBoards )
        , ( "seed", Encode.int args.seed )
        ]


generateArgsDecoder : Decode.Decoder Engine.GenerateArgs
generateArgsDecoder =
    Decode.map4 Engine.GenerateArgs
        (Decode.field "blockSize" Decode.int)
        (Decode.field "overlap" Decode.int)
        (Decode.field "numberOfBoards" Decode.int)
        (Decode.field "seed" Decode.int)
