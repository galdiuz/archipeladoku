module Archipeladoku.Json exposing (..)

import Archipeladoku.Engine as Engine
import Json.Decode as Decode
import Json.Encode as Encode
import Dict exposing (Dict)


boardDecoder : Decode.Decoder Engine.Board
boardDecoder =
    Decode.map3 Engine.Board
        (Decode.field "solution" cellsDecoder)
        (Decode.field "givens" cellsDecoder)
        (Decode.field "blockSize" Decode.int)


encodeBoard : Engine.Board -> Encode.Value
encodeBoard board =
    Encode.object
        [ ( "solution", encodeCells board.solution )
        , ( "givens", encodeCells board.givens )
        , ( "blockSize", Encode.int board.blockSize )
        ]


encodeCells : Dict ( Int, Int ) Int -> Encode.Value
encodeCells dict =
    Dict.toList dict
        |> Encode.list
            (\( ( row, col ), value ) ->
                Encode.list Encode.int [ row, col, value ]
            )


cellsDecoder : Decode.Decoder (Dict ( Int, Int ) Int)
cellsDecoder =
    Decode.list
        (Decode.map3
            (\row col value -> ( ( row, col ), value ))
            (Decode.index 0 Decode.int)
            (Decode.index 1 Decode.int)
            (Decode.index 2 Decode.int)
        )
        |> Decode.map Dict.fromList


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
