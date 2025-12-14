module Archipeladoku.Json exposing (..)

import Archipeladoku.Engine as Engine
import Json.Decode as Decode
import Json.Encode as Encode
import Dict exposing (Dict)


encodeBoard : Engine.Board -> Encode.Value
encodeBoard board =
    Encode.object
        [ ( "blockSize", Encode.int board.blockSize )
        , ( "givens", encodeCellsDict Encode.int board.givens )
        , ( "puzzleAreas", encodePuzzleAreas board.puzzleAreas )
        , ( "solution", encodeCellsDict Encode.int board.solution )
        , ( "unlockOrder", Encode.list (encodeTuple Encode.int Encode.int) board.unlockOrder )
        ]


boardDecoder : Decode.Decoder Engine.Board
boardDecoder =
    Decode.map5 Engine.Board
        (Decode.field "blockSize" Decode.int)
        (Decode.field "givens" (cellsDictDecoder Decode.int))
        (Decode.field "puzzleAreas" puzzleAreasDecoder)
        (Decode.field "solution" (cellsDictDecoder Decode.int))
        (Decode.field "unlockOrder" (Decode.list (decodeTuple Decode.int Decode.int)))


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


encodePuzzleAreas : Engine.PuzzleAreas -> Encode.Value
encodePuzzleAreas puzzleAreas =
    Encode.object
        [ ( "blocks", Encode.list encodeArea puzzleAreas.blocks )
        , ( "boards", Encode.list encodeArea puzzleAreas.boards )
        , ( "rows", Encode.list encodeArea puzzleAreas.rows )
        , ( "cols", Encode.list encodeArea puzzleAreas.cols )
        ]


puzzleAreasDecoder : Decode.Decoder Engine.PuzzleAreas
puzzleAreasDecoder =
    Decode.map4 Engine.PuzzleAreas
        (Decode.field "blocks" (Decode.list areaDecoder))
        (Decode.field "boards" (Decode.list areaDecoder))
        (Decode.field "rows" (Decode.list areaDecoder))
        (Decode.field "cols" (Decode.list areaDecoder))


encodeTuple : (a -> Encode.Value) -> (b -> Encode.Value) -> (a, b) -> Encode.Value
encodeTuple encodeA encodeB ( a, b ) =
    Encode.list identity
        [ encodeA a
        , encodeB b
        ]


decodeTuple : Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder ( a, b )
decodeTuple decodeA decodeB =
    Decode.map2 Tuple.pair
        (Decode.index 0 decodeA)
        (Decode.index 1 decodeB)


encodeGenerateArgs : Engine.GenerateArgs -> Encode.Value
encodeGenerateArgs args =
    Encode.object
        [ ( "blockSize", Encode.int args.blockSize )
        , ( "difficulty", Encode.int args.difficulty )
        , ( "numberOfBoards", Encode.int args.numberOfBoards )
        , ( "seed", Encode.int args.seed )
        ]


generateArgsDecoder : Decode.Decoder Engine.GenerateArgs
generateArgsDecoder =
    Decode.map4 Engine.GenerateArgs
        (Decode.field "blockSize" Decode.int)
        (Decode.field "difficulty" Decode.int)
        (Decode.field "numberOfBoards" Decode.int)
        (Decode.field "seed" Decode.int)


decodeGenerateArgs2 : Decode.Decoder Engine.Args2
decodeGenerateArgs2 =
    Decode.map5 Engine.Args2
        (Decode.field "blockSize" Decode.int)
        (Decode.field "blockUnlockOrder" (Decode.list (decodeTuple Decode.int Decode.int)))
        (Decode.field "clusters" (Decode.list (Decode.list (decodeTuple Decode.int Decode.int))))
        (Decode.field "seed" Decode.int)
        (Decode.field "unlockedBlocks" Decode.int)
