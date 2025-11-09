module Archipeladoku exposing (..)

import Archipeladoku.Engine as Engine
import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Attributes.Extra as HAE
import Html.Events as HE
import Json.Decode as Decode
import List.Extra
import MultiDict exposing (MultiDict)
import Random


type alias Model =
    { board : Engine.Board
    }


type Msg
    = NoOp


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : Decode.Value -> ( Model, Cmd Msg )
init flagsValue =
    let
        generationResult =
            Engine.generate
                { blockSize = 2
                , overlap = 1
                , numberOfBoards = 5
                , seed = Random.initialSeed 1
                }

        board =
            case generationResult of
                Ok b ->
                    b

                Err err ->
                    let
                        _ =
                            Debug.log "Generation error" err
                    in
                    { solution = Dict.empty
                    , current = Dict.empty
                    , blockSize = 0
                    , puzzleAreas = []
                    }
    in
    ( { board = board }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.node "style"
            []
            [ Html.text """
                body {
                    background-color: #222222;
                    color: #eeeeee;
                }

                .board td {
                    border: 1px solid #888888;
                    width: 40px;
                    height: 40px;
                    text-align: center;
                    font-size: 24px;
                }

                .board td:empty {
                    border: none;
                }

                .board td.block-border-top {
                    border-top: 2px solid #aaaaaa;
                }

                .board td.block-border-bottom {
                    border-bottom: 2px solid #aaaaaa;
                }

                .board td.block-border-left {
                    border-left: 2px solid #aaaaaa;
                }

                .board td.block-border-right {
                    border-right: 2px solid #aaaaaa;
                }
                """
            ]
        , viewBoard model.board
        ]


qwer : List Int -> Dict Int Engine.CellValue
qwer list =
    List.indexedMap
        (\i v ->
            if v == 0 then
                Nothing

            else
                Just ( i + 1, Engine.Given v )
        )
        list
        |> List.filterMap identity
        |> Dict.fromList


createEmpty : Int -> Int -> Int -> MultiDict Int Engine.CellValue
createEmpty blockSize startRow startCol =
    let
        size : Int
        size =
            blockSize * blockSize

        rows : List Int
        rows =
            List.range startRow (startRow + size - 1)

        cols : List Int
        cols =
            List.range startCol (startCol + size - 1)
    in
    List.Extra.cartesianProduct [ rows, cols ]
        |> List.filterMap
            (\list ->
                case list of
                    [ row, col ] ->
                        Just ( row, col )

                    _ ->
                        Nothing
            )
        |> List.foldl
            (\( row, col ) acc ->
                MultiDict.insert row col Engine.Empty acc
            )
            Dict.empty


testBoard : Engine.Board
testBoard =
    let
        blockSize : Int
        blockSize =
            2

        overlap : Int
        overlap =
            1

        boards : Int
        boards =
            11
    in
    { blockSize = blockSize
    , solution = Dict.empty
    , current =
        List.foldl
            (\(row, col) acc ->
                MultiDict.union (createEmpty blockSize row col) acc
            )
            Dict.empty
            (Engine.positionBoards blockSize overlap boards)
        -- Dict.fromList
        --     [ ( 1, qwer [ 1, 2, 3, 4, 0, 0, 1, 2, 3, 4 ] )
        --     , ( 2, qwer [ 3, 4, 1, 2, 0, 0, 3, 4, 1, 2 ] )
        --     , ( 3, qwer [ 2, 1, 4, 3, 0, 0, 2, 1, 4, 3 ] )
        --     , ( 4, qwer [ 4, 3, 2, 1, 2, 3, 4, 3, 2, 1 ] )
        --     , ( 5, qwer [ 0, 0, 0, 3, 4, 1, 2 ] )
        --     , ( 6, qwer [ 0, 0, 0, 2, 1, 4, 3 ] )
        --     , ( 7, qwer [ 0, 0, 0, 4, 3, 2, 1 ] )
        --     ]
            -- [ ( 1, qwer <| List.range 1 9 )
            -- , ( 2, qwer <| List.range 4 9 ++ List.range 1 3 )
            -- , ( 3, qwer <| List.range 7 9 ++ List.range 1 6 )
            -- , ( 4, qwer <| List.range 2 9 ++ List.range 1 1 )
            -- , ( 5, qwer <| List.range 5 9 ++ List.range 1 4 )
            -- , ( 6, qwer <| List.range 8 9 ++ List.range 1 7 )
            -- , ( 7, qwer <| List.range 3 9 ++ List.range 1 2 ++ List.range 3 8 )
            -- , ( 8, qwer <| List.range 6 9 ++ List.range 1 5 )
            -- , ( 9, qwer <| List.range 9 9 ++ List.range 1 8 )
            -- ]
    , puzzleAreas =
        List.map
            (\( row, col ) ->
                Engine.buildPuzzleAreas blockSize row col
            )
            (Engine.positionBoards blockSize overlap boards)
    }


viewBoard : Engine.Board -> Html Msg
viewBoard board =
    Html.table
        [ HA.style "border-collapse" "collapse"
        , HA.class "board"
        ]
        (List.map
            (\( row, rowDict ) -> viewRow board row rowDict)
            (Dict.toList board.current)
        )


viewRow : Engine.Board -> Int -> Dict Int Engine.CellValue -> Html Msg
viewRow board row rowDict =
    Html.tr
        []
        (List.map
            (\col ->
                case Dict.get col rowDict of
                    Just value ->
                        viewCell board row col value

                    Nothing ->
                        Html.td [] []
            )
            (List.range
                1
                (List.maximum (Dict.keys rowDict)
                    |> Maybe.withDefault 0
                )
            )
        )


viewCell : Engine.Board -> Int -> Int -> Engine.CellValue -> Html Msg
viewCell board row col value =
    let
        blocks =
            Engine.getAreasAt row col .blocks board.puzzleAreas
    in
    Html.td
        [ HAE.attributeIf
            (List.any (.startRow >> (==) row) blocks)
            (HA.class "block-border-top")
        , HAE.attributeIf
            (List.any (.endRow >> (==) row) blocks)
            (HA.class "block-border-bottom")
        , HAE.attributeIf
            (List.any (.startCol >> (==) col) blocks)
            (HA.class "block-border-left")
        , HAE.attributeIf
            (List.any (.endCol >> (==) col) blocks)
            (HA.class "block-border-right")
        ]
        [ Html.text (Engine.getCellText 2 value) ]





