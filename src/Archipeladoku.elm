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
                , numberOfBoards = 25
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

                .board {
                    display: grid;
                    grid-auto-rows: 1.5em;
                    grid-auto-columns: 1.5em;
                    font-size: 32px;
                }

                .cell {
                    border: 1px solid #888888;
                    display: flex;
                    justify-content: center;
                    align-items: center;
                }

                .cell:empty {
                    border: none;
                }

                .cell.block-border-top {
                    border-top: 2px solid #aaaaaa;
                }

                .cell.block-border-bottom {
                    border-bottom: 2px solid #aaaaaa;
                }

                .cell.block-border-left {
                    border-left: 2px solid #aaaaaa;
                }

                .cell.block-border-right {
                    border-right: 2px solid #aaaaaa;
                }
                """
            ]
        , viewBoard model.board
        ]


viewBoard : Engine.Board -> Html Msg
viewBoard board =
    Html.div
        [ HA.class "board"
        ]
        (List.map
            (\( a, b ) -> viewCell board a b)
            (Dict.toList board.current)
        )


viewCell : Engine.Board -> ( Int, Int ) -> Engine.CellValue -> Html Msg
viewCell board ( row, col ) value =
    let
        blocks : List Engine.Area
        blocks =
            []
            -- Engine.getAreasAt row col .blocks board.puzzleAreas
    in
    Html.div
        [ HA.class "cell"
        , HA.style "grid-row" (String.fromInt row)
        , HA.style "grid-column" (String.fromInt col)
        , HAE.attributeIf
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
        [ Html.text (Engine.getCellText board.blockSize value) ]





