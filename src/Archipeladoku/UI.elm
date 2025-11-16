port module Archipeladoku.UI exposing (..)

import Archipeladoku.Engine as Engine
import Archipeladoku.Json as Json
import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Attributes.Extra as HAE
import Html.Events as HE
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Random


port receiveBoard : (Decode.Value -> msg) -> Sub msg
port generateBoard : Encode.Value -> Cmd msg


type alias Model =
    { board : Maybe BoardState
    }


type Msg
    = GotBoard Decode.Value


type alias BoardState =
    { solution : Dict ( Int, Int ) Int
    , current : Dict ( Int, Int ) CellValue
    , blockSize : Int
    }


type CellValue
    = Given Int
    | UserInput Int


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
    ( { board = Nothing }
    , generateBoard
        (Json.encodeGenerateArgs
            { blockSize = 3
            , overlap = 3
            , numberOfBoards = 5
            , seed = 1
            }
        )
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveBoard GotBoard
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBoard value ->
            case Decode.decodeValue Json.boardDecoder value of
                Ok board ->
                    ( { model
                        | board =
                            Just
                                { solution = board.solution
                                , current = Dict.map (\_ v -> Given v) board.givens
                                , blockSize = board.blockSize
                                }
                    }
                    , Cmd.none
                    )

                Err err ->
                    let
                        _ =
                            Debug.log "Decoding error" err
                    in
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
                    font-size: 24px;
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
        , case model.board of
            Just board ->
                viewBoard board

            Nothing ->
                Html.text "No board loaded."
        ]


viewBoard : BoardState -> Html Msg
viewBoard board =
    Html.div
        [ HA.class "board"
        ]
        (List.map
            (viewCell board)
            (Dict.keys board.solution)
        )


viewCell : BoardState -> ( Int, Int ) -> Html Msg
viewCell board ( row, col ) =
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
        [ case Dict.get ( row, col ) board.current of
            Just value ->
                Html.text (getCellText board.blockSize value)

            Nothing ->
                Html.text " "
        ]


cellValueToInt : CellValue -> Maybe Int
cellValueToInt cellValue =
    case cellValue of
        Given v ->
            Just v

        UserInput v ->
            Just v


getCellText : Int -> CellValue -> String
getCellText blockSize cellValue =
    let
        maybeInt : Maybe Int
        maybeInt =
            cellValueToInt cellValue
    in
    if blockSize == 4 then
        case maybeInt of
            Just v ->
                if v <= 9 then
                    String.fromInt (v - 1)

                else
                    Char.fromCode (v - 10 + Char.toCode 'A')
                        |> String.fromChar

            Nothing ->
                ""

    else
        maybeInt
            |> Maybe.map String.fromInt
            |> Maybe.withDefault " "
