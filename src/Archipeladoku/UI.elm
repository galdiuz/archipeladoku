port module Archipeladoku.UI exposing (..)

import Archipeladoku.Engine as Engine
import Archipeladoku.Json as Json
import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Attributes.Extra as HAE
import Html.Events as HE
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Random
import Set exposing (Set)


port receiveBoard : (Decode.Value -> msg) -> Sub msg
port generateBoard : Encode.Value -> Cmd msg


type alias Model =
    { board : Maybe BoardState
    , selectedCell : Maybe ( Int, Int )
    }


type Msg
    = CellSelected ( Int, Int )
    | GotBoard Decode.Value
    | KeyPressed Int


type alias BoardState =
    { blockSize : Int
    , cellBlocks : Dict ( Int, Int ) (List Engine.Area)
    , current : Dict ( Int, Int ) CellValue
    , solution : Dict ( Int, Int ) Int
    }


type CellValue
    = Given Int
    | Single Int
    | Multiple (Set Int)


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
    ( { board = Nothing
      , selectedCell = Nothing
      }
    , generateBoard
        (Json.encodeGenerateArgs
            { blockSize = 3
            , overlap = 3
            , numberOfBoards = 3
            , seed = 1
            }
        )
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveBoard GotBoard
        , Browser.Events.onKeyPress keyDecoder
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case Maybe.map Tuple.first (String.uncons key) of
                    Just char ->
                        if Char.isHexDigit char then
                            if Char.isDigit char then
                                Char.toCode char - Char.toCode '0'
                                    |> KeyPressed
                                    |> Decode.succeed

                            else
                                Char.toCode (Char.toUpper char) - Char.toCode 'A' + 10
                                    |> KeyPressed
                                    |> Decode.succeed

                        else
                            Decode.fail key

                    Nothing ->
                        Decode.fail key
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellSelected ( row, col ) ->
            ( { model | selectedCell = Just ( row, col ) }
            , Cmd.none
            )

        GotBoard value ->
            case Decode.decodeValue Json.boardDecoder value of
                Ok board ->
                    ( { model
                        | board =
                            Just
                                { cellBlocks = board.cellBlocks
                                , blockSize = board.blockSize
                                , current = Dict.map (\_ v -> Given v) board.givens
                                , solution = board.solution
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

        KeyPressed number ->
            let
                _ = Debug.log "Key pressed in update:" number
            in
            case ( model.selectedCell, model.board ) of
                ( Just ( row, col ), Just board ) ->
                    let
                        newCurrent : Dict ( Int, Int ) CellValue
                        newCurrent =
                            if number < 1 || number > board.blockSize * board.blockSize then
                                board.current

                            else
                                Dict.update
                                    ( row, col )
                                    (toggleNumber number)
                                    board.current
                    in
                    ( { model | board = Just { board | current = newCurrent } }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )


toggleNumber : Int -> Maybe CellValue -> Maybe CellValue
toggleNumber number maybeCellValue =
    case maybeCellValue of
        Just (Given v) ->
            Just <| Given v

        Just (Single v) ->
            if v == number then
                Nothing

            else
                Just <| Multiple <| Set.fromList [ v, number ]

        Just (Multiple numbers) ->
            if Set.member number numbers then
                if Set.size numbers == 2 then
                    Set.remove number numbers
                        |> Set.toList
                        |> List.head
                        |> Maybe.map Single

                else
                    Just <| Multiple <| Set.remove number numbers

            else
                Just <| Multiple <| Set.insert number numbers

        Nothing ->
            Just <| Single number


view : Model -> Html Msg
view model =
    Html.div
        [ HA.class "dark-mode" ]
        [ Html.node "style"
            []
            [ Html.text css ]
        , case model.board of
            Just board ->
                viewBoard model board

            Nothing ->
                Html.text "No board loaded."
        ]


viewBoard : Model -> BoardState -> Html Msg
viewBoard model board =
    let
        rows : Int
        rows =
            Dict.keys board.solution
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 0

        cols : Int
        cols =
            Dict.keys board.solution
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0
    in
    Html.div
        [ HA.class "board"
        , HA.class <| "block-" ++ String.fromInt board.blockSize
        ]
        (List.map
            (viewCell model board)
            (List.range 1 (rows + 1)
                |> List.concatMap
                    (\row ->
                        List.range 1 (cols + 1)
                            |> List.map (Tuple.pair row)
                    )
            )
        )


viewCell : Model -> BoardState -> ( Int, Int ) -> Html Msg
viewCell model board ( row, col ) =
    let
        cellIsAt : ( Int, Int ) -> Bool
        cellIsAt ( r, c ) =
            Dict.member ( r, c ) board.solution

        blocks : List Engine.Area
        blocks =
            Dict.get ( row, col ) board.cellBlocks
                |> Maybe.withDefault []

        blockAbove : List Engine.Area
        blockAbove =
            Dict.get ( row - 1, col ) board.cellBlocks
                |> Maybe.withDefault []

        blockLeft : List Engine.Area
        blockLeft =
            Dict.get ( row, col - 1 ) board.cellBlocks
                |> Maybe.withDefault []
    in
    if cellIsAt ( row, col ) then
        Html.button
            [ HA.class "cell"
            , HA.style "grid-row" (String.fromInt row)
            , HA.style "grid-column" (String.fromInt col)
            , HAE.attributeIf
                (Dict.get ( row, col ) board.current
                    |> Maybe.map isGiven
                    |> Maybe.withDefault False
                )
                (HA.style "font-weight" "700")
            , HAE.attributeMaybe
                (\v -> HA.class <| "val-" ++ String.fromInt v)
                (Dict.get ( row, col ) board.current
                    |> Maybe.andThen cellValueToInt
                )
            , HA.classList
                [ ( "selected", model.selectedCell == Just ( row, col ) )
                , ( "block-border-top"
                  , not (cellIsAt ( row - 1, col ))
                    || List.any (.startRow >> (==) row) blocks
                    || List.any (.endRow >> (==) (row - 1)) blockAbove
                  )
                , ( "block-border-left"
                  , not (cellIsAt ( row, col - 1 ))
                    || List.any (.startCol >> (==) col) blocks
                    || List.any (.endCol >> (==) (col - 1)) blockLeft
                  )
                ]
            , HE.onClick (CellSelected ( row, col ))
            ]
            (case Dict.get ( row, col ) board.current of
                Just value ->
                    case value of
                        Given v ->
                            [ Html.text (numberToString board.blockSize v) ]

                        Single v ->
                            [ Html.text (numberToString board.blockSize v) ]

                        Multiple numbers ->
                            viewMultipleNumbers board.blockSize numbers

                Nothing ->
                    []
            )

    else
        Html.div
            [ HA.style "grid-row" (String.fromInt row)
            , HA.style "grid-column" (String.fromInt col)
            , HA.classList
                [ ( "block-border-top", cellIsAt ( row - 1, col ) )
                , ( "block-border-left", cellIsAt ( row, col - 1 ) )
                ]
            ]
            []


viewMultipleNumbers : Int -> Set Int -> List (Html Msg)
viewMultipleNumbers blockSize numbers =
    let
        gridSize : Int
        gridSize =
            Set.size numbers
                |> toFloat
                |> sqrt
                |> ceiling
    in
    numbers
        |> Set.toList
        |> List.indexedMap
            (\idx n ->
                let
                    row : Int
                    row =
                        if idx == 0 then
                            1

                        else
                            (idx // gridSize) + 1

                    col : Int
                    col =
                        if idx == 0 then
                            1

                        else
                            modBy gridSize idx + 1
                in
                Html.div
                    [ HA.style "grid-row" (String.fromInt row)
                    , HA.style "grid-column" (String.fromInt col)
                    , HA.style "font-size" (String.fromFloat (1 / toFloat gridSize) ++ "em")
                    , HA.style "align-self" "stretch"
                    , HA.class "center"
                    , HA.class <| "val-" ++ String.fromInt n
                    ]
                    [ Html.text (numberToString blockSize n) ]
            )


cellValueToInt : CellValue -> Maybe Int
cellValueToInt cellValue =
    case cellValue of
        Given v ->
            Just v

        Single v ->
            Just v

        Multiple _ ->
            Nothing


isGiven : CellValue -> Bool
isGiven cellValue =
    case cellValue of
        Given _ ->
            True

        _ ->
            False


numberToString : Int -> Int -> String
numberToString blockSize number =
    if blockSize == 4 then
        if number <= 9 then
            String.fromInt (number - 1)

        else
            Char.fromCode (number - 10 + Char.toCode 'A')
                |> String.fromChar

    else
        String.fromInt number


css : String
css =
    """
    :root {
        color-scheme: dark;

        --h4-1: 0;
        --h4-2: 60;
        --h4-3: 120;
        --h4-4: 210;

        --h9-1: 0;
        --h9-2: 30;
        --h9-3: 60;
        --h9-4: 90;
        --h9-5: 150;
        --h9-6: 195;
        --h9-7: 225;
        --h9-8: 270;
        --h9-9: 315;

        --h16-1: 0;
        --h16-2: 22;
        --h16-3: 45;
        --h16-4: 67;
        --h16-5: 90;
        --h16-6: 112;
        --h16-7: 135;
        --h16-8: 157;
        --h16-9: 180;
        --h16-10: 202;
        --h16-11: 225;
        --h16-12: 247;
        --h16-13: 270;
        --h16-14: 292;
        --h16-15: 315;
        --h16-16: 337;
    }

    body {
        background-color: light-dark(#eeeeee, #222222);
        color: light-dark(#000000, #eeeeee);
        padding: 1em;
    }

    .board {
        display: grid;
        grid-auto-rows: 1.5em;
        grid-auto-columns: 1.5em;
        font-size: 24px;
    }

    .cell {
        display: grid;
        grid-auto-rows: 1fr;
        grid-auto-columns: 1fr;
        justify-content: center;
        align-items: center;
        background-color: inherit;
        color: inherit;
        font-size: inherit;
        font-family: inherit;
        padding: 0;
        border-top: 1px solid #888888;
        border-bottom: none;
        border-left: 1px solid #888888;
        border-right: none;
    }

    .cell.selected {
        z-index: 10;
        box-shadow: 1px 1px 0 2px light-dark(#aaaaaa, #aaaaaa),
                    1px 1px 0 5px light-dark(#0066cc, #00d9ff);
         /* border: none !important; */
    }

    .center {
        display: flex;
        justify-content: center;
        align-items: center;
    }

    .block-border-top {
        border-top: 2px solid #aaaaaa;
    }

    .block-border-bottom {
        border-bottom: 2px solid #aaaaaa;
    }

    .block-border-left {
        border-left: 2px solid #aaaaaa;
    }

    .block-border-right {
        border-right: 2px solid #aaaaaa;
    }

    .block-2 .val-1  { background-color: light-dark(hsl(var(--h4-1), 60%, 80%), hsl(var(--h4-1), 60%, 25%)); }
    .block-2 .val-2  { background-color: light-dark(hsl(var(--h4-2), 60%, 80%), hsl(var(--h4-2), 60%, 20%)); }
    .block-2 .val-3  { background-color: light-dark(hsl(var(--h4-3), 60%, 80%), hsl(var(--h4-3), 60%, 25%)); }
    .block-2 .val-4  { background-color: light-dark(hsl(var(--h4-4), 60%, 80%), hsl(var(--h4-4), 60%, 30%)); }

    .block-3 .val-1  { background-color: light-dark(hsl(var(--h9-1), 60%, 80%), hsl(var(--h9-1), 60%, 25%)); }
    .block-3 .val-2  { background-color: light-dark(hsl(var(--h9-2), 60%, 80%), hsl(var(--h9-2), 60%, 25%)); }
    .block-3 .val-3  { background-color: light-dark(hsl(var(--h9-3), 60%, 80%), hsl(var(--h9-3), 60%, 20%)); }
    .block-3 .val-4  { background-color: light-dark(hsl(var(--h9-4), 60%, 80%), hsl(var(--h9-4), 60%, 25%)); }
    .block-3 .val-5  { background-color: light-dark(hsl(var(--h9-5), 60%, 80%), hsl(var(--h9-5), 60%, 25%)); }
    .block-3 .val-6  { background-color: light-dark(hsl(var(--h9-6), 60%, 80%), hsl(var(--h9-6), 60%, 25%)); }
    .block-3 .val-7  { background-color: light-dark(hsl(var(--h9-7), 60%, 80%), hsl(var(--h9-7), 60%, 30%)); }
    .block-3 .val-8  { background-color: light-dark(hsl(var(--h9-8), 60%, 80%), hsl(var(--h9-8), 60%, 30%)); }
    .block-3 .val-9  { background-color: light-dark(hsl(var(--h9-9), 60%, 80%), hsl(var(--h9-9), 60%, 25%)); }
    """
