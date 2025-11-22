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
    , errors : Dict ( Int, Int ) (Set Int)
    , puzzleAreas : Engine.PuzzleAreas
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
            { blockSize = 9
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
                                , errors = Dict.empty
                                , puzzleAreas = board.puzzleAreas
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
                    ( { model | board = Just <| updateBoard { board | current = newCurrent } }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )


updateBoard : BoardState -> BoardState
updateBoard board =
    { board | errors = getBoardErrors board }


getBoardErrors : BoardState -> Dict ( Int, Int ) (Set Int)
getBoardErrors board =
    List.foldl
        (\area errors ->
            let
                areaCells : List ( Int, Int )
                areaCells =
                    Engine.getAreaCells area
            in
            List.foldl
                (\cell acc ->
                    case Dict.get cell board.current of
                        Just (Given v) ->
                            let
                                numbersInArea : Set Int
                                numbersInArea =
                                    areaCells
                                        |> List.filter ((/=) cell)
                                        |> List.filterMap (\areaCell -> Dict.get areaCell board.current)
                                        |> List.map cellValueToInts
                                        |> List.foldl Set.union Set.empty
                            in
                            if Set.member v numbersInArea then
                                insertDictSetValue cell v acc

                            else
                                acc

                        Just (Single v) ->
                            let
                                numbersInArea : Set Int
                                numbersInArea =
                                    areaCells
                                        |> List.filter ((/=) cell)
                                        |> List.filterMap (\areaCell -> Dict.get areaCell board.current)
                                        |> List.map cellValueToInts
                                        |> List.foldl Set.union Set.empty
                            in
                            if Set.member v numbersInArea then
                                insertDictSetValue cell v acc

                            else
                                acc

                        Just (Multiple numbers) ->
                            let
                                numbersInArea : Set Int
                                numbersInArea =
                                    areaCells
                                        |> List.filter ((/=) cell)
                                        |> List.filterMap (\areaCell -> Dict.get areaCell board.current)
                                        |> List.filterMap cellValueToInt
                                        |> Set.fromList
                            in
                            Dict.update
                                cell
                                (\maybeSet ->
                                    case maybeSet of
                                        Just set ->
                                            Just <| Set.union set numbersInArea

                                        Nothing ->
                                            Just numbersInArea
                                )
                                acc

                        Nothing ->
                            acc

                )
                errors
                areaCells
        )
        Dict.empty
        (List.concat
            [ board.puzzleAreas.rows
            , board.puzzleAreas.cols
            , board.puzzleAreas.blocks
            ]
        )


insertDictSetValue : ( Int, Int ) -> Int -> Dict ( Int, Int ) (Set Int) -> Dict ( Int, Int ) (Set Int)
insertDictSetValue key value dict =
    Dict.update
        key
        (\maybeSet ->
            case maybeSet of
                Just set ->
                    Just <| Set.insert value set

                Nothing ->
                    Just <| Set.singleton value
        )
        dict


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
        []
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

        errorsAtCell : Set Int
        errorsAtCell =
            Dict.get ( row, col ) board.errors
                |> Maybe.withDefault Set.empty

        cellIsGiven : Bool
        cellIsGiven =
            Dict.get ( row, col ) board.current
                |> Maybe.map isGiven
                |> Maybe.withDefault False

        cellIsMultiple : Bool
        cellIsMultiple =
            Dict.get ( row, col ) board.current
                |> Maybe.map isMultiple
                |> Maybe.withDefault False
    in
    if cellIsAt ( row, col ) then
        Html.button
            [ HA.class "cell"
            , HA.style "grid-row" (String.fromInt row)
            , HA.style "grid-column" (String.fromInt col)
            , HAE.attributeMaybe
                (\v -> HA.class <| "val-" ++ String.fromInt v)
                (Dict.get ( row, col ) board.current
                    |> Maybe.andThen cellValueToInt
                )
            , HA.classList
                [ ( "selected", model.selectedCell == Just ( row, col ) )
                , ( "given", cellIsGiven )
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
                , ( "multi", cellIsMultiple )
                , ( "error", (not <| Set.isEmpty errorsAtCell) && (not cellIsMultiple) )
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
                            viewMultipleNumbers board.blockSize errorsAtCell numbers

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


viewMultipleNumbers : Int -> Set Int -> Set Int -> List (Html Msg)
viewMultipleNumbers blockSize errorsAtCell numbers =
    List.map
        (\number ->
            let
                blockWidth : Int
                blockWidth =
                    Tuple.first (Engine.blockSizeToDimensions blockSize)

                row : Int
                row =
                    (number - 1) // blockWidth + 1

                col : Int
                col =
                    modBy blockWidth (number - 1) + 1
            in
            Html.div
                [ HA.style "grid-row" (String.fromInt row)
                , HA.style "grid-column" (String.fromInt col)
                , HA.class "center"
                , HA.class <| "val-" ++ String.fromInt number
                , HAE.attributeIf
                    (Set.member number errorsAtCell)
                    (HA.class "error")
                ]
                [ Html.text (numberToString blockSize number) ]
        )
        (Set.toList numbers)


cellValueToInt : CellValue -> Maybe Int
cellValueToInt cellValue =
    case cellValue of
        Given v ->
            Just v

        Single v ->
            Just v

        Multiple _ ->
            Nothing


cellValueToInts : CellValue -> Set Int
cellValueToInts cellValue =
    case cellValue of
        Given v ->
            Set.fromList [ v ]

        Single v ->
            Set.fromList [ v ]

        Multiple numbers ->
            numbers


isGiven : CellValue -> Bool
isGiven cellValue =
    case cellValue of
        Given _ ->
            True

        _ ->
            False


isMultiple : CellValue -> Bool
isMultiple cellValue =
    case cellValue of
        Multiple _ ->
            True

        _ ->
            False


numberToString : Int -> Int -> String
numberToString blockSize number =
    if blockSize >= 10 then
        if number <= 10 then
            String.fromInt (number - 1)

        else
            Char.fromCode (number - 11 + Char.toCode 'A')
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

        --h6-1: 0;
        --h6-2: 40;
        --h6-3: 110;
        --h6-4: 180;
        --h6-5: 240;
        --h6-6: 300;

        --h8-1: 0;
        --h8-2: 35;
        --h8-3: 65;
        --h8-4: 120;
        --h8-5: 170;
        --h8-6: 220;
        --h8-7: 275;
        --h8-8: 320;

        --h9-1: 0;
        --h9-2: 30;
        --h9-3: 60;
        --h9-4: 90;
        --h9-5: 150;
        --h9-6: 195;
        --h9-7: 225;
        --h9-8: 270;
        --h9-9: 315;

        --h12-1: 0;
        --h12-2: 30;
        --h12-3: 60;
        --h12-4: 90;
        --h12-5: 120;
        --h12-6: 150;
        --h12-7: 180;
        --h12-8: 210;
        --h12-9: 240;
        --h12-10: 270;
        --h12-11: 300;
        --h12-12: 330;

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
        font-size: 32px;
    }

    .cell {
        display: flex;
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
    }

    .cell.given {
        font-weight: 700;
        font-family: arial;
    }

    .cell.multi {
        display: grid;
        align-items: stretch;
    }

    .cell.error, .cell .error {
        color: light-dark(#aa0000, #ff8888);
        text-decoration: underline;
        font-weight: 700;
    }

    .block-4 .cell.multi {
        grid-template-rows: repeat(2, 1fr);
        grid-template-columns: repeat(2, 1fr);
        font-size: 0.6em;
    }

    .block-6 .cell.multi {
        grid-template-rows: repeat(2, 1fr);
        grid-template-columns: repeat(3, 1fr);
        font-size: 0.5em;
    }

    .block-8 .cell.multi {
        grid-template-rows: repeat(2, 1fr);
        grid-template-columns: repeat(4, 1fr);
        font-size: 0.45em;
    }

    .block-9 .cell.multi {
        grid-template-rows: repeat(3, 1fr);
        grid-template-columns: repeat(3, 1fr);
        font-size: 0.4em;
    }

    .block-16 .cell.multi {
        grid-template-rows: repeat(4, 1fr);
        grid-template-columns: repeat(4, 1fr);
        font-size: 0.3em;
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

    .block-4 .val-1 { background-color: light-dark(hsl(var(--h4-1), 60%, 80%), hsl(var(--h4-1), 60%, 25%)); }
    .block-4 .val-2 { background-color: light-dark(hsl(var(--h4-2), 60%, 80%), hsl(var(--h4-2), 60%, 20%)); }
    .block-4 .val-3 { background-color: light-dark(hsl(var(--h4-3), 60%, 80%), hsl(var(--h4-3), 60%, 25%)); }
    .block-4 .val-4 { background-color: light-dark(hsl(var(--h4-4), 60%, 80%), hsl(var(--h4-4), 60%, 30%)); }

    .block-6 .val-1 { background-color: light-dark(hsl(var(--h6-1), 60%, 80%), hsl(var(--h6-1), 60%, 25%)); }
    .block-6 .val-2 { background-color: light-dark(hsl(var(--h6-2), 60%, 80%), hsl(var(--h6-2), 60%, 20%)); }
    .block-6 .val-3 { background-color: light-dark(hsl(var(--h6-3), 60%, 80%), hsl(var(--h6-3), 60%, 20%)); }
    .block-6 .val-4 { background-color: light-dark(hsl(var(--h6-4), 60%, 80%), hsl(var(--h6-4), 60%, 25%)); }
    .block-6 .val-5 { background-color: light-dark(hsl(var(--h6-5), 60%, 80%), hsl(var(--h6-5), 60%, 30%)); }
    .block-6 .val-6 { background-color: light-dark(hsl(var(--h6-6), 60%, 80%), hsl(var(--h6-6), 60%, 25%)); }

    .block-8 .val-1 { background-color: light-dark(hsl(var(--h8-1), 60%, 80%), hsl(var(--h8-1), 60%, 25%)); }
    .block-8 .val-2 { background-color: light-dark(hsl(var(--h8-2), 60%, 80%), hsl(var(--h8-2), 60%, 25%)); }
    .block-8 .val-3 { background-color: light-dark(hsl(var(--h8-3), 60%, 80%), hsl(var(--h8-3), 60%, 20%)); }
    .block-8 .val-4 { background-color: light-dark(hsl(var(--h8-4), 60%, 80%), hsl(var(--h8-4), 60%, 20%)); }
    .block-8 .val-5 { background-color: light-dark(hsl(var(--h8-5), 60%, 80%), hsl(var(--h8-5), 60%, 20%)); }
    .block-8 .val-6 { background-color: light-dark(hsl(var(--h8-6), 60%, 80%), hsl(var(--h8-6), 60%, 30%)); }
    .block-8 .val-7 { background-color: light-dark(hsl(var(--h8-7), 60%, 80%), hsl(var(--h8-7), 60%, 30%)); }
    .block-8 .val-8 { background-color: light-dark(hsl(var(--h8-8), 60%, 80%), hsl(var(--h8-8), 60%, 25%)); }

    .block-9 .val-1 { background-color: light-dark(hsl(var(--h9-1), 60%, 80%), hsl(var(--h9-1), 60%, 25%)); }
    .block-9 .val-2 { background-color: light-dark(hsl(var(--h9-2), 60%, 80%), hsl(var(--h9-2), 60%, 25%)); }
    .block-9 .val-3 { background-color: light-dark(hsl(var(--h9-3), 60%, 80%), hsl(var(--h9-3), 60%, 20%)); }
    .block-9 .val-4 { background-color: light-dark(hsl(var(--h9-4), 60%, 80%), hsl(var(--h9-4), 60%, 25%)); }
    .block-9 .val-5 { background-color: light-dark(hsl(var(--h9-5), 60%, 80%), hsl(var(--h9-5), 60%, 25%)); }
    .block-9 .val-6 { background-color: light-dark(hsl(var(--h9-6), 60%, 80%), hsl(var(--h9-6), 60%, 25%)); }
    .block-9 .val-7 { background-color: light-dark(hsl(var(--h9-7), 60%, 80%), hsl(var(--h9-7), 60%, 30%)); }
    .block-9 .val-8 { background-color: light-dark(hsl(var(--h9-8), 60%, 80%), hsl(var(--h9-8), 60%, 30%)); }
    .block-9 .val-9 { background-color: light-dark(hsl(var(--h9-9), 60%, 80%), hsl(var(--h9-9), 60%, 25%)); }

    .block-12 .val-1 { background-color: light-dark(hsl(var(--h12-1), 60%, 80%), hsl(var(--h12-1), 60%, 25%)); }
    .block-12 .val-2 { background-color: light-dark(hsl(var(--h12-2), 60%, 80%), hsl(var(--h12-2), 60%, 25%)); }
    .block-12 .val-3 { background-color: light-dark(hsl(var(--h12-3), 60%, 80%), hsl(var(--h12-3), 60%, 20%)); }
    .block-12 .val-4 { background-color: light-dark(hsl(var(--h12-4), 60%, 80%), hsl(var(--h12-4), 60%, 20%)); }
    .block-12 .val-5 { background-color: light-dark(hsl(var(--h12-5), 60%, 80%), hsl(var(--h12-5), 60%, 20%)); }
    .block-12 .val-6 { background-color: light-dark(hsl(var(--h12-6), 60%, 80%), hsl(var(--h12-6), 60%, 20%)); }
    .block-12 .val-7 { background-color: light-dark(hsl(var(--h12-7), 60%, 80%), hsl(var(--h12-7), 60%, 25%)); }
    .block-12 .val-8 { background-color: light-dark(hsl(var(--h12-8), 60%, 80%), hsl(var(--h12-8), 60%, 30%)); }
    .block-12 .val-9 { background-color: light-dark(hsl(var(--h12-9), 60%, 80%), hsl(var(--h12-9), 60%, 30%)); }
    .block-12 .val-10 { background-color: light-dark(hsl(var(--h12-10), 60%, 80%), hsl(var(--h12-10), 60%, 30%)); }
    .block-12 .val-11 { background-color: light-dark(hsl(var(--h12-11), 60%, 80%), hsl(var(--h12-11), 60%, 25%)); }
    .block-12 .val-12 { background-color: light-dark(hsl(var(--h12-12), 60%, 80%), hsl(var(--h12-12), 60%, 25%)); }

    .block-16 .val-1 { background-color: light-dark(hsl(var(--h16-1), 60%, 80%), hsl(var(--h16-1), 60%, 25%)); }
    .block-16 .val-2 { background-color: light-dark(hsl(var(--h16-2), 60%, 80%), hsl(var(--h16-2), 60%, 25%)); }
    .block-16 .val-3 { background-color: light-dark(hsl(var(--h16-3), 60%, 80%), hsl(var(--h16-3), 60%, 20%)); }
    .block-16 .val-4 { background-color: light-dark(hsl(var(--h16-4), 60%, 80%), hsl(var(--h16-4), 60%, 20%)); }
    .block-16 .val-5 { background-color: light-dark(hsl(var(--h16-5), 60%, 80%), hsl(var(--h16-5), 60%, 20%)); }
    .block-16 .val-6 { background-color: light-dark(hsl(var(--h16-6), 60%, 80%), hsl(var(--h16-6), 60%, 20%)); }
    .block-16 .val-7 { background-color: light-dark(hsl(var(--h16-7), 60%, 80%), hsl(var(--h16-7), 60%, 20%)); }
    .block-16 .val-8 { background-color: light-dark(hsl(var(--h16-8), 60%, 80%), hsl(var(--h16-8), 60%, 20%)); }
    .block-16 .val-9 { background-color: light-dark(hsl(var(--h16-9), 60%, 80%), hsl(var(--h16-9), 60%, 25%)); }
    .block-16 .val-10 { background-color: light-dark(hsl(var(--h16-10), 60%, 80%), hsl(var(--h16-10), 60%, 30%)); }
    .block-16 .val-11 { background-color: light-dark(hsl(var(--h16-11), 60%, 80%), hsl(var(--h16-11), 60%, 30%)); }
    .block-16 .val-12 { background-color: light-dark(hsl(var(--h16-12), 60%, 80%), hsl(var(--h16-12), 60%, 30%)); }
    .block-16 .val-13 { background-color: light-dark(hsl(var(--h16-13), 60%, 80%), hsl(var(--h16-13), 60%, 30%)); }
    .block-16 .val-14 { background-color: light-dark(hsl(var(--h16-14), 60%, 80%), hsl(var(--h16-14), 60%, 25%)); }
    .block-16 .val-15 { background-color: light-dark(hsl(var(--h16-15), 60%, 80%), hsl(var(--h16-15), 60%, 25%)); }
    .block-16 .val-16 { background-color: light-dark(hsl(var(--h16-16), 60%, 80%), hsl(var(--h16-16), 60%, 25%)); }
    """
