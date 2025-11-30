port module Archipeladoku.UI exposing (..)

import Archipeladoku.Engine as Engine
import Archipeladoku.Json as Json
import Array exposing (Array)
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
port connect : Encode.Value -> Cmd msg
port checkLocation : Int -> Cmd msg
port receiveItems : (List Int -> msg) -> Sub msg
port receiveCheckedLocations : (List Int -> msg) -> Sub msg


type alias Model =
    { board : Maybe BoardState
    , gameIsLocal : Bool
    , host : String
    , pendingBlockUnlocks : Set ( Int, Int )
    , pendingCellChanges : Set ( Int, Int )
    , pendingSolvedBlocks : Set ( Int, Int )
    , player : String
    , selectedCell : Maybe ( Int, Int )
    }


type Msg
    = CellSelected ( Int, Int )
    | ConnectPressed
    | GotBoard Decode.Value
    | GotCheckedLocations (List Int)
    | GotItems (List Int)
    | HostInputChanged String
    | KeyPressed Int
    | PlayLocalPressed
    | PlayerInputChanged String


type alias BoardState =
    { blockSize : Int
    , cellBlocks : Dict ( Int, Int ) (List Engine.Area)
    , current : Dict ( Int, Int ) CellValue
    , errors : Dict ( Int, Int ) (Set Int)
    , lockedBlocks : List ( Int, Int )
    , puzzleAreas : Engine.PuzzleAreas
    , solution : Dict ( Int, Int ) Int
    , unlockedBlocks : Set ( Int, Int )
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
      , gameIsLocal = False
      , host = "localhost:8123"
      , pendingBlockUnlocks = Set.empty
      , pendingCellChanges = Set.empty
      , pendingSolvedBlocks = Set.empty
      , player = "Player1"
      , selectedCell = Nothing
      }
    , Cmd.none
    -- , connect
    --     (Encode.object
            -- [ ( "host", Encode.string "localhost:8123" )
            -- , ( "player", Encode.string "player1" )
            -- , ( "password", Encode.null )
            -- ]
    --     )
    -- , generateBoard
    --     (Json.encodeGenerateArgs
    --         { blockSize = blockSize
    --         , overlapRows = Engine.blockSizeToOverlap blockSize |> Tuple.first
    --         , overlapCols = Engine.blockSizeToOverlap blockSize |> Tuple.second
    --         , numberOfBoards = 5
    --         , seed = 1
    --         }
    --     )
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveBoard GotBoard
        , receiveItems GotItems
        , receiveCheckedLocations GotCheckedLocations
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
                            if char == '0' then
                                KeyPressed 10
                                    |> Decode.succeed

                            else if Char.isDigit char then
                                Char.toCode char - Char.toCode '0'
                                    |> KeyPressed
                                    |> Decode.succeed

                            else
                                Char.toCode (Char.toUpper char) - Char.toCode 'A' + 11
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

        ConnectPressed ->
            ( model
            , connect
                (Encode.object
                    [ ( "host", Encode.string model.host )
                    , ( "player", Encode.string model.player )
                    , ( "password", Encode.null )
                    ]
                )
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
                                , lockedBlocks = List.drop board.unlockCount board.unlockOrder
                                , puzzleAreas = board.puzzleAreas
                                , solution = board.solution
                                , unlockedBlocks =
                                    board.unlockOrder
                                        |> List.take board.unlockCount
                                        |> Set.fromList
                                }
                      }
                    , Cmd.none
                    )
                        |> updateBoard

                Err err ->
                    let
                        _ =
                            Debug.log "Decoding error" err
                    in
                    ( model, Cmd.none )

        GotCheckedLocations locationIds ->
            ( { model
                | pendingSolvedBlocks =
                    Set.union
                        model.pendingSolvedBlocks
                        (locationIds
                            |> List.filterMap
                                (\id ->
                                    if id >= 1000000 then
                                        Just (blockFromId id)

                                    else
                                        Nothing
                                )
                            |> Debug.log "Solved blocks"
                            |> Set.fromList
                        )
              }
            , Cmd.none
            )
                |> updateBoard

        GotItems itemIds ->
            ( { model
                | pendingBlockUnlocks =
                    Set.union
                        model.pendingBlockUnlocks
                        (itemIds
                            |> List.filterMap
                                (\id ->
                                    if id >= 1000000 then
                                        Just (blockFromId id)

                                    else
                                        Nothing
                                )
                            |> Debug.log "Received blocks"
                            |> Set.fromList
                        )
              }
            , Cmd.none
            )
                |> updateBoard

        HostInputChanged value ->
            ( { model | host = value }
            , Cmd.none
            )

        KeyPressed number ->
            case ( model.selectedCell, model.board ) of
                ( Just ( row, col ), Just board ) ->
                    let
                        newCurrent : Dict ( Int, Int ) CellValue
                        newCurrent =
                            if number < 1 || number > board.blockSize then
                                board.current

                            else
                                Dict.update
                                    ( row, col )
                                    (toggleNumber number)
                                    board.current
                    in
                    ( { model
                        | board = Just { board | current = newCurrent }
                        , pendingCellChanges = Set.insert ( row, col ) model.pendingCellChanges
                      }
                    , Cmd.none
                    )
                        |> updateBoard

                _ ->
                    ( model
                    , Cmd.none
                    )

        PlayLocalPressed ->
            let
                blockSize = 4
            in
            ( { model | gameIsLocal = True }
            , generateBoard
                (Json.encodeGenerateArgs
                    { blockSize = blockSize
                    , overlapRows = Engine.blockSizeToOverlap blockSize |> Tuple.first
                    , overlapCols = Engine.blockSizeToOverlap blockSize |> Tuple.second
                    , numberOfBoards = 5
                    , seed = 1
                    }
                )
            )

        PlayerInputChanged value ->
            ( { model | player = value }
            , Cmd.none
            )


andThen : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen fun ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            fun model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


updateBoardStateInModel : Model -> BoardState -> Model
updateBoardStateInModel model board =
    { model | board = Just board }


updateBoard : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateBoard ( model, cmd ) =
    case model.board of
        Just _ ->
            ( model, cmd )
                |> andThen updateBoardCellChanges
                |> andThen updateBoardBlockUnlocks
                |> andThen updateBoardSolvedBlocks
                |> andThen updateBoardErrors

        Nothing ->
            ( model, cmd )


updateBoardCellChanges : Model -> ( Model, Cmd Msg )
updateBoardCellChanges model =
    Set.foldl
        (andThen << updateBoardCellChange)
        ( { model | pendingCellChanges = Set.empty }
        , Cmd.none
        )
        model.pendingCellChanges


updateBoardBlockUnlocks : Model -> ( Model, Cmd Msg )
updateBoardBlockUnlocks model =
    Set.foldl
        (andThen << updateBoardBlockUnlock)
        ( { model | pendingBlockUnlocks = Set.empty }
        , Cmd.none
        )
        model.pendingBlockUnlocks


updateBoardSolvedBlocks : Model -> ( Model, Cmd Msg )
updateBoardSolvedBlocks model =
    Set.foldl
        (andThen << updateBoardSolvedBlock)
        ( { model | pendingSolvedBlocks = Set.empty }
        , Cmd.none
        )
        model.pendingSolvedBlocks


updateBoardErrors : Model -> ( Model, Cmd Msg )
updateBoardErrors model =
    case model.board of
        Just board ->
            ( { board | errors = getBoardErrors board }
                |> updateBoardStateInModel model
            , Cmd.none
            )

        Nothing ->
            ( model
            , Cmd.none
            )


updateBoardCellChange : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
updateBoardCellChange updatedCell model =
    case model.board of
        Just board ->
            let
                blocksAtCell : List Engine.Area
                blocksAtCell =
                    Dict.get updatedCell board.cellBlocks
                        |> Maybe.withDefault []
            in
            List.foldl
                (\block ( boardAcc, cmd ) ->
                    let
                        isSolved : ( Int, Int ) -> Bool
                        isSolved cell =
                            case ( Dict.get cell boardAcc.current, Dict.get cell boardAcc.solution ) of
                                ( Just (Given v), Just sol ) ->
                                    v == sol

                                ( Just (Single v), Just sol ) ->
                                    v == sol

                                _ ->
                                    False

                        blockCells : List ( Int, Int )
                        blockCells =
                            Engine.getAreaCells block
                    in
                    if List.all isSolved blockCells then
                        { boardAcc
                            | current =
                                List.foldl
                                    (\cell acc ->
                                        Dict.insert
                                            cell
                                            (Given (Dict.get cell boardAcc.solution |> Maybe.withDefault 0))
                                            acc
                                    )
                                    boardAcc.current
                                    blockCells
                        }
                            |> (\newBoard ->
                                    if model.gameIsLocal then
                                        ( unlockNextBlock newBoard
                                        , cmd
                                        )

                                    else
                                        ( newBoard
                                        , checkLocation (cellToLocationId ( block.startRow, block.startCol ))
                                        )
                               )

                    else
                        ( boardAcc
                        , cmd
                        )
                )
                ( board
                , Cmd.none
                )
                blocksAtCell
                |> Tuple.mapFirst (updateBoardStateInModel model)

        Nothing ->
            ( model
            , Cmd.none
            )


unlockNextBlock : BoardState -> BoardState
unlockNextBlock board =
    case board.lockedBlocks of
        block :: remainingBlocks ->
            { board
                | lockedBlocks = remainingBlocks
                , unlockedBlocks = Set.insert block board.unlockedBlocks
            }

        [] ->
            board


getBoardErrors : BoardState -> Dict ( Int, Int ) (Set Int)
getBoardErrors board =
    List.foldl
        (\area errors ->
            let
                areaCells : List ( Int, Int )
                areaCells =
                    Engine.getAreaCells area

                cellIsVisible : ( Int, Int ) -> Bool
                cellIsVisible cell =
                    Set.intersect
                        (Dict.get cell board.cellBlocks
                            |> Maybe.withDefault []
                            |> List.map (\blockArea -> ( blockArea.startRow, blockArea.startCol ))
                            |> Set.fromList
                        )
                        board.unlockedBlocks
                        |> Set.isEmpty
                        |> not
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
                                        |> List.filter cellIsVisible
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
                                        |> List.filter cellIsVisible
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
                                        |> List.filter cellIsVisible
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


updateBoardBlockUnlock : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
updateBoardBlockUnlock cell model =
    case model.board of
        Just board ->
            ( { board
                | lockedBlocks =
                    List.filter ((/=) cell) board.lockedBlocks
                , unlockedBlocks =
                    Set.insert cell board.unlockedBlocks
              }
                |> updateBoardStateInModel model
            , Cmd.none
            )

        Nothing ->
            ( model
            , Cmd.none
            )


updateBoardSolvedBlock : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
updateBoardSolvedBlock block model =
    case model.board of
        Just board ->
            let
                cells : List ( Int, Int )
                cells =
                    Dict.get block board.cellBlocks
                        |> Maybe.withDefault []
                        |> List.Extra.find
                            (\area ->
                                area.startRow == Tuple.first block
                                    && area.startCol == Tuple.second block
                            )
                        |> Maybe.map Engine.getAreaCells
                        |> Maybe.withDefault []
            in
            ( { board
                | current =
                    List.foldl
                        (\cell acc ->
                            Dict.insert
                                cell
                                (Given (Dict.get cell board.solution |> Maybe.withDefault 0))
                                acc
                        )
                        board.current
                        cells
              }
                |> updateBoardStateInModel model
            , Cmd.none
            )

        Nothing ->
            ( model
            , Cmd.none
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
                Html.div
                    []
                    [ Html.input
                        [ HA.type_ "text"
                        , HA.placeholder "Host"
                        , HA.value model.host
                        , HE.onInput HostInputChanged
                        ]
                        []
                    , Html.input
                        [ HA.type_ "text"
                        , HA.placeholder "Player name"
                        , HA.value model.player
                        , HE.onInput PlayerInputChanged
                        ]
                        []
                    , Html.button
                        [ HE.onClick ConnectPressed ]
                        [ Html.text "Connect"]
                    , Html.br [] []
                    , Html.button
                        [ HE.onClick PlayLocalPressed ]
                        [ Html.text "Play Singleplayer" ]
                    ]
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
            (\( row, col ) ->
                if row == 0 && col == 0 then
                    Html.text ""

                else if row == 0 && col <= cols then
                    Html.div
                        [ HA.style "grid-row" "1"
                        , HA.style "grid-column" (String.fromInt (col + 1))
                        , HA.style "font-size" "0.75em"
                        , HA.class "center"
                        ]
                        [ Html.text (String.fromInt col) ]

                else if col == 0 && row <= rows then
                    Html.div
                        [ HA.style "grid-row" (String.fromInt (row + 1))
                        , HA.style "grid-column" "1"
                        , HA.style "font-size" "0.75em"
                        , HA.class "center"
                        ]
                        [ Html.text (rowToLabel row) ]

                 else
                    viewCell model board ( row, col )
            )
            (List.range 0 (rows + 1)
                |> List.concatMap
                    (\row ->
                        List.range 0 (cols + 1)
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

        blockPositions : Set ( Int, Int )
        blockPositions =
            List.map
                (\area -> ( area.startRow, area.startCol ))
                blocks
                |> Set.fromList

        isHidden : Bool
        isHidden =
            Set.intersect blockPositions board.unlockedBlocks
                |> Set.isEmpty

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
            , HA.style "grid-row" (String.fromInt (row + 1))
            , HA.style "grid-column" (String.fromInt (col + 1))
            , HAE.attributeMaybe
                (\v ->
                    if isHidden then
                        HAE.empty

                    else
                        HA.class <| "val-" ++ String.fromInt v
                )
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
                , ( "hidden", isHidden )
                , ( "error", (not <| Set.isEmpty errorsAtCell) && (not cellIsMultiple) && (not isHidden) )
                ]
            , HA.disabled isHidden
            , HE.onClick (CellSelected ( row, col ))
            ]
            (if isHidden then
                []

             else
                case Dict.get ( row, col ) board.current of
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
            [ HA.style "grid-row" (String.fromInt (row + 1))
            , HA.style "grid-column" (String.fromInt (col + 1))
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
    if number < 10 then
        String.fromInt (number)

    else if number == 10 then
        "0"

    else
        Char.fromCode (number - 11 + Char.toCode 'A')
            |> String.fromChar


rowToLabel : Int -> String
rowToLabel row =
    rowToLabelHelper row ""


rowToLabelHelper : Int -> String -> String
rowToLabelHelper row label =
    if row <= 0 then
        label

    else
        let
            chars : Array String
            chars =
                Array.fromList
                    [ "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M"
                    , "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"
                    ]

            base : Int
            base =
                Array.length chars

            rem : Int
            rem =
                modBy base (row - 1)

            next : Int
            next =
                (row - 1) // base

            char =
                Array.get rem chars
                    |> Maybe.withDefault ""
        in
        rowToLabelHelper next (char ++ label)


cellToLocationId : ( Int, Int ) -> Int
cellToLocationId ( row, col ) =
    1000000 + row * 1000 + col


blockFromId : Int -> ( Int, Int )
blockFromId id =
    ( (modBy 1000000 id) // 1000, modBy 1000 id )


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

    .cell.hidden {
        background-color: light-dark(#888888, #888888);
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

    .block-12 .cell.multi {
        grid-template-rows: repeat(3, 1fr);
        grid-template-columns: repeat(4, 1fr);
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
