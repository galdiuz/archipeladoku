port module Archipeladoku.UI exposing (..)

import Archipeladoku.Engine as Engine
import Archipeladoku.Json as Json
import Array exposing (Array)
import Bitwise
import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Extra
import Html.Attributes as HA
import Html.Attributes.Extra as HAE
import Html.Events as HE
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra
import List.Extra
import Process
import Random
import Set exposing (Set)
import Set.Extra
import Task


port checkLocation : Int -> Cmd msg
port connect : Encode.Value -> Cmd msg
port generateBoard : Encode.Value -> Cmd msg
port goal : () -> Cmd msg
port hintForItem : String -> Cmd msg
port log : String -> Cmd msg
port moveCellIntoView : String -> Cmd msg
port scoutLocations : List Int -> Cmd msg
port sendMessage : String -> Cmd msg
port triggerAnimation : Encode.Value -> Cmd msg
port zoom : Encode.Value -> Cmd msg
port zoomReset : () -> Cmd msg

port receiveBoard : (Decode.Value -> msg) -> Sub msg
port receiveCheckedLocations : (List Int -> msg) -> Sub msg
port receiveConnectionStatus : (Bool -> msg) -> Sub msg
port receiveGenerationProgress : (Decode.Value -> msg) -> Sub msg
port receiveHintCost : (Int -> msg) -> Sub msg
port receiveHintPoints : (Int -> msg) -> Sub msg
port receiveHints : (Decode.Value -> msg) -> Sub msg
port receiveItems : (List Int -> msg) -> Sub msg
port receiveMessage : (Decode.Value -> msg) -> Sub msg
port receiveScoutedItems : (Decode.Value -> msg) -> Sub msg


type alias Model =
    { blockSize : Int
    , candidateMode : Bool
    , cellBlocks : Dict ( Int, Int ) (List Engine.Area)
    , cellBoards : Dict ( Int, Int ) (List Engine.Area)
    , cellCols : Dict ( Int, Int ) (List Engine.Area)
    , cellRows : Dict ( Int, Int ) (List Engine.Area)
    , current : Dict ( Int, Int ) CellValue
    , difficulty : Int
    , errors : Dict ( Int, Int ) (Set Int)
    , generationProgress : ( String, Float )
    , gameIsLocal : Bool
    , gameState : GameState
    , heldKeys : Set String
    , hints : Dict Int Hint
    , hintCost : Int
    , hintPoints : Int
    , host : String
    , lockedBlocks : List ( Int, Int )
    , messageInput : String
    , messages : List Message
    , numberOfBoards : Int
    , pendingCellChanges : Set ( Int, Int )
    , pendingItems : List Item
    , pendingScoutLocations : Set Int
    , pendingSolvedBlocks : Set ( Int, Int )
    , pendingSolvedCols : Set ( Int, Int )
    , pendingSolvedRows : Set ( Int, Int )
    , player : String
    , puzzleAreas : Engine.PuzzleAreas
    , scoutedItems : Dict Int Hint
    , seed : Random.Seed
    , seedInput : Int
    , selectedCell : Maybe ( Int, Int )
    , shiftDebounce : Int
    , solution : Dict ( Int, Int ) Int
    , solveRandomCellUses : Int
    , solveSelectedCellUses : Int -- TODO: Need to persist these counts
    , solvedLocations : Set Int
    , unlockedBlocks : Set ( Int, Int )
    , visibleCells : Set ( Int, Int )
    }


type Msg
    = BlockSizeChanged Int
    | CellSelected ( Int, Int )
    | ConnectPressed
    | DifficultyChanged Int
    | GotBoard Decode.Value
    | GotCheckedLocations (List Int)
    | GotConnectionStatus Bool
    | GotGenerationProgress Decode.Value
    | GotHintCost Int
    | GotHintPoints Int
    | GotHints Decode.Value
    | GotItems (List Int)
    | GotMessage Decode.Value
    | GotScoutedItems Decode.Value
    | HintItemPressed String
    | HostInputChanged String
    | KeyPressed Input
    | KeyReleased Input
    | MessageInputChanged String
    | NumberOfBoardsChanged Int
    | PlayLocalPressed
    | PlayerInputChanged String
    | SeedInputChanged String
    | SendMessagePressed
    | ShiftDebouncePassed Int
    | SolveRandomCellPressed
    | SolveSelectedCellPressed
    | ToggleCandidateModePressed
    | ZoomInPressed
    | ZoomOutPressed
    | ZoomResetPressed


type alias Flags =
    { seed : Int
    }


type GameState
    = MainMenu
    | Connecting
    | Generating
    | Playing


type CellValue
    = Given Int
    | Single Int
    | Multiple (Set Int)


type Item
    = ProgressiveBlock
    | Block ( Int, Int )
    | SolveSelectedCell
    | SolveRandomCell


type Input
    = DeleteInput
    | HoldShiftInput
    | MoveInput ( Int, Int )
    | NumberInput Int
    | ToggleCandidateInput


itemFromId : Int -> Maybe Item
itemFromId id =
    if id >= 1000000 then
        Just <| Block (cellFromId id)

    else if id == 1 then
        Just SolveRandomCell

    else if id == 101 then
        Just ProgressiveBlock

    else if id == 201 then
        Just SolveSelectedCell

    else
        Nothing


type alias Hint =
    { locationId : Int
    , locationName : String
    , itemId : Int
    , itemName : String
    , itemClass : ItemClass
    , playerName : String
    , gameName : String
    }


decodeHint : Decode.Decoder Hint
decodeHint =
    Decode.map7 Hint
        (Decode.field "locationId" Decode.int)
        (Decode.field "locationName" Decode.string)
        (Decode.field "itemId" Decode.int)
        (Decode.field "itemName" Decode.string)
        (Decode.field "itemClass" itemClassDecoder)
        (Decode.field "playerName" Decode.string)
        (Decode.field "gameName" Decode.string)


itemClassDecoder : Decode.Decoder ItemClass
itemClassDecoder =
    Decode.int
        |> Decode.andThen
            (\value ->
                if Bitwise.and 1 value == 1 then
                    Decode.succeed Progression

                else if Bitwise.and 2 value == 2 then
                    Decode.succeed Useful

                else if Bitwise.and 4 value == 4 then
                    Decode.succeed Trap

                else
                    Decode.succeed Filler
            )


type ItemClass
    = Progression
    | Useful
    | Filler
    | Trap


itemClassToString : ItemClass -> String
itemClassToString classification =
    case classification of
        Progression ->
            "Progression"

        Useful ->
            "Useful"

        Filler ->
            "Filler"

        Trap ->
            "Trap"


type alias Message =
    { nodes : List MessageNode
    , extra : MessageExtra
    }


type MessageExtra
    = AdminCommandMessage
    | ChatMessage String
    | CollectedMessage
    | ConnectedMessage
    | CountdownMessage
    | DisconnectedMessage
    | GoaledMessage
    | ItemCheatedMessage
    | ItemHintedMessage
    | ItemSentMessage
    | LocalMessage
    | ReleasedMessage
    | ServerChatMessage
    | TagsUpdatedMessage
    | TutorialMessage
    | UserCommandMessage


messageDecoder : Decode.Decoder Message
messageDecoder =
    Decode.map2 Message
        (Decode.field "nodes" (Decode.list messageNodeDecoder))
        messageExtraDecoder


messageExtraDecoder : Decode.Decoder MessageExtra
messageExtraDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\msgType ->
                case msgType of
                    "adminCommand" ->
                        Decode.succeed AdminCommandMessage

                    "chat" ->
                        Decode.map ChatMessage
                            (Decode.at [ "player", "alias" ] Decode.string)

                    "collected" ->
                        Decode.succeed CollectedMessage

                    "connected" ->
                        Decode.succeed ConnectedMessage

                    "countdown" ->
                        Decode.succeed CountdownMessage

                    "disconnected" ->
                        Decode.succeed DisconnectedMessage

                    "goaled" ->
                        Decode.succeed GoaledMessage

                    "itemCheated" ->
                        Decode.succeed ItemCheatedMessage

                    "itemHinted" ->
                        Decode.succeed ItemHintedMessage

                    "itemSent" ->
                        Decode.succeed ItemSentMessage

                    "released" ->
                        Decode.succeed ReleasedMessage

                    "serverChat" ->
                        Decode.succeed ServerChatMessage

                    "tagsUpdated" ->
                        Decode.succeed TagsUpdatedMessage

                    "tutorial" ->
                        Decode.succeed TutorialMessage

                    "userCommand" ->
                        Decode.succeed UserCommandMessage

                    _ ->
                        Decode.fail ("Unknown message type: " ++ msgType)
            )


type MessageNode
    = ItemMessageNode String
    | LocationMessageNode String
    | ColorMessageNode String String
    | TextualMessageNode String
    | PlayerMessageNode String


messageNodeDecoder : Decode.Decoder MessageNode
messageNodeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\nodeType ->
                case nodeType of
                    "item" ->
                        Decode.map ItemMessageNode
                            (Decode.field "text" Decode.string)

                    "location" ->
                        Decode.map LocationMessageNode
                            (Decode.field "text" Decode.string)

                    "color" ->
                        Decode.map2 ColorMessageNode
                            (Decode.field "color" Decode.string)
                            (Decode.field "text" Decode.string)

                    "text" ->
                        Decode.map TextualMessageNode
                            (Decode.field "text" Decode.string)

                    "player" ->
                        Decode.map PlayerMessageNode
                            (Decode.field "text" Decode.string)

                    _ ->
                        Decode.fail ("Unknown message node type: " ++ nodeType)
            )


decodeGenerationProgress : Decode.Decoder ( String, Float )
decodeGenerationProgress =
    Decode.map2 Tuple.pair
        (Decode.field "label" Decode.string)
        (Decode.field "percent" Decode.float)


encodeTriggerAnimation : String -> List ( Int, Int ) -> Encode.Value
encodeTriggerAnimation animationType cells =
    Encode.object
        [ ( "ids", Encode.list Encode.string (List.map cellHtmlId cells) )
        , ( "type", Encode.string animationType )
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { blockSize = 9
      , candidateMode = False
      , cellBlocks = Dict.empty
      , cellBoards = Dict.empty
      , cellCols = Dict.empty
      , cellRows = Dict.empty
      , current = Dict.empty
      , difficulty = 2
      , errors = Dict.empty
      , generationProgress = ( "", 0 )
      , gameIsLocal = False
      , gameState = MainMenu
      , heldKeys = Set.empty
      , hints = Dict.empty
      , hintCost = 0
      , hintPoints = 0
      , host = "localhost:8123"
      , lockedBlocks = []
      , messageInput = ""
      , messages = []
      , numberOfBoards = 5
      , pendingCellChanges = Set.empty
      , pendingItems = []
      , pendingScoutLocations = Set.empty
      , pendingSolvedBlocks = Set.empty
      , pendingSolvedCols = Set.empty
      , pendingSolvedRows = Set.empty
      , player = "Player1"
      , puzzleAreas =
            { blocks = []
            , boards = []
            , rows = []
            , cols = []
            }
      , scoutedItems = Dict.empty
      , seed = Random.initialSeed (flags.seed + 1)
      , seedInput = flags.seed
      , selectedCell = Nothing
      , shiftDebounce = 0
      , solution = Dict.empty
      , solveRandomCellUses = 0
      , solveSelectedCellUses = 0
      , solvedLocations = Set.empty
      , unlockedBlocks = Set.empty
      , visibleCells = Set.empty
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveBoard GotBoard
        , receiveCheckedLocations GotCheckedLocations
        , receiveConnectionStatus GotConnectionStatus
        , receiveGenerationProgress GotGenerationProgress
        , receiveHintCost GotHintCost
        , receiveHintPoints GotHintPoints
        , receiveHints GotHints
        , receiveItems GotItems
        , receiveMessage GotMessage
        , receiveScoutedItems GotScoutedItems
        , Browser.Events.onKeyDown (keyDownDecoder model)
        , Browser.Events.onKeyUp keyUpDecoder
        ]


keyDownDecoder : Model -> Decode.Decoder Msg
keyDownDecoder model =
    Decode.map2
        (\code target ->
            { code = code
            , target = target
            }
        )
        (Decode.field "code" Decode.string)
        (Decode.at [ "target", "tagName" ] Decode.string)
        |> Decode.andThen
            (\{ code, target } ->
                let
                    keyMap : Dict String Input
                    keyMap =
                        [ ( "ArrowUp", MoveInput ( -1, 0 ) )
                        , ( "ArrowDown", MoveInput ( 1, 0 ) )
                        , ( "ArrowLeft", MoveInput ( 0, -1 ) )
                        , ( "ArrowRight", MoveInput ( 0, 1 ) )
                        , ( "Backspace", DeleteInput )
                        , ( "Delete", DeleteInput )
                        , ( "Digit1", NumberInput 1 )
                        , ( "Digit2", NumberInput 2 )
                        , ( "Digit3", NumberInput 3 )
                        , ( "Digit4", NumberInput 4 )
                        , ( "Digit5", NumberInput 5 )
                        , ( "Digit6", NumberInput 6 )
                        , ( "Digit7", NumberInput 7 )
                        , ( "Digit8", NumberInput 8 )
                        , ( "Digit9", NumberInput 9 )
                        , ( "Digit0", NumberInput 10 )
                        , ( "KeyA", NumberInput 11 )
                        , ( "KeyB", NumberInput 12 )
                        , ( "KeyC", NumberInput 13 )
                        , ( "KeyD", NumberInput 14 )
                        , ( "KeyE", NumberInput 15 )
                        , ( "KeyF", NumberInput 16 )
                        , ( "KeyH", MoveInput ( 0, -1 ) )
                        , ( "KeyJ", MoveInput ( 1, 0 ) )
                        , ( "KeyK", MoveInput ( -1, 0 ) )
                        , ( "KeyL", MoveInput ( 0, 1 ) )
                        , ( "Numpad1", NumberInput 1 )
                        , ( "Numpad2", NumberInput 2 )
                        , ( "Numpad3", NumberInput 3 )
                        , ( "Numpad4", NumberInput 4 )
                        , ( "Numpad5", NumberInput 5 )
                        , ( "Numpad6", NumberInput 6 )
                        , ( "Numpad7", NumberInput 7 )
                        , ( "Numpad8", NumberInput 8 )
                        , ( "Numpad9", NumberInput 9 )
                        , ( "Numpad0", NumberInput 10 )
                        , ( "ShiftLeft", HoldShiftInput )
                        , ( "ShiftRight", HoldShiftInput )
                        , ( "Space", ToggleCandidateInput )
                        ]
                        |> Dict.fromList
                in
                case Dict.get code keyMap of
                    Just key ->
                        if target == "INPUT" then
                            Decode.fail code

                        else
                            Decode.succeed (KeyPressed key)

                    Nothing ->
                        Decode.fail code
            )


keyUpDecoder : Decode.Decoder Msg
keyUpDecoder =
    Decode.field "code" Decode.string
        |> Decode.andThen
            (\code ->
                case code of
                    "ShiftLeft" ->
                        Decode.succeed (KeyReleased HoldShiftInput)

                    "ShiftRight" ->
                        Decode.succeed (KeyReleased HoldShiftInput)

                    _ ->
                        Decode.fail code
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BlockSizeChanged size ->
            ( { model | blockSize = size }
            , Cmd.none
            )

        CellSelected ( row, col ) ->
            ( { model | selectedCell = Just ( row, col ) }
            , Cmd.none
            )

        ConnectPressed ->
            ( { model | gameState = Connecting }
            , connect
                (Encode.object
                    [ ( "host", Encode.string model.host )
                    , ( "player", Encode.string model.player )
                    , ( "password", Encode.null )
                    ]
                )
            )

        DifficultyChanged value ->
            ( { model | difficulty = value }
            , Cmd.none
            )

        GotBoard value ->
            case Decode.decodeValue Json.boardDecoder value of
                Ok board ->
                    List.foldl
                        (\_ ->
                            andThen unlockNextBlock
                        )
                        ( { model
                            | cellBlocks = Engine.buildCellAreasMap board.puzzleAreas.blocks
                            , cellBoards = Engine.buildCellAreasMap board.puzzleAreas.boards
                            , cellCols = Engine.buildCellAreasMap board.puzzleAreas.cols
                            , cellRows = Engine.buildCellAreasMap board.puzzleAreas.rows
                            , blockSize = board.blockSize
                            , current = Dict.map (\_ v -> Given v) board.givens
                            , errors = Dict.empty
                            , gameState = Playing
                            , lockedBlocks = board.unlockOrder
                            , puzzleAreas = board.puzzleAreas
                            , solution = board.solution
                            , unlockedBlocks = Set.empty
                            , pendingItems = model.pendingItems
                          }
                        , Cmd.none
                        )
                        (List.range 1 board.blockSize)
                        |> updateState

                Err err ->
                    ( model, Cmd.none )

        GotCheckedLocations locationIds ->
            ( { model
                | pendingSolvedBlocks =
                    Set.union
                        model.pendingSolvedBlocks
                        (locationIds
                            |> List.filterMap
                                (\id ->
                                    if id >= 1000000 && id < 2000000 then
                                        Just (cellFromId id)

                                    else
                                        Nothing
                                )
                            |> Set.fromList
                        )
                , pendingSolvedCols =
                    Set.union
                        model.pendingSolvedCols
                        (locationIds
                            |> List.filterMap
                                (\id ->
                                    if id >= 3000000 && id < 4000000 then
                                        Just (cellFromId id)

                                    else
                                        Nothing
                                )
                            |> Set.fromList
                        )
                , pendingSolvedRows =
                    Set.union
                        model.pendingSolvedRows
                        (locationIds
                            |> List.filterMap
                                (\id ->
                                    if id >= 2000000 && id < 3000000 then
                                        Just (cellFromId id)

                                    else
                                        Nothing
                                )
                            |> Set.fromList
                        )
              }
            , Cmd.none
            )
                |> updateState

        GotConnectionStatus status ->
            ( { model
                | gameState =
                    case ( status, model.gameState ) of
                        ( True, Connecting ) ->
                            Generating

                        ( False, Connecting ) ->
                            MainMenu

                        -- TODO: Handle disconnect during play

                        _ ->
                            model.gameState
                , messages =
                    if status then
                        model.messages

                    else
                        addLocalMessage "Disconnected from server, reload page." model.messages
              }
            , Cmd.none
            )

        GotGenerationProgress value ->
            case Decode.decodeValue decodeGenerationProgress value of
                Ok progress ->
                    ( { model | generationProgress = progress }
                    , Cmd.none
                    )

                Err err ->
                    ( model, Cmd.none )

        GotHintCost cost ->
            ( { model | hintCost = cost }
            , Cmd.none
            )

        GotHintPoints points ->
            ( { model | hintPoints = points }
            , Cmd.none
            )

        GotHints value ->
            case Decode.decodeValue (Decode.list decodeHint) value of
                Ok hints ->
                    ( { model
                        | hints =
                            List.foldl
                                (\item acc ->
                                    Dict.insert item.itemId item acc
                                )
                                model.hints
                                hints
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                    , Cmd.none
                    )

        GotItems itemIds ->
            ( { model
                | pendingItems =
                    List.append
                        model.pendingItems
                        (itemIds
                            |> List.filterMap itemFromId
                        )
              }
            , Cmd.none
            )
                |> updateState

        GotMessage value ->
            case Decode.decodeValue messageDecoder value of
                Ok message ->
                    ( { model
                        | messages =
                            (message :: model.messages)
                                |> List.take maxMessages

                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                    , log (Decode.errorToString err)
                    )

        GotScoutedItems value ->
            case Decode.decodeValue (Decode.list decodeHint) value of
                Ok scoutedItems ->
                    ( { model
                        | scoutedItems =
                            List.foldl
                                (\item acc ->
                                    Dict.insert item.locationId item acc
                                )
                                model.scoutedItems
                                scoutedItems
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                    , Cmd.none
                    )

        HintItemPressed name ->
            ( model
            , hintForItem name
            )

        HostInputChanged value ->
            ( { model | host = value }
            , Cmd.none
            )

        KeyPressed key ->
            case key of
                DeleteInput ->
                    case model.selectedCell of
                        Just cell ->
                            if Set.member cell model.visibleCells && not (cellIsGiven model cell) then
                                ( { model
                                    | current =
                                        Dict.remove cell model.current
                                    , pendingCellChanges =
                                        Set.insert cell model.pendingCellChanges
                                  }
                                , Cmd.none
                                )
                                    |> updateState

                            else
                                ( model
                                , Cmd.none
                                )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                HoldShiftInput ->
                    ( { model
                        | heldKeys = Set.insert "Shift" model.heldKeys
                        , shiftDebounce = model.shiftDebounce + 1
                      }
                    , Cmd.none
                    )

                MoveInput move ->
                    ( model
                    , Cmd.none
                    )
                        |> andThen (moveSelection move)

                NumberInput number ->
                    case model.selectedCell of
                        Just cell ->
                            if Set.member cell model.visibleCells && not (cellIsGiven model cell) then
                                let
                                    newCurrent : Dict ( Int, Int ) CellValue
                                    newCurrent =
                                        if number < 1 || number > model.blockSize then
                                            model.current

                                        else
                                            if getCandidateMode model then
                                                Dict.update
                                                    cell
                                                    (toggleNumber number)
                                                    model.current

                                            else
                                                Dict.insert
                                                    cell
                                                    (Single number)
                                                    model.current
                                in
                                ( { model
                                    | current = newCurrent
                                    , pendingCellChanges = Set.insert cell model.pendingCellChanges
                                  }
                                , Cmd.none
                                )
                                    |> updateState

                            else
                                ( model
                                , Cmd.none
                                )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                ToggleCandidateInput ->
                    ( { model | candidateMode = not model.candidateMode }
                    , Cmd.none
                    )

        KeyReleased key ->
            case key of
                HoldShiftInput ->
                    ( model
                    , Process.sleep 100
                        |> Task.perform (\_ -> ShiftDebouncePassed model.shiftDebounce)
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        MessageInputChanged value ->
            ( { model | messageInput = value }
            , Cmd.none
            )

        NumberOfBoardsChanged value ->
            ( { model | numberOfBoards = value }
            , Cmd.none
            )

        PlayLocalPressed ->
            ( { model
                | gameIsLocal = True
                , gameState = Generating
              }
            , generateBoard
                (Json.encodeGenerateArgs
                    { blockSize = model.blockSize
                    , difficulty = model.difficulty
                    , numberOfBoards = model.numberOfBoards
                    , seed = model.seedInput
                    }
                )
            )

        PlayerInputChanged value ->
            ( { model | player = value }
            , Cmd.none
            )

        SeedInputChanged value ->
            ( { model
                | seedInput =
                    value
                        |> String.filter Char.isDigit
                        |> String.toInt
                        |> Maybe.withDefault model.seedInput
              }
            , Cmd.none
            )

        SendMessagePressed ->
            ( { model | messageInput = "" }
            , if String.isEmpty model.messageInput || model.gameIsLocal then
                Cmd.none

              else
                sendMessage model.messageInput
            )

        ShiftDebouncePassed debounceId ->
            if model.shiftDebounce == debounceId then
                ( { model | heldKeys = Set.remove "Shift" model.heldKeys }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        SolveRandomCellPressed ->
            let
                cellCandidates : List ( Int, Int )
                cellCandidates =
                    List.filterMap
                        (\cell ->
                            case Dict.get cell model.current of
                                Just (Given _) ->
                                    Nothing

                                _ ->
                                    if Set.member cell model.visibleCells then
                                        Just cell

                                    else
                                        Nothing
                        )
                        (Dict.keys model.solution)

                ( selectedCell, newSeed ) =
                    case cellCandidates of
                        [] ->
                            ( (-1, -1), model.seed )

                        firstCandidate :: restCandidates ->
                            Random.step
                                (Random.uniform firstCandidate restCandidates)
                                model.seed
            in
            if selectedCell == (-1, -1) then
                ( { model
                    | messages =
                        addLocalMessage
                            "Solve Random Cell item could not be used because there are no unsolved visible cells."
                            model.messages
                  }
                , Cmd.none
                )

            else
                ( { model
                    | current =
                        Dict.insert
                            selectedCell
                            (Dict.get selectedCell model.solution
                                |> Maybe.withDefault 0
                                |> Given
                            )
                            model.current
                    , pendingCellChanges = Set.insert selectedCell model.pendingCellChanges
                    , seed = newSeed
                    , solveRandomCellUses = model.solveRandomCellUses - 1
                    , messages =
                        addLocalMessage
                            (String.concat
                                [ "Used Solve Random Cell item at Cell "
                                , rowToLabel (Tuple.first selectedCell)
                                , String.fromInt (Tuple.second selectedCell)
                                ]
                            )
                            model.messages
                  }
                , Cmd.none
                )
                    |> updateState

        SolveSelectedCellPressed ->
            case model.selectedCell of
                Just cell ->
                    case Dict.get cell model.current of
                        Just (Given _) ->
                            ( { model
                                | messages =
                                    addLocalMessage
                                        (String.concat
                                            [ "Solve Selected Cell item at Cell "
                                            , rowToLabel (Tuple.first cell)
                                            , String.fromInt (Tuple.second cell)
                                            , " could not be used because the cell is already solved."
                                            ]
                                        )
                                        model.messages
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model
                                | current =
                                    Dict.insert
                                        cell
                                        (Dict.get cell model.solution
                                            |> Maybe.withDefault 0
                                            |> Given
                                        )
                                        model.current
                                , pendingCellChanges = Set.insert cell model.pendingCellChanges
                                , solveSelectedCellUses = model.solveSelectedCellUses - 1
                                , messages =
                                    addLocalMessage
                                        (String.concat
                                            [ "Used Solve Selected Cell item at Cell "
                                            , rowToLabel (Tuple.first cell)
                                            , String.fromInt (Tuple.second cell)
                                            ]
                                        )
                                        model.messages
                              }
                            , Cmd.none
                            )
                                |> updateState

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        ToggleCandidateModePressed ->
            ( { model | candidateMode = not model.candidateMode }
            , Cmd.none
            )

        ZoomInPressed ->
            ( model
            , zoom
                (Encode.object
                    [ ( "id", Encode.string <| cellHtmlId <| Maybe.withDefault (1, 1) model.selectedCell )
                    , ( "scaleMult", Encode.float 1.5 )
                    ]
                )
            )

        ZoomOutPressed ->
            ( model
            , zoom
                (Encode.object
                    [ ( "id", Encode.string <| cellHtmlId <| Maybe.withDefault (1, 1) model.selectedCell )
                    , ( "scaleMult", Encode.float 0.66 )
                    ]
                )
            )

        ZoomResetPressed ->
            ( model
            , zoomReset ()
            )


moveSelection : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
moveSelection ( rowOffset, colOffset ) model =
    case model.selectedCell of
        Just ( row, col ) ->
            let
                selectedCell : ( Int, Int )
                selectedCell =
                    List.range 1 5
                        |> List.map (\mult -> ( row + rowOffset * mult, col + colOffset * mult ))
                        |> List.filter (\cell -> Dict.member cell model.solution)
                        |> List.head
                        |> Maybe.withDefault ( row, col )
            in
            ( { model
                | selectedCell = Just selectedCell
              }
            , moveCellIntoView (cellHtmlId selectedCell)
            )

        Nothing ->
            ( model
            , Cmd.none
            )


getCandidateMode : Model -> Bool
getCandidateMode model =
    if Set.member "Shift" model.heldKeys then
        not model.candidateMode

    else
        model.candidateMode


andThen : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen fun ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            fun model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


updateState : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateState ( model, cmd ) =
    case model.gameState of
        Playing ->
            ( model, cmd )
                |> andThen updateStateCellChanges
                |> andThen updateStateItems
                |> andThen updateStateSolvedBlocks
                |> andThen updateStateCellChanges
                |> andThen updateStateErrors
                |> andThen updateStateScoutLocations

        _ ->
            ( model, cmd )


updateStateCellChanges : Model -> ( Model, Cmd Msg )
updateStateCellChanges model =
    Set.foldl
        (andThen << updateStateCellChange)
        ( { model | pendingCellChanges = Set.empty }
        , Cmd.none
        )
        model.pendingCellChanges


updateStateItems : Model -> ( Model, Cmd Msg )
updateStateItems model =
    List.foldl
        (andThen << updateStateItem)
        ( { model | pendingItems = [] }
        , Cmd.none
        )
        model.pendingItems


updateStateSolvedBlocks : Model -> ( Model, Cmd Msg )
updateStateSolvedBlocks model =
    ( { model
        | pendingSolvedBlocks = Set.empty
        , pendingSolvedCols = Set.empty
        , pendingSolvedRows = Set.empty
      }
    , Cmd.none
    )
        |> setFoldlChain
            (andThen << (updateStateSolvedArea model.cellBlocks cellToBlockId))
            model.pendingSolvedBlocks
        |> setFoldlChain
            (andThen << (updateStateSolvedArea model.cellRows cellToRowId))
            model.pendingSolvedRows
        |> setFoldlChain
            (andThen << (updateStateSolvedArea model.cellCols cellToColId))
            model.pendingSolvedCols


updateStateErrors : Model -> ( Model, Cmd Msg )
updateStateErrors model =
    ( { model | errors = getBoardErrors model }
    , Cmd.none
    )


updateStateScoutLocations : Model -> ( Model, Cmd Msg )
updateStateScoutLocations model =
    ( { model | pendingScoutLocations = Set.empty }
    , if model.gameIsLocal then
        Cmd.none

      else
        scoutLocations (Set.toList model.pendingScoutLocations)
    )


listFoldlChain : (a -> b -> b) -> List a -> b -> b
listFoldlChain fun list initial =
    List.foldl fun initial list


setFoldlChain : (a -> b -> b) -> Set a -> b -> b
setFoldlChain fun set initial =
    Set.foldl fun initial set


updateStateCellChange : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
updateStateCellChange updatedCell initialModel =
    ( initialModel
    , Cmd.none
    )
        |> listFoldlChain
            (\block ->
                andThen
                    (\model ->
                        let
                            blockId : Int
                            blockId =
                                cellToBlockId ( block.startRow, block.startCol )
                        in
                        if (not <|Set.member blockId model.solvedLocations)
                            && List.all (cellIsSolved model) (Engine.getAreaCells block)
                        then
                            if model.gameIsLocal then
                                ( { model
                                    | pendingSolvedBlocks =
                                        Set.insert
                                            ( block.startRow, block.startCol )
                                            model.pendingSolvedBlocks
                                    , solvedLocations =
                                        Set.insert blockId model.solvedLocations
                                  }
                                , Cmd.none
                                )
                                    |> andThen unlockNextBlock

                            else
                                ( model
                                , checkLocation (cellToBlockId ( block.startRow, block.startCol ))
                                )

                        else
                            ( model
                            , Cmd.none
                            )
                    )
            )
            (Dict.get updatedCell initialModel.cellBlocks
                |> Maybe.withDefault []
            )
        |> listFoldlChain
            (\row ->
                andThen
                    (\model ->
                        let
                            rowId : Int
                            rowId =
                                cellToRowId ( row.startRow, row.startCol )
                        in
                        if (not <| Set.member rowId model.solvedLocations)
                            && List.all (cellIsSolved model) (Engine.getAreaCells row)
                        then
                            if model.gameIsLocal then
                                ( { model
                                    | pendingSolvedRows =
                                        Set.insert
                                            ( row.startRow, row.startCol )
                                            model.pendingSolvedBlocks
                                    , solvedLocations =
                                        Set.insert rowId model.solvedLocations
                                  }
                                , Cmd.none
                                )
                                    |> andThen unlockNextBlock

                            else
                                ( model
                                , checkLocation (cellToRowId ( row.startRow, row.startCol ))
                                )

                        else
                            ( model
                            , Cmd.none
                            )
                    )
            )
            (Dict.get updatedCell initialModel.cellRows
                |> Maybe.withDefault []
            )
        |> listFoldlChain
            (\col ->
                andThen
                    (\model ->
                        let
                            colId : Int
                            colId =
                                cellToColId ( col.startRow, col.startCol )
                        in
                        if (not <| Set.member colId model.solvedLocations)
                            && List.all (cellIsSolved model) (Engine.getAreaCells col)
                        then
                            if model.gameIsLocal then
                                ( { model
                                    | pendingSolvedCols =
                                        Set.insert
                                            ( col.startRow, col.startCol )
                                            model.pendingSolvedBlocks
                                    , solvedLocations =
                                        Set.insert colId model.solvedLocations
                                  }
                                , Cmd.none
                                )
                                    |> andThen unlockNextBlock

                            else
                                ( model
                                , checkLocation (cellToColId ( col.startRow, col.startCol ))
                                )

                        else
                            ( model
                            , Cmd.none
                            )
                    )
            )
            (Dict.get updatedCell initialModel.cellCols
                |> Maybe.withDefault []
            )
        |> listFoldlChain
            (\boardArea ->
                andThen
                    (\model ->
                        let
                            boardId : Int
                            boardId =
                                cellToBoardId ( boardArea.startRow, boardArea.startCol )
                        in
                        if (not <| Set.member boardId model.solvedLocations)
                            && List.all (cellIsSolved model) (Engine.getAreaCells boardArea)
                        then
                            if model.gameIsLocal then
                                ( { model
                                    | solvedLocations =
                                        Set.insert boardId model.solvedLocations
                                  }
                                , Cmd.none
                                )
                                    |> andThen unlockNextBlock

                            else
                                ( model
                                , checkLocation (cellToBoardId ( boardArea.startRow, boardArea.startCol ))
                                )

                        else
                            ( model
                            , Cmd.none
                            )
                    )
            )
            (Dict.get updatedCell initialModel.cellBoards
                |> Maybe.withDefault []
            )
        |> andThen
            (\model ->
                if List.all (cellIsSolved model) (Dict.keys model.solution) then
                    ( model
                    , if model.gameIsLocal then
                        Cmd.none

                    else
                        goal ()
                    )

                else
                    ( model
                    , Cmd.none
                    )
            )


unlockNextBlock : Model -> ( Model, Cmd Msg )
unlockNextBlock model =
    case model.lockedBlocks of
        block :: remainingBlocks ->
            if Tuple.first block > 0 then
                unlockBlock block model

            else
                let
                    ( rand, newSeed ) =
                        Random.step (Random.int 0 99) model.seed

                    item : Maybe Item
                    item =
                        if rand < 10 then
                            Just SolveSelectedCell

                        else if rand < 30 then
                            Just SolveRandomCell

                        else
                            Nothing
                in
                ( { model
                    | lockedBlocks = remainingBlocks
                    , solveRandomCellUses =
                        if item == Just SolveRandomCell then
                            model.solveRandomCellUses + 1

                        else
                            model.solveRandomCellUses
                    , solveSelectedCellUses =
                        if item == Just SolveSelectedCell then
                            model.solveSelectedCellUses + 1

                        else
                            model.solveSelectedCellUses
                    , seed = newSeed
                    , messages =
                        if model.gameIsLocal then
                            addLocalMessage
                                (if item == Just SolveRandomCell then
                                    "Unlocked a Solve Random Cell."

                                 else if item == Just SolveSelectedCell then
                                    "Unlocked a Solve Selected Cell."

                                 else
                                     "Unlocked Nothing"
                                )
                                model.messages

                        else
                            model.messages
                  }
                , Cmd.none
                )

        [] ->
            ( model
            , Cmd.none
            )


unlockBlock : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
unlockBlock block model =
    let
        blockCells : Set ( Int, Int )
        blockCells =
            Dict.get block model.cellBlocks
                |> Maybe.withDefault []
                |> List.Extra.find
                    (\area ->
                        area.startRow == Tuple.first block
                            && area.startCol == Tuple.second block
                    )
                |> Maybe.map Engine.getAreaCells
                |> Maybe.withDefault []
                |> Set.fromList

        newVisibleCells : Set ( Int, Int )
        newVisibleCells =
            Set.union blockCells model.visibleCells

        unlockedAreas :
            Dict ( Int, Int ) (List Engine.Area)
            -> (( Int, Int ) -> Int)
            -> Set Int
        unlockedAreas areaDict toId =
            blockCells
                |> Set.toList
                |> List.filterMap
                    (\cell ->
                        Dict.get cell areaDict
                    )
                |> List.concat
                |> List.filterMap
                    (\area ->
                        if
                            Engine.getAreaCells area
                                |> Set.fromList
                                |> Set.Extra.isSubsetOf newVisibleCells
                        then
                            Just (toId ( area.startRow, area.startCol ))

                        else
                            Nothing
                    )
                |> Set.fromList

        unlockedBoards : Set Int
        unlockedBoards =
            unlockedAreas model.cellBoards cellToBoardId

        unlockedRows : Set Int
        unlockedRows =
            unlockedAreas model.cellRows cellToRowId

        unlockedCols : Set Int
        unlockedCols =
            unlockedAreas model.cellCols cellToColId
    in
    ( { model
        | lockedBlocks =
            List.filter ((/=) block) model.lockedBlocks
        , messages =
            if model.gameIsLocal then
                addLocalMessage
                    (String.concat
                        [ "Unlocked Block at "
                        , rowToLabel (Tuple.first block)
                        , String.fromInt (Tuple.second block)
                        ]
                    )
                    model.messages

            else
                model.messages
        , pendingScoutLocations =
            model.pendingScoutLocations
                |> Set.insert (cellToBlockId block)
                |> Set.union (unlockedRows)
                |> Set.union (unlockedCols)
                |> Set.union (unlockedBoards)
        , unlockedBlocks = Set.insert block model.unlockedBlocks
        , visibleCells = newVisibleCells
      }
    , triggerAnimation
        (Set.diff blockCells model.visibleCells
            |> Set.toList
            |> encodeTriggerAnimation "shatter"
        )
    )


getBoardErrors : Model -> Dict ( Int, Int ) (Set Int)
getBoardErrors model =
    List.foldl
        (\area errors ->
            let
                areaCells : List ( Int, Int )
                areaCells =
                    Engine.getAreaCells area
            in
            List.foldl
                (\cell acc ->
                    case Dict.get cell model.current of
                        Just (Given v) ->
                            let
                                numbersInArea : Set Int
                                numbersInArea =
                                    areaCells
                                        |> List.filter ((/=) cell)
                                        |> List.filter (\areaCell -> Set.member areaCell model.visibleCells)
                                        |> List.filterMap (\areaCell -> Dict.get areaCell model.current)
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
                                        |> List.filter (\areaCell -> Set.member areaCell model.visibleCells)
                                        |> List.filterMap (\areaCell -> Dict.get areaCell model.current)
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
                                        |> List.filter (\areaCell -> Set.member areaCell model.visibleCells)
                                        |> List.filterMap (\areaCell -> Dict.get areaCell model.current)
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
            [ model.puzzleAreas.rows
            , model.puzzleAreas.cols
            , model.puzzleAreas.blocks
            ]
        )


updateStateItem : Item -> Model -> ( Model, Cmd Msg )
updateStateItem item model =
    case item of
        ProgressiveBlock ->
            unlockNextBlock model

        Block block ->
            unlockBlock block model

        SolveSelectedCell ->
            ( { model
                | solveSelectedCellUses = model.solveSelectedCellUses + 1
              }
            , Cmd.none
            )

        SolveRandomCell ->
            ( { model
                | solveRandomCellUses = model.solveRandomCellUses + 1
              }
            , Cmd.none
            )


updateStateSolvedArea :
    Dict ( Int, Int ) (List Engine.Area)
    -> (( Int, Int ) -> Int)
    -> ( Int, Int )
    -> Model
    -> ( Model, Cmd Msg )
updateStateSolvedArea cellAreas toId ( row, col ) model =
    let
        cells : List ( Int, Int )
        cells =
            Dict.get ( row, col ) cellAreas
                |> Maybe.withDefault []
                |> List.Extra.find (\area -> area.startRow == row && area.startCol == col)
                |> Maybe.map Engine.getAreaCells
                |> Maybe.withDefault []
    in
    ( { model
        | current =
            List.foldl
                (\cell acc ->
                    Dict.insert
                        cell
                        (Given (Dict.get cell model.solution |> Maybe.withDefault 0))
                        acc
                )
                model.current
                cells
        , pendingCellChanges =
            Set.union (Set.fromList cells) model.pendingCellChanges
        , solvedLocations =
            Set.insert (toId ( row, col )) model.solvedLocations
      }
    , triggerAnimation (encodeTriggerAnimation "shine" cells)
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
                Just <| Multiple <| Set.remove number numbers

            else
                Just <| Multiple <| Set.insert number numbers

        Nothing ->
            Just <| Multiple (Set.singleton number)


cellIsSolved : Model -> ( Int, Int ) -> Bool
cellIsSolved model cell =
    case ( Dict.get cell model.current, Dict.get cell model.solution ) of
        ( Just (Given v), Just sol ) ->
            v == sol

        ( Just (Single v), Just sol ) ->
            v == sol

        _ ->
            False


cellIsGiven : Model -> ( Int, Int ) -> Bool
cellIsGiven model cell =
    Dict.get cell model.current
        |> Maybe.map isGiven
        |> Maybe.withDefault False


view : Model -> Html Msg
view model =
    case model.gameState of
        MainMenu ->
            viewMenu model

        Connecting ->
            Html.div
                [ HA.style "padding" "1em"
                ]
                [ Html.text "Connecting..." ]

        Generating ->
            Html.div
                [ HA.style "padding" "1em"
                ]
                [ Html.text (Tuple.first model.generationProgress)
                , Html.text " "
                , Html.text
                    (Tuple.second model.generationProgress
                        |> round
                        |> String.fromInt
                    )
                , Html.text "%"
                ]

        Playing ->
            Html.div
                [ HA.style "display" "flex"
                , HA.style "justify-content" "space-between"
                , HA.style "height" "100vh"
                ]
                [ viewBoard model
                , viewInfoPanel model
                ]


viewMenu : Model -> Html Msg
viewMenu model =
    Html.div
        [ HA.style "padding" "1em"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "2em"
        ]
        [ Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "start"
            , HA.style "gap" "0.5em"
            ]
            [ Html.h2
                [ HA.style "margin" "0" ]
                [ Html.text "Connect to Archipelago" ]
            , Html.form
                [ HA.style "display" "flex"
                , HA.style "gap" "0.5em"
                , HE.onSubmit ConnectPressed
                ]
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
                    []
                    [ Html.text "Connect"]
                ]
            ]
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "start"
            , HA.style "gap" "0.5em"
            ]
            [ Html.h2
                [ HA.style "margin" "0" ]
                [ Html.text "Play Local Game" ]
            , Html.div
                []
                [ Html.text "Block Size:"
                , Html.div
                    [ HA.style "display" "flex"
                    , HA.style "flex-direction" "row"
                    , HA.style "gap" "0.5em"
                    ]
                    [ viewNumberRadioButton 4 model.blockSize "block-size" BlockSizeChanged
                    , viewNumberRadioButton 6 model.blockSize "block-size" BlockSizeChanged
                    , viewNumberRadioButton 8 model.blockSize "block-size" BlockSizeChanged
                    , viewNumberRadioButton 9 model.blockSize "block-size" BlockSizeChanged
                    , viewNumberRadioButton 12 model.blockSize "block-size" BlockSizeChanged
                    , viewNumberRadioButton 16 model.blockSize "block-size" BlockSizeChanged
                    ]
                ]
            , Html.div
                []
                [ Html.text "Number of Boards:"
                , Html.div
                    [ HA.style "display" "flex"
                    , HA.style "flex-direction" "row"
                    , HA.style "gap" "0.5em"
                    ]
                    [ viewNumberRadioButton 1 model.numberOfBoards "number-of-boards" NumberOfBoardsChanged
                    , viewNumberRadioButton 3 model.numberOfBoards "number-of-boards" NumberOfBoardsChanged
                    , viewNumberRadioButton 5 model.numberOfBoards "number-of-boards" NumberOfBoardsChanged
                    , viewNumberRadioButton 8 model.numberOfBoards "number-of-boards" NumberOfBoardsChanged
                    , viewNumberRadioButton 13 model.numberOfBoards "number-of-boards" NumberOfBoardsChanged
                    , viewNumberRadioButton 18 model.numberOfBoards "number-of-boards" NumberOfBoardsChanged
                    , viewNumberRadioButton 25 model.numberOfBoards "number-of-boards" NumberOfBoardsChanged
                    , viewNumberRadioButton 32 model.numberOfBoards "number-of-boards" NumberOfBoardsChanged
                    , viewNumberRadioButton 41 model.numberOfBoards "number-of-boards" NumberOfBoardsChanged
                    , viewNumberRadioButton 50 model.numberOfBoards "number-of-boards" NumberOfBoardsChanged
                    , viewNumberRadioButton 98 model.numberOfBoards "number-of-boards" NumberOfBoardsChanged
                    ]
                ]
            , Html.div
                []
                [ Html.text "Difficulty:"
                , Html.div
                    [ HA.style "display" "flex"
                    , HA.style "flex-direction" "row"
                    , HA.style "gap" "0.5em"
                    ]
                    [ viewRadioButton 1 model.difficulty "difficulty" DifficultyChanged (\_ -> "Beginner")
                    , viewRadioButton 2 model.difficulty "difficulty" DifficultyChanged (\_ -> "Easy")
                    , viewRadioButton 3 model.difficulty "difficulty" DifficultyChanged (\_ -> "Medium")
                    , viewRadioButton 4 model.difficulty "difficulty" DifficultyChanged (\_ -> "Hard")
                    ]
                ]
            , Html.div
                [ HA.style "display" "flex"
                , HA.style "flex-direction" "row"
                , HA.style "gap" "0.25em"
                , HA.style "align-items" "baseline"
                , HA.min "0"
                , HA.max (String.fromInt Random.maxInt)
                ]
                [ Html.text "Seed:"
                , Html.input
                    [ HA.type_ "number"
                    , HA.value (String.fromInt model.seedInput)
                    , HE.onInput SeedInputChanged
                    ]
                    []
                ]
            , Html.button
                [ HE.onClick PlayLocalPressed ]
                [ Html.text "Play" ]
            ]
        ]


viewRadioButton : a -> a -> String -> (a -> Msg) -> (a -> String) -> Html Msg
viewRadioButton value selected name msg toLabel =
    Html.label
        [ HA.style "display" "flex"
        , HA.style "align-items" "baseline"
        , HA.style "gap" "0.25em"
        ]
        [ Html.input
            [ HA.type_ "radio"
            , HA.name name
            , HA.checked (value == selected)
            , HE.onCheck (\_ -> msg value)
            ]
            []
        , Html.text (toLabel value)
        ]


viewNumberRadioButton : Int -> Int -> String -> (Int -> Msg) -> Html Msg
viewNumberRadioButton value selected name msg =
    viewRadioButton value selected name msg String.fromInt


viewBoard : Model -> Html Msg
viewBoard model =
    let
        rows : Int
        rows =
            Dict.keys model.solution
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 0

        cols : Int
        cols =
            Dict.keys model.solution
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0
    in
    Html.node "panzoom-board-wrapper"
        [ HA.class "board"
        , HA.class <| "block-" ++ String.fromInt model.blockSize
        , HA.style "grid-template-rows" ("repeat(" ++ String.fromInt (rows + 2) ++ ", 1.5em)")
        , HA.style "grid-template-columns" ("repeat(" ++ String.fromInt (cols + 2) ++ ", 1.5em)")
        ]
        [ Html.div
            [ HA.class "board-corner" ]
            []
        , Html.div
            [ HA.class "board-columns-header" ]
            (List.map
                (\col ->
                    Html.div
                        [ HA.style "grid-row" "1"
                        , HA.style "grid-column" (String.fromInt col)
                        ]
                        [ Html.text (String.fromInt col) ]
                )
                (List.range 1 cols)
            )
        , Html.div
            [ HA.class "board-rows-header" ]
            (List.map
                (\row ->
                    Html.div
                        [ HA.style "grid-column" "1"
                        , HA.style "grid-row" (String.fromInt row)
                        ]
                        [ Html.text (rowToLabel row) ]
                )
                (List.range 1 rows)
            )
        , Html.div
            [ HA.class "board-cells" ]
            (List.map
                (\( row, col ) ->
                    viewCell model ( row, col )
                )
                (List.range 0 (rows + 1)
                    |> List.concatMap
                        (\row ->
                            List.range 0 (cols + 1)
                                |> List.map (Tuple.pair row)
                        )
                )
            )
        , viewZoomControls
        ]



viewCell : Model -> ( Int, Int ) -> Html Msg
viewCell model ( row, col ) =
    let
        cellIsAt : ( Int, Int ) -> Bool
        cellIsAt ( r, c ) =
            Dict.member ( r, c ) model.solution

        blocks : List Engine.Area
        blocks =
            Dict.get ( row, col ) model.cellBlocks
                |> Maybe.withDefault []

        isVisible : Bool
        isVisible =
            Set.member ( row, col ) model.visibleCells

        blockAbove : List Engine.Area
        blockAbove =
            Dict.get ( row - 1, col ) model.cellBlocks
                |> Maybe.withDefault []

        blockLeft : List Engine.Area
        blockLeft =
            Dict.get ( row, col - 1 ) model.cellBlocks
                |> Maybe.withDefault []

        errorsAtCell : Set Int
        errorsAtCell =
            Dict.get ( row, col ) model.errors
                |> Maybe.withDefault Set.empty

        cellIsMultiple : Bool
        cellIsMultiple =
            Dict.get ( row, col ) model.current
                |> Maybe.map isMultiple
                |> Maybe.withDefault False
    in
    if cellIsAt ( row, col ) then
        Html.button
            [ HA.id (cellHtmlId ( row, col ))
            , HA.class "cell"
            , HA.style "grid-row" (String.fromInt row)
            , HA.style "grid-column" (String.fromInt col)
            , HAE.attributeMaybe
                (\v ->
                    if isVisible then
                        HA.class <| "val-" ++ String.fromInt v

                    else
                        HAE.empty
                )
                (Dict.get ( row, col ) model.current
                    |> Maybe.andThen cellValueToInt
                )
            , HA.classList
                [ ( "selected", model.selectedCell == Just ( row, col ) )
                , ( "given", cellIsGiven model ( row, col ) && isVisible )
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
                , ( "hidden", not isVisible )
                , ( "error", (not <| Set.isEmpty errorsAtCell) && (not cellIsMultiple) && isVisible )
                ]
            , HE.onClick (CellSelected ( row, col ))
            ]
            (if isVisible then
                case Dict.get ( row, col ) model.current of
                    Just value ->
                        case value of
                            Given v ->
                                [ Html.text (numberToString model.blockSize v) ]

                            Single v ->
                                [ Html.text (numberToString model.blockSize v) ]

                            Multiple numbers ->
                                viewMultipleNumbers model.blockSize errorsAtCell numbers

                    Nothing ->
                        []

             else
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
                    Tuple.second (Engine.blockSizeToDimensions blockSize)

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


viewZoomControls : Html Msg
viewZoomControls =
    Html.div
        [ HA.style "position" "absolute"
        , HA.style "top" "0.5em"
        , HA.style "right" "0.5em"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "0.5em"
        , HA.style "z-index" "10"
        ]
        [ Html.button
            [ HE.onClick ZoomInPressed
            , HA.style "width" "1.5rem"
            , HA.style "height" "1.5rem"
            ]
            [ Html.text "+" ]
        , Html.button
            [ HE.onClick ZoomOutPressed
            , HA.style "width" "1.5rem"
            , HA.style "height" "1.5rem"
            ]
            [ Html.text "−" ]
        , Html.button
            [ HE.onClick ZoomResetPressed
            , HA.style "width" "1.5rem"
            , HA.style "height" "1.5rem"
            ]
            [ Html.text "=" ]
        ]


viewInfoPanel : Model -> Html Msg
viewInfoPanel model =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "1em"
        , HA.style "padding" "1em"
        ]
        [ case model.selectedCell of
            Just ( row, col ) ->
                Html.div
                    [ HA.style "display" "flex"
                    , HA.style "flex-direction" "column"
                    , HA.style "gap" "0.5em"
                    ]
                    (List.concat
                        [ [ viewCellInfo model ( row, col ) ]
                        , List.map
                            (viewBlockInfo model)
                            (Dict.get ( row, col ) model.cellBlocks
                                |> Maybe.withDefault []
                                |> List.sortBy .startRow
                            )

                        , List.map
                            (viewRowInfo model)
                            (Dict.get ( row, col ) model.cellRows
                                |> Maybe.withDefault []
                                |> List.sortBy .startCol
                            )

                        , List.map
                            (viewColInfo model)
                            (Dict.get ( row, col ) model.cellCols
                                |> Maybe.withDefault []
                                |> List.sortBy .startRow
                            )

                        , List.map
                            (viewBoardInfo model)
                            (Dict.get ( row, col ) model.cellBoards
                                |> Maybe.withDefault []
                                |> List.sortBy .startRow
                            )
                        ]
                    )

            Nothing ->
                Html.text ""
        , viewInfoHints model
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "row"
            , HA.style "gap" "0.25em"
            , HA.style "align-items" "baseline"
            ]
            [ Html.text "Input mode (Toggle: Space, Hold: Shift)"
            , Html.button
                [ HE.onClick ToggleCandidateModePressed ]
                [ if getCandidateMode model then
                    Html.text "Candidates"

                  else
                    Html.text "Set"
                ]
            ]
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "row"
            , HA.style "gap" "0.5em"
            ]
            [ Html.button
                [ HAE.attributeIf
                    (model.solveSelectedCellUses > 0)
                    (HE.onClick SolveSelectedCellPressed)
                , HA.disabled (model.solveSelectedCellUses <= 0)
                ]
                [ Html.text
                    (String.concat
                        [ "Solve Selected Cell ("
                        , String.fromInt model.solveSelectedCellUses
                        , " uses)"
                        ]
                    )
                ]
            , Html.button
                [ HAE.attributeIf
                    (model.solveRandomCellUses > 0)
                    (HE.onClick SolveRandomCellPressed)
                , HA.disabled (model.solveRandomCellUses <= 0)
                ]
                [ Html.text
                    (String.concat
                        [ "Solve Random Cell ("
                        , String.fromInt model.solveRandomCellUses
                        , " uses)"
                        ]
                    )
                ]
            ]
        , viewMessages model
        ]


viewCellInfo : Model -> ( Int, Int ) -> Html Msg
viewCellInfo model ( row, col ) =
    Html.div
        []
        [ viewCellLabel "Cell" row col
        ]


viewBlockInfo : Model -> Engine.Area -> Html Msg
viewBlockInfo model block =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ viewCellLabel "Block" block.startRow block.startCol

        , if model.gameIsLocal then
            Html.text ""

          else
            case Dict.get (cellToBlockId ( block.startRow, block.startCol )) model.scoutedItems of
                Just item ->
                    viewRewardLabel item

                Nothing ->
                    Html.div
                        []
                        [ Html.text "Reward: ???" ]

        -- TODO: Only if unlock method is fixed
        , if model.gameIsLocal
            || Set.member ( block.startRow, block.startCol ) model.unlockedBlocks
          then
            Html.text ""

          else
            case Dict.get (cellToBlockId ( block.startRow, block.startCol )) model.hints of
                Just item ->
                    Html.div
                        []
                        [ Html.text
                            (String.concat
                                [ "Unlock: "
                                , item.locationName
                                , " ("
                                , item.playerName
                                , ", "
                                , item.gameName
                                , ")"
                                ]
                            )
                        ]

                Nothing ->
                    Html.div
                        [ HA.style "display" "flex"
                        , HA.style "gap" "0.5em"
                        ]
                        [ Html.text "Unlock: ???"
                        , Html.button
                            [ HE.onClick
                                (HintItemPressed
                                    (String.concat
                                        [ "Block "
                                        , rowToLabel block.startRow
                                        , String.fromInt block.startCol
                                        ]
                                    )
                                )
                            , HA.disabled (model.hintPoints < model.hintCost)
                            ]
                            [ Html.text "Hint" ]
                        ]
        ]


viewRowInfo : Model -> Engine.Area -> Html Msg
viewRowInfo model row =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ viewCellLabel "Row" row.startRow row.startCol

        , if model.gameIsLocal then
            Html.text ""

          else
            case Dict.get (cellToRowId ( row.startRow, row.startCol )) model.scoutedItems of
                Just item ->
                    viewRewardLabel item

                Nothing ->
                    Html.div
                        []
                        [ Html.text "Reward: ???" ]
        ]


viewColInfo : Model -> Engine.Area -> Html Msg
viewColInfo model col =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ viewCellLabel "Column" col.startRow col.startCol

        , if model.gameIsLocal then
            Html.text ""

          else
            case Dict.get (cellToColId ( col.startRow, col.startCol )) model.scoutedItems of
                Just item ->
                    viewRewardLabel item

                Nothing ->
                    Html.div
                        []
                        [ Html.text "Reward: ???" ]
        ]


viewBoardInfo : Model -> Engine.Area -> Html Msg
viewBoardInfo model board =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ viewCellLabel "Board" board.startRow board.startCol

        , if model.gameIsLocal then
            Html.text ""

          else
            case Dict.get (cellToBoardId ( board.startRow, board.startCol )) model.scoutedItems of
                Just item ->
                    viewRewardLabel item

                Nothing ->
                    Html.div
                        []
                        [ Html.text "Reward: ???" ]
        ]


viewCellLabel : String -> Int -> Int -> Html Msg
viewCellLabel label row col =
    Html.div
        []
        [ Html.text
            (String.concat
                [ label
                , " "
                , rowToLabel row
                , String.fromInt col
                , " "
                ]
            )
        , Html.span
            [ HA.style "color" "gray"
            ]
            [ Html.text
                (String.concat
                    [ "(r"
                    , String.fromInt row
                    , "c"
                    , String.fromInt col
                    , ")"
                    ]
                )
            ]
        ]


viewRewardLabel : Hint -> Html Msg
viewRewardLabel hint =
    -- TODO: Strikethrough if obtained
    Html.div
        []
        [ Html.text
            (String.concat
                [ "Reward: "
                , hint.itemName
                , " ("
                , itemClassToString hint.itemClass
                , ", "
                , hint.playerName
                , ", "
                , hint.gameName
                , ")"
                ]
            )
        ]


viewInfoHints : Model -> Html Msg
viewInfoHints model =
    if model.gameIsLocal then
        Html.text ""

    else
        Html.div
            []
            [ Html.text
                (String.concat
                    [ "Hints available: "
                    , String.fromInt <| model.hintPoints // model.hintCost
                    , " ("
                    , String.fromInt model.hintPoints
                    , " points, cost "
                    , String.fromInt model.hintCost
                    , ")"
                    ]
                )
            ]


viewMessages : Model -> Html Msg
viewMessages model =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "0.5em"
        , HA.style "flex-grow" "1"
        , HA.style "justify-content" "flex-end"
        ]
        [ Html.div
            [ HA.style "max-width" "400px"
            , HA.style "max-height" "400px"
            , HA.style "overflow-y" "auto"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column-reverse"
            , HA.style "gap" "0.5em"
            ]
            (List.map viewMessage model.messages)
        , Html.Extra.viewIf
            (not model.gameIsLocal)
            (Html.form
                [ HA.style "display" "flex"
                , HA.style "flex-direction" "row"
                , HA.style "gap" "0.5em"
                , HE.onSubmit SendMessagePressed
                ]
                [ Html.input
                    [ HA.type_ "text"
                    , HA.placeholder "Enter message..."
                    , HA.value model.messageInput
                    , HA.style "flex-grow" "1"
                    , HE.onInput MessageInputChanged
                    ]
                    []
                , Html.button
                    []
                    [ Html.text "Send" ]
                ]
            )
        ]


viewMessage : Message -> Html Msg
viewMessage message =
    Html.div
        [ HA.style "white-space-collapse" "preserve" ]
        (List.map
            (\node ->
                case node of
                    ItemMessageNode item ->
                        -- TODO: Color the item based on its class
                        Html.span
                            []
                            [ Html.text item ]

                    LocationMessageNode location ->
                        Html.span
                            []
                            [ Html.text location ]

                    ColorMessageNode color text ->
                        Html.text text

                    TextualMessageNode text ->
                        Html.text text

                    PlayerMessageNode player ->
                        Html.span
                            []
                            [ Html.text player ]
            )
            message.nodes
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


cellHtmlId : ( Int, Int ) -> String
cellHtmlId ( row, col ) =
    "cell-" ++ String.fromInt row ++ "-" ++ String.fromInt col


cellToBlockId : ( Int, Int ) -> Int
cellToBlockId ( row, col ) =
    1000000 + row * 1000 + col


cellToRowId : ( Int, Int ) -> Int
cellToRowId ( row, col ) =
    2000000 + row * 1000 + col


cellToColId : ( Int, Int ) -> Int
cellToColId ( row, col ) =
    3000000 + row * 1000 + col


cellToBoardId : ( Int, Int ) -> Int
cellToBoardId ( row, col ) =
    4000000 + row * 1000 + col


cellFromId : Int -> ( Int, Int )
cellFromId id =
    ( (modBy 1000000 id) // 1000, modBy 1000 id )


addLocalMessage : String -> List Message -> List Message
addLocalMessage text messages =
    let
        newMessage : Message
        newMessage =
            { nodes = [ TextualMessageNode text ]
            , extra = LocalMessage
            }
    in
    (newMessage :: messages)
        |> List.take maxMessages


maxMessages : Int
maxMessages =
    1000
