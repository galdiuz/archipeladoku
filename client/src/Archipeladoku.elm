port module Archipeladoku exposing (..)

import Array exposing (Array)
import Bitwise
import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Extra
import Html.Attributes as HA
import Html.Attributes.Extra as HAE
import Html.Events as HE
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra
import Json.Decode.Field as Field
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
port sendPlayingStatus : () -> Cmd msg
port setLocalStorage : (String, String) -> Cmd msg
port triggerAnimation : Encode.Value -> Cmd msg
port zoom : Encode.Value -> Cmd msg
port zoomReset : () -> Cmd msg

port receiveCheckedLocations : (List Int -> msg) -> Sub msg
port receiveConnectionStatus : (Bool -> msg) -> Sub msg
port receiveGeneratedBoard : (Decode.Value -> msg) -> Sub msg
port receiveGenerationProgress : (Decode.Value -> msg) -> Sub msg
port receiveHintCost : (Int -> msg) -> Sub msg
port receiveHintPoints : (Int -> msg) -> Sub msg
port receiveHints : (Decode.Value -> msg) -> Sub msg
port receiveItems : (List Int -> msg) -> Sub msg
port receiveMessage : (Decode.Value -> msg) -> Sub msg
port receiveScoutedItems : (Decode.Value -> msg) -> Sub msg
port receiveSlotData : (Decode.Value -> msg) -> Sub msg


type alias Model =
    { animationsEnabled : Bool
    , autoFillCandidatesOnUnlock : Bool
    , autoRemoveInvalidCandidates : Bool
    , blockSize : Int
    , boardsPerCluster : Int
    , candidateMode : Bool
    , cellBlocks : Dict ( Int, Int ) (List Area)
    , cellBoards : Dict ( Int, Int ) (List Area)
    , cellCols : Dict ( Int, Int ) (List Area)
    , cellRows : Dict ( Int, Int ) (List Area)
    , colorScheme : String
    , current : Dict ( Int, Int ) CellValue
    , difficulty : Int
    , errors : Dict ( Int, Int ) CellError
    , generationProgress : ( String, Float )
    , gameIsLocal : Bool
    , gameState : GameState
    , givens : Set ( Int, Int )
    , heldKeys : Set String
    , highlightedCells : Set ( Int, Int )
    , highlightMode : HighlightMode
    , hints : Dict Int Hint
    , hintCost : Int
    , hintPoints : Int
    , host : String
    , locationScouting : LocationScouting
    , lockedBlocks : List ( Int, Int )
    , messageInput : String
    , messages : List Message
    , numberOfBoards : Int
    , password : String
    , pendingCellChanges : Set ( Int, Int )
    , pendingCheckLocations : Set Int
    , pendingItems : List Item
    , pendingScoutLocations : Set Int
    , pendingSolvedBlocks : Set ( Int, Int )
    , pendingSolvedCols : Set ( Int, Int )
    , pendingSolvedRows : Set ( Int, Int )
    , player : String
    , progression : Progression
    , puzzleAreas : PuzzleAreas
    , removeRandomCandidateUses : Int
    , scoutedItems : Dict Int Hint
    , seed : Random.Seed
    , seedInput : Int
    , selectedCell : ( Int, Int )
    , shiftDebounce : Int
    , solution : Dict ( Int, Int ) Int
    , solveRandomCellUses : Int
    , solveSelectedCellUses : Int -- TODO: Need to persist these counts
    , solvedLocations : Set Int
    , undoStack : List (Dict ( Int, Int ) CellValue)
    , unlockedBlocks : Set ( Int, Int )
    , unlockMap : Dict Int Item
    , visibleCells : Set ( Int, Int )
    }


type Msg
    = AddDebugItemsPressed
    | AutoFillCandidatesOnUnlockChanged Bool
    | AutoRemoveInvalidCandidatesChanged Bool
    | BlockSizeChanged Int
    | BoardsPerClusterChanged Int
    | CandidateModeChanged Bool
    | CellSelected ( Int, Int )
    | ClearBoardPressed
    | ColorSchemeChanged String
    | ConnectPressed
    | DeletePressed
    | DifficultyChanged Int
    | EnableAnimationsChanged Bool
    | FillBoardCandidatesPressed
    | FillCellCandidatesPressed
    | GotCheckedLocations (List Int)
    | GotConnectionStatus Bool
    | GotGeneratedBoard Decode.Value
    | GotGenerationProgress Decode.Value
    | GotHintCost Int
    | GotHintPoints Int
    | GotHints Decode.Value
    | GotItems (List Int)
    | GotMessage Decode.Value
    | GotSlotData Decode.Value
    | HighlightModeChanged HighlightMode
    | HintItemPressed String
    | HostInputChanged String
    | MessageInputChanged String
    | MoveSelectionPressed ( Int, Int )
    | NoOp
    | NumberOfBoardsChanged Int
    | NumberPressed Int
    | PasswordInputChanged String
    | PlayLocalPressed
    | PlayerInputChanged String
    | ProgressionChanged Progression
    | RemoveInvalidCandidatesPressed
    | RemoveRandomCandidatePressed
    | ScoutLocationPressed Int
    | SeedInputChanged String
    | SelectSingleCandidateCellPressed
    | SendMessagePressed
    | ShiftDebouncePassed Int
    | ShiftHeld
    | ShiftReleased
    | SolveRandomCellPressed
    | SolveSelectedCellPressed
    | SolveSingleCandidatesPressed
    | ToggleCandidateModePressed
    | ToggleHighlightModePressed
    | UndoPressed
    | UnlockSelectedBlockPressed
    | ZoomInPressed
    | ZoomOutPressed
    | ZoomResetPressed


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
        flags : Flags
        flags =
            Decode.decodeValue flagsDecoder flagsValue
                |> Result.withDefault defaultFlags
    in
    ( { animationsEnabled = True
      , autoFillCandidatesOnUnlock = False
      , autoRemoveInvalidCandidates = False
      , blockSize = 9
      , boardsPerCluster = 5
      , candidateMode = False
      , cellBlocks = Dict.empty
      , cellBoards = Dict.empty
      , cellCols = Dict.empty
      , cellRows = Dict.empty
      , colorScheme = "light dark"
      , current = Dict.empty
      , difficulty = 2
      , errors = Dict.empty
      , generationProgress = ( "", 0 )
      , gameIsLocal = False
      , gameState = MainMenu
      , givens = Set.empty
      , heldKeys = Set.empty
      , highlightedCells = Set.empty
      , highlightMode = HighlightNone
      , hints = Dict.empty
      , hintCost = 0
      , hintPoints = 0
      , host = "localhost:8123"
      , locationScouting = ScoutingManual
      , lockedBlocks = []
      , messageInput = ""
      , messages = []
      , numberOfBoards = 5
      , password = ""
      , pendingCellChanges = Set.empty
      , pendingCheckLocations = Set.empty
      , pendingItems = []
      , pendingScoutLocations = Set.empty
      , pendingSolvedBlocks = Set.empty
      , pendingSolvedCols = Set.empty
      , pendingSolvedRows = Set.empty
      , player = "Player1"
      , progression = Shuffled
      , puzzleAreas =
            { blocks = []
            , boards = []
            , rows = []
            , cols = []
            }
      , removeRandomCandidateUses = 0
      , scoutedItems = Dict.empty
      , seed = Random.initialSeed (flags.seed + 1)
      , seedInput = flags.seed
      , selectedCell = ( 1, 1 )
      , shiftDebounce = 0
      , solution = Dict.empty
      , solveRandomCellUses = 0
      , solveSelectedCellUses = 0
      , solvedLocations = Set.empty
      , undoStack = []
      , unlockedBlocks = Set.empty
      , unlockMap = Dict.empty
      , visibleCells = Set.empty
      }
    , Cmd.none
    )
        |> andThen (updateFromLocalStorage flags.localStorage)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveCheckedLocations GotCheckedLocations
        , receiveConnectionStatus GotConnectionStatus
        , receiveGeneratedBoard GotGeneratedBoard
        , receiveGenerationProgress GotGenerationProgress
        , receiveHintCost GotHintCost
        , receiveHintPoints GotHintPoints
        , receiveHints GotHints
        , receiveItems GotItems
        , receiveMessage GotMessage
        , receiveSlotData GotSlotData
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddDebugItemsPressed ->
            ( { model
                | removeRandomCandidateUses = model.removeRandomCandidateUses + 1000
                , solveSelectedCellUses = model.solveSelectedCellUses + 1000
                , solveRandomCellUses = model.solveRandomCellUses + 1000
              }
            , Cmd.none
            )

        AutoFillCandidatesOnUnlockChanged value ->
            ( { model | autoFillCandidatesOnUnlock = value }
            , Cmd.none
            )

        AutoRemoveInvalidCandidatesChanged value ->
            ( { model | autoRemoveInvalidCandidates = value }
            , Cmd.none
            )
                |> andThen updateState

        BlockSizeChanged size ->
            ( { model
                | blockSize = size
                , numberOfBoards = min model.numberOfBoards (maxNumberOfBoards size)
              }
            , Cmd.none
            )

        BoardsPerClusterChanged value ->
            ( { model | boardsPerCluster = value }
            , Cmd.none
            )

        CandidateModeChanged value ->
            ( { model | candidateMode = value }
            , Cmd.none
            )

        CellSelected ( row, col ) ->
            ( { model | selectedCell = ( row, col ) }
                |> updateHighlightedCells
            , Cmd.none
            )

        ClearBoardPressed ->
            let
                boardCells : Set ( Int, Int )
                boardCells =
                    Dict.get model.selectedCell model.cellBoards
                        |> Maybe.withDefault []
                        |> List.concatMap getAreaCells
                        |> Set.fromList
            in
            ( { model
                | current =
                    Dict.filter
                        (\cell _ ->
                            not (Set.member cell boardCells)
                        )
                        model.current
                , undoStack = pushUndoStack model
              }
            , Cmd.none
            )
                |> andThen updateState

        ColorSchemeChanged scheme ->
            ( { model | colorScheme = scheme }
            , setLocalStorage ( "apdk-color-scheme", scheme )
            )

        ConnectPressed ->
            ( { model | gameState = Connecting }
            , connect
                (Encode.object
                    [ ( "host", Encode.string model.host )
                    , ( "player", Encode.string model.player )
                    , ( "password"
                      , if model.password == "" then
                            Encode.null

                        else
                            Encode.string model.password
                      )
                    ]
                )
            )

        DeletePressed ->
            if Set.member model.selectedCell model.visibleCells
                && not (cellIsGiven model model.selectedCell)
            then
                ( { model
                    | current = Dict.remove model.selectedCell model.current
                    , pendingCellChanges = Set.insert model.selectedCell model.pendingCellChanges
                    , undoStack = pushUndoStack model
                  }
                , Cmd.none
                )
                    |> andThen updateState

            else
                ( model
                , Cmd.none
                )

        DifficultyChanged value ->
            ( { model | difficulty = value }
            , Cmd.none
            )

        EnableAnimationsChanged value ->
            ( { model | animationsEnabled = value }
            , setLocalStorage ( "apdk-animations-enabled", if value then "1" else "0" )
            )

        FillBoardCandidatesPressed ->
            let
                boardCells : Set ( Int, Int )
                boardCells =
                    Dict.get model.selectedCell model.cellBoards
                        |> Maybe.withDefault []
                        |> List.concatMap getAreaCells
                        |> Set.fromList

                cellIsValidTarget : ( Int, Int ) -> Bool
                cellIsValidTarget cell =
                    case Dict.get cell model.current of
                        Just (Given _) ->
                            False

                        Just (Single _) ->
                            False

                        Just (Multiple _) ->
                            Set.member cell model.visibleCells
                                && Set.member cell boardCells

                        Nothing ->
                            Set.member cell model.visibleCells
                                && Set.member cell boardCells
            in
            ( { model
                | current =
                    Set.foldl
                        (\cell current ->
                            if cellIsValidTarget cell then
                                Dict.insert
                                    cell
                                    (Multiple (getValidCellCandidates model cell))
                                    current

                            else
                                current
                        )
                        model.current
                        model.visibleCells
                , undoStack = pushUndoStack model
              }
            , Cmd.none
            )

        FillCellCandidatesPressed ->
            if Set.member model.selectedCell model.visibleCells
                && not (cellIsGiven model model.selectedCell)
            then
                ( { model
                    | current =
                        Dict.insert
                            model.selectedCell
                            (Multiple (getValidCellCandidates model model.selectedCell))
                            model.current
                    , undoStack = pushUndoStack model
                  }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

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
                |> andThen updateState

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
                    if not status && model.gameState == Playing then
                        addLocalMessage "Disconnected from server, reload page." model.messages

                    else
                        model.messages
              }
            , Cmd.none
            )

        GotGeneratedBoard value ->
            case Decode.decodeValue generatedBoardDecoder value of
                Ok board ->
                    List.foldl
                        (\block -> andThen (unlockBlock block))
                        ( { model
                            | cellBlocks = buildCellAreasMap board.puzzleAreas.blocks
                            , cellBoards = buildCellAreasMap board.puzzleAreas.boards
                            , cellCols = buildCellAreasMap board.puzzleAreas.cols
                            , cellRows = buildCellAreasMap board.puzzleAreas.rows
                            , blockSize = board.blockSize
                            , current = Dict.empty
                            , errors = Dict.empty
                            , gameState = Playing
                            , givens = Set.fromList (Dict.keys board.givens)
                            , lockedBlocks = board.blockUnlockOrder
                            , puzzleAreas = board.puzzleAreas
                            , solution = board.solution
                            , unlockedBlocks = Set.empty
                            , unlockMap = board.unlockMap
                            , pendingItems = model.pendingItems
                          }
                        , if model.gameIsLocal then
                            Cmd.none

                          else
                            sendPlayingStatus ()
                        )
                        (List.filterMap
                            (\block ->
                                if block.endRow <= board.blockSize
                                    && block.endCol <= board.blockSize
                                then
                                    Just ( block.startRow, block.startCol )

                                else
                                    Nothing
                            )
                            board.puzzleAreas.blocks
                        )
                        |> andThen updateState

                Err err ->
                    ( model, Cmd.none )

        GotGenerationProgress value ->
            case Decode.decodeValue generationProgressDecoder value of
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
            case Decode.decodeValue (Decode.list hintDecoder) value of
                Ok hints ->
                    ( { model
                        | hints =
                            List.foldl
                                (\item acc ->
                                    if item.receiverName == model.player then
                                        Dict.insert item.itemId item acc

                                    else
                                        acc
                                )
                                model.hints
                                hints
                        , scoutedItems =
                            List.foldl
                                (\item acc ->
                                    if item.senderName == model.player then
                                        Dict.insert item.locationId item acc

                                    else
                                        acc
                                )
                                model.scoutedItems
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
                            |> List.map itemFromId
                        )
              }
            , Cmd.none
            )
                |> andThen updateState

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

        GotSlotData value ->
            case Decode.decodeValue slotDataDecoder value of
                Ok slotData ->
                    ( { model
                        | locationScouting = slotData.locationScouting
                        , progression = slotData.progression
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                    , Cmd.none
                    )

        HighlightModeChanged mode ->
            ( { model | highlightMode = mode }
                |> updateHighlightedCells
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

        MessageInputChanged value ->
            ( { model | messageInput = value }
            , Cmd.none
            )

        MoveSelectionPressed move ->
            ( model
            , Cmd.none
            )
                |> andThen (moveSelection move)

        NoOp ->
            ( model
            , Cmd.none
            )

        NumberOfBoardsChanged value ->
            ( { model | numberOfBoards = value }
            , Cmd.none
            )

        NumberPressed number ->
            if cellIsVisible model model.selectedCell && not (cellIsGiven model model.selectedCell) then
                let
                    newCurrent : Dict ( Int, Int ) CellValue
                    newCurrent =
                        if number < 1 || number > model.blockSize then
                            model.current

                        else
                            if getCandidateMode model then
                                Dict.update
                                    model.selectedCell
                                    (toggleNumber number)
                                    model.current

                            else
                                Dict.update
                                    model.selectedCell
                                    (\cellValue ->
                                        case cellValue of
                                            Just (Single curretNum) ->
                                                if curretNum == number then
                                                    Nothing

                                                else
                                                    Just (Single number)

                                            _ ->
                                                Just (Single number)
                                    )
                                    model.current
                in
                ( { model
                    | current = newCurrent
                    , pendingCellChanges = Set.insert model.selectedCell model.pendingCellChanges
                    , undoStack = pushUndoStack model
                  }
                , Cmd.none
                )
                    |> andThen updateState

            else
                ( model
                , Cmd.none
                )

        PasswordInputChanged value ->
            ( { model | password = value }
            , Cmd.none
            )

        PlayLocalPressed ->
            ( { model
                | gameIsLocal = True
                , gameState = Generating
              }
            , generateBoard
                (encodeGenerateArgs
                    { blockSize = model.blockSize
                    , boardsPerCluster = model.boardsPerCluster
                    , difficulty = model.difficulty
                    , numberOfBoards = model.numberOfBoards
                    , progression = model.progression
                    , seed = model.seedInput
                    }
                )
            )

        PlayerInputChanged value ->
            ( { model | player = value }
            , Cmd.none
            )

        ProgressionChanged value ->
            ( { model | progression = value }
            , Cmd.none
            )

        RemoveInvalidCandidatesPressed ->
            ( removeInvalidCandidates model
            , Cmd.none
            )
                |> andThen updateState

        RemoveRandomCandidatePressed ->
            let
                boardCells : Set ( Int, Int )
                boardCells =
                    Dict.get model.selectedCell model.cellBoards
                        |> Maybe.withDefault []
                        |> List.concatMap getAreaCells
                        |> Set.fromList

                targets : List ( ( Int, Int ), Int )
                targets =
                    Dict.foldl
                        (\cell cellValue acc ->
                            case cellValue of
                                Multiple candidates ->
                                    Set.foldl
                                        (\candidate ->
                                            if (Dict.get cell model.solution /= Just candidate)
                                                && Set.member cell boardCells
                                                && cellIsVisible model cell
                                            then
                                                (::) ( cell, candidate )

                                            else
                                                identity
                                        )
                                        acc
                                        candidates

                                _ ->
                                    acc
                        )
                        []
                        model.current

                ( maybeTarget, newSeed ) =
                    case targets of
                        [] ->
                            ( Nothing, model.seed )

                        firstTarget :: restTargets ->
                            Random.step
                                (Random.uniform firstTarget restTargets)
                                model.seed
                                |> Tuple.mapFirst Just
            in
            case maybeTarget of
                Just ( cell, number ) ->
                    ( { model
                        | current = Dict.update cell (toggleNumber number) model.current
                        , pendingCellChanges = Set.insert cell model.pendingCellChanges
                        , removeRandomCandidateUses = model.removeRandomCandidateUses - 1
                        , seed = newSeed
                        , messages =
                            addLocalMessage
                                (String.concat
                                    [ "Used Remove Random Candidate item to remove candidate "
                                    , String.fromInt number
                                    , " from Cell "
                                    , rowToLabel (Tuple.first cell)
                                    , String.fromInt (Tuple.second cell)
                                    ]
                                )
                                model.messages
                      }
                    , Cmd.none
                    )
                        |> andThen updateState

                Nothing ->
                    ( { model
                        | messages =
                            addLocalMessage
                                "Remove Random Candidate item could not be used because there are no valid candidates to remove in the selected board."
                                model.messages
                      }
                    , Cmd.none
                    )

        ScoutLocationPressed id ->
            if model.gameIsLocal then
                case Dict.get id model.unlockMap of
                    Just item ->
                        ( { model
                            | scoutedItems = Dict.insert id (createHint id item) model.scoutedItems
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model
                        , Cmd.none
                        )

            else
                ( model
                , scoutLocations [ id ]
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

        SelectSingleCandidateCellPressed ->
            let
                targetCell : Maybe ( Int, Int )
                targetCell =
                    Dict.foldl
                        (\cell cellValue acc ->
                            case ( acc, cellValue ) of
                                ( Nothing, Multiple candidates ) ->
                                    if Set.size candidates == 1
                                        && Set.member cell model.visibleCells
                                        && not (Set.member cell model.givens)
                                    then
                                        Just cell

                                    else
                                        Nothing

                                _ ->
                                    acc
                        )
                        Nothing
                        model.current
            in
            case targetCell of
                Just cell ->
                    ( { model | selectedCell = cell }
                        |> updateHighlightedCells
                    , moveCellIntoView (cellHtmlId cell)
                    )

                Nothing ->
                    ( model
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

        ShiftHeld ->
            ( { model
                | heldKeys = Set.insert "Shift" model.heldKeys
                , shiftDebounce = model.shiftDebounce + 1
              }
            , Cmd.none
            )

        ShiftReleased ->
            ( model
            , Process.sleep 100
                |> Task.perform (\_ -> ShiftDebouncePassed model.shiftDebounce)
            )

        SolveRandomCellPressed ->
            let
                boardCells : Set ( Int, Int )
                boardCells =
                    Dict.get model.selectedCell model.cellBoards
                        |> Maybe.withDefault []
                        |> List.concatMap getAreaCells
                        |> Set.fromList

                cellCandidates : List ( Int, Int )
                cellCandidates =
                    List.filter
                        (\cell ->
                            Set.member cell model.visibleCells
                                && Set.member cell boardCells
                                && not (Set.member cell model.givens)
                        )
                        (Dict.keys model.solution)

                ( maybeTargetCell, newSeed ) =
                    case cellCandidates of
                        [] ->
                            ( Nothing, model.seed )

                        firstCandidate :: restCandidates ->
                            Random.step
                                (Random.uniform firstCandidate restCandidates)
                                model.seed
                                |> Tuple.mapFirst Just
            in
            case maybeTargetCell of
                Just targetCell ->
                    ( { model
                        | current = Dict.remove targetCell model.current
                        , givens = Set.insert targetCell model.givens
                        , pendingCellChanges = Set.insert targetCell model.pendingCellChanges
                        , seed = newSeed
                        , solveRandomCellUses = model.solveRandomCellUses - 1
                        , messages =
                            addLocalMessage
                                (String.concat
                                    [ "Used Solve Random Cell item at Cell "
                                    , rowToLabel (Tuple.first targetCell)
                                    , String.fromInt (Tuple.second targetCell)
                                    ]
                                )
                                model.messages
                      }
                    , Cmd.none
                    )
                    |> andThen updateState

                Nothing ->
                    ( { model
                        | messages =
                            addLocalMessage
                                "Solve Random Cell item could not be used because there are no unsolved visible cells in the selected board."
                                model.messages
                      }
                    , Cmd.none
                    )

        SolveSelectedCellPressed ->
            if not (Set.member model.selectedCell model.visibleCells) then
                ( { model
                    | messages =
                        addLocalMessage
                            (String.concat
                                [ "Solve Selected Cell item at Cell "
                                , rowToLabel (Tuple.first model.selectedCell)
                                , String.fromInt (Tuple.second model.selectedCell)
                                , " could not be used because the cell isn't unlocked."
                                ]
                            )
                            model.messages
                  }
                , Cmd.none
                )

            else if Set.member model.selectedCell model.givens then
                ( { model
                    | messages =
                        addLocalMessage
                            (String.concat
                                [ "Solve Selected Cell item at Cell "
                                , rowToLabel (Tuple.first model.selectedCell)
                                , String.fromInt (Tuple.second model.selectedCell)
                                , " could not be used because the cell is already solved."
                                ]
                            )
                            model.messages
                  }
                , Cmd.none
                )

            else
                ( { model
                    | current = Dict.remove model.selectedCell model.current
                    , givens = Set.insert model.selectedCell model.givens
                    , pendingCellChanges = Set.insert model.selectedCell model.pendingCellChanges
                    , solveSelectedCellUses = model.solveSelectedCellUses - 1
                    , messages =
                        addLocalMessage
                            (String.concat
                                [ "Used Solve Selected Cell item at Cell "
                                , rowToLabel (Tuple.first model.selectedCell)
                                , String.fromInt (Tuple.second model.selectedCell)
                                ]
                            )
                            model.messages
                  }
                , Cmd.none
                )
                    |> andThen updateState

        SolveSingleCandidatesPressed ->
            let
                boardCells : Set ( Int, Int )
                boardCells =
                    Dict.get model.selectedCell model.cellBoards
                        |> Maybe.withDefault []
                        |> List.concatMap getAreaCells
                        |> Set.fromList

                singleCandidates : Dict ( Int, Int ) Int
                singleCandidates =
                    Dict.foldl
                        (\cell cellValue acc ->
                            case cellValue of
                                Multiple values ->
                                    if Set.size values == 1
                                        && Set.member cell model.visibleCells
                                        && Set.member cell boardCells
                                    then
                                        Dict.insert
                                            cell
                                            (Set.toList values |> List.head |> Maybe.withDefault 0)
                                            acc

                                    else
                                        acc

                                _ ->
                                    acc
                        )
                        Dict.empty
                        model.current
            in
            ( { model
                | current =
                    Dict.foldl
                        (\cell number current ->
                            Dict.insert
                                cell
                                (Single number)
                                current
                        )
                        model.current
                        singleCandidates
                , pendingCellChanges =
                    Set.union
                        model.pendingCellChanges
                        (Dict.keys singleCandidates
                            |> Set.fromList
                        )
                , undoStack = pushUndoStack model
              }
            , Cmd.none
            )
                |> andThen updateState

        ToggleCandidateModePressed ->
            ( { model | candidateMode = not model.candidateMode }
            , Cmd.none
            )

        ToggleHighlightModePressed ->
            ( { model
                | highlightMode =
                    if Set.member "Shift" model.heldKeys then
                        case model.highlightMode of
                            HighlightNone ->
                                HighlightNumber

                            HighlightNumber ->
                                HighlightArea

                            HighlightArea ->
                                HighlightBoard

                            HighlightBoard ->
                                HighlightNone

                    else
                        case model.highlightMode of
                            HighlightNone ->
                                HighlightBoard

                            HighlightBoard ->
                                HighlightArea

                            HighlightArea ->
                                HighlightNumber

                            HighlightNumber ->
                                HighlightNone
              }
                |> updateHighlightedCells
            , Cmd.none
            )

        UndoPressed ->
            case model.undoStack of
                [] ->
                    ( model, Cmd.none )

                prevCurrent :: restUndoStack ->
                    ( { model
                        | current =
                            Dict.filter
                                (\cell _ ->
                                    not (Set.member cell model.givens)
                                )
                                prevCurrent
                        , undoStack = restUndoStack
                      }
                    , Cmd.none
                    )
                        |> andThen updateState

        UnlockSelectedBlockPressed ->
            List.foldl
                (andThen << \block -> (unlockBlock ( block.startRow, block.startCol ))
                )
                ( model
                , Cmd.none
                )
                (Dict.get model.selectedCell model.cellBlocks
                    |> Maybe.withDefault []
                )

        ZoomInPressed ->
            ( model
            , zoom
                (Encode.object
                    [ ( "id", Encode.string <| cellHtmlId model.selectedCell )
                    , ( "scaleMult", Encode.float 1.5 )
                    ]
                )
            )

        ZoomOutPressed ->
            ( model
            , zoom
                (Encode.object
                    [ ( "id", Encode.string <| cellHtmlId model.selectedCell )
                    , ( "scaleMult", Encode.float 0.66 )
                    ]
                )
            )

        ZoomResetPressed ->
            ( model
            , zoomReset ()
            )


---
-- Types
---


type alias Flags =
    { localStorage : Dict String String
    , seed : Int
    }


defaultFlags : Flags
defaultFlags =
    { localStorage = Dict.empty
    , seed = 1
    }


type alias GeneratedBoard =
    { blockSize : Int
    , blockUnlockOrder : List ( Int, Int )
    , givens : Dict ( Int, Int ) Int
    , puzzleAreas : PuzzleAreas
    , solution : Dict ( Int, Int ) Int
    , unlockMap : Dict Int Item
    }


type alias Area =
    { startRow : Int
    , startCol : Int
    , endRow : Int
    , endCol : Int
    }


type alias PuzzleAreas =
    { blocks : List Area
    , boards : List Area
    , rows : List Area
    , cols : List Area
    }


type alias GenerateArgs =
    { blockSize : Int
    , boardsPerCluster : Int
    , difficulty : Int
    , numberOfBoards : Int
    , progression : Progression
    , seed : Int
    }



type Progression
    = Fixed
    | Shuffled


type GameState
    = MainMenu
    | Connecting
    | Generating
    | Playing


type CellValue
    = Given Int
    | Single Int
    | Multiple (Set Int)


type CellError
    = CandidateErrors (Set Int)
    | NumberError
    | ErrorContext


type HighlightMode
    = HighlightNone
    | HighlightBoard
    | HighlightArea
    | HighlightNumber


type Item
    = ProgressiveBlock
    | Block ( Int, Int )
    | SolveSelectedCell
    | SolveRandomCell
    | RemoveRandomCandidate
    | NothingItem


type alias Hint =
    { locationId : Int
    , locationName : String
    , itemId : Int
    , itemName : String
    , itemClass : ItemClass
    , senderAlias : String
    , senderName : String
    , receiverAlias : String
    , receiverName : String
    , gameName : String
    }


type ItemClass
    = Progression
    | Useful
    | Filler
    | Trap


type alias Message =
    { nodes : List MessageNode
    , extra : MessageExtra
    }


type MessageNode
    = ItemMessageNode String
    | LocationMessageNode String
    | ColorMessageNode String String
    | TextualMessageNode String
    | PlayerMessageNode String


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


type alias SlotData =
    { locationScouting : LocationScouting
    , progression : Progression
    }


type LocationScouting
    = ScoutingAuto
    | ScoutingManual
    | ScoutingDisabled


---
-- JSON encoding/decoding
---


keyDownDecoder : Model -> Decode.Decoder ( Msg, Bool )
keyDownDecoder model =
    Decode.field "code" Decode.string
        |> Decode.andThen
            (\code ->
                let
                    keyMap : Dict String Msg
                    keyMap =
                        [ ( "ArrowUp", MoveSelectionPressed ( -1, 0 ) )
                        , ( "ArrowDown", MoveSelectionPressed ( 1, 0 ) )
                        , ( "ArrowLeft", MoveSelectionPressed ( 0, -1 ) )
                        , ( "ArrowRight", MoveSelectionPressed ( 0, 1 ) )
                        , ( "Backspace", DeletePressed )
                        , ( "Delete", DeletePressed )
                        , ( "Digit1", NumberPressed 1 )
                        , ( "Digit2", NumberPressed 2 )
                        , ( "Digit3", NumberPressed 3 )
                        , ( "Digit4", NumberPressed 4 )
                        , ( "Digit5", NumberPressed 5 )
                        , ( "Digit6", NumberPressed 6 )
                        , ( "Digit7", NumberPressed 7 )
                        , ( "Digit8", NumberPressed 8 )
                        , ( "Digit9", NumberPressed 9 )
                        , ( "Digit0", NumberPressed 10 )
                        , ( "KeyA", NumberPressed 11 )
                        , ( "KeyB", NumberPressed 12 )
                        , ( "KeyC", NumberPressed 13 )
                        , ( "KeyD", NumberPressed 14 )
                        , ( "KeyE", NumberPressed 15 )
                        , ( "KeyF", NumberPressed 16 )
                        , ( "KeyH", MoveSelectionPressed ( 0, -1 ) )
                        , ( "KeyJ", MoveSelectionPressed ( 1, 0 ) )
                        , ( "KeyK", MoveSelectionPressed ( -1, 0 ) )
                        , ( "KeyL", MoveSelectionPressed ( 0, 1 ) )
                        , ( "KeyQ", FillCellCandidatesPressed )
                        , ( "KeyW", RemoveInvalidCandidatesPressed )
                        , ( "KeyS", SelectSingleCandidateCellPressed )
                        , ( "KeyZ", UndoPressed )
                        , ( "Numpad1", NumberPressed 1 )
                        , ( "Numpad2", NumberPressed 2 )
                        , ( "Numpad3", NumberPressed 3 )
                        , ( "Numpad4", NumberPressed 4 )
                        , ( "Numpad5", NumberPressed 5 )
                        , ( "Numpad6", NumberPressed 6 )
                        , ( "Numpad7", NumberPressed 7 )
                        , ( "Numpad8", NumberPressed 8 )
                        , ( "Numpad9", NumberPressed 9 )
                        , ( "Numpad0", NumberPressed 10 )
                        , ( "NumpadAdd", ZoomInPressed )
                        , ( "NumpadSubtract", ZoomOutPressed )
                        , ( "ShiftLeft", ShiftHeld )
                        , ( "ShiftRight", ShiftHeld )
                        , ( "Space", ToggleCandidateModePressed )
                        , ( "Tab", ToggleHighlightModePressed )
                        ]
                        |> Dict.fromList
                in
                case Dict.get code keyMap of
                    Just msg ->
                        Decode.succeed ( msg, True )

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
                        Decode.succeed ShiftReleased

                    "ShiftRight" ->
                        Decode.succeed ShiftReleased

                    _ ->
                        Decode.fail code
            )


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map2 Flags
        (Decode.field "localStorage" (Decode.dict Decode.string))
        (Decode.field "seed" Decode.int)


generatedBoardDecoder : Decode.Decoder GeneratedBoard
generatedBoardDecoder =
    Decode.map6 GeneratedBoard
        (Decode.field "blockSize" Decode.int)
        (Decode.field "blockUnlockOrder" (Decode.list blockUnlockOrderDecoder))
        (Decode.field "givens" (cellsDictDecoder Decode.int))
        (Decode.field "puzzleAreas" puzzleAreasDecoder)
        (Decode.field "solution" (cellsDictDecoder Decode.int))
        (Decode.field "unlockMap" unlockMapDecoder)


blockUnlockOrderDecoder : Decode.Decoder ( Int, Int )
blockUnlockOrderDecoder =
    Decode.oneOf
        [ tupleDecoder Decode.int Decode.int
        , Decode.andThen
            (\id ->
                case itemFromId id of
                    Block ( row, col ) ->
                        Decode.succeed ( row, col )

                    _ ->
                        Decode.fail ("Invalid block id: " ++ String.fromInt id)
            )
            Decode.int
        ]


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


unlockMapDecoder : Decode.Decoder (Dict Int Item)
unlockMapDecoder =
    Decode.list
        (Decode.map2
            Tuple.pair
            (Decode.index 0 Decode.int)
            (Decode.index 1 itemDecoder)
        )
        |> Decode.map Dict.fromList


areaDecoder : Decode.Decoder Area
areaDecoder =
    Decode.map4 Area
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Decode.int)
        (Decode.index 3 Decode.int)


puzzleAreasDecoder : Decode.Decoder PuzzleAreas
puzzleAreasDecoder =
    Decode.map4 PuzzleAreas
        (Decode.field "blocks" (Decode.list areaDecoder))
        (Decode.field "boards" (Decode.list areaDecoder))
        (Decode.field "rows" (Decode.list areaDecoder))
        (Decode.field "cols" (Decode.list areaDecoder))


tupleDecoder : Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder ( a, b )
tupleDecoder decodeA decodeB =
    Decode.map2 Tuple.pair
        (Decode.index 0 decodeA)
        (Decode.index 1 decodeB)


encodeGenerateArgs : GenerateArgs -> Encode.Value
encodeGenerateArgs args =
    Encode.object
        [ ( "blockSize", Encode.int args.blockSize )
        , ( "boardsPerCluster", Encode.int args.boardsPerCluster )
        , ( "difficulty", Encode.int args.difficulty )
        , ( "numberOfBoards", Encode.int args.numberOfBoards )
        , ( "progression", encodeProgression args.progression )
        , ( "seed", Encode.int args.seed )
        ]


encodeProgression : Progression -> Encode.Value
encodeProgression progression =
    case progression of
        Fixed ->
            Encode.string "fixed"

        Shuffled ->
            Encode.string "shuffled"


progressionDecoder : Decode.Decoder Progression
progressionDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "fixed" ->
                        Decode.succeed Fixed

                    "shuffled" ->
                        Decode.succeed Shuffled

                    _ ->
                        Decode.fail ("Unknown progression: " ++ value)
            )


itemDecoder : Decode.Decoder Item
itemDecoder =
    Decode.int
        |> Decode.map itemFromId


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


slotDataDecoder : Decode.Decoder SlotData
slotDataDecoder =
    Field.optional "locationScouting" locationScoutingDecoder <| \locationScouting ->
    Field.optional "progression" progressionDecoder <| \progression ->
    Decode.succeed
        { locationScouting = Maybe.withDefault ScoutingManual locationScouting
        , progression = Maybe.withDefault Shuffled progression
        }


locationScoutingDecoder : Decode.Decoder LocationScouting
locationScoutingDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "auto" ->
                        Decode.succeed ScoutingAuto

                    "manual" ->
                        Decode.succeed ScoutingManual

                    "disabled" ->
                        Decode.succeed ScoutingDisabled

                    _ ->
                        Decode.fail ("Unknown location scouting: " ++ value)
            )


generationProgressDecoder : Decode.Decoder ( String, Float )
generationProgressDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "label" Decode.string)
        (Decode.field "percent" Decode.float)


encodeTriggerAnimation : String -> List ( Int, Int ) -> Encode.Value
encodeTriggerAnimation animationType cells =
    Encode.object
        [ ( "ids", Encode.list Encode.string (List.map cellHtmlId cells) )
        , ( "type", Encode.string animationType )
        ]


hintDecoder : Decode.Decoder Hint
hintDecoder =
    Decode.succeed Hint
        |> DecodeExtra.andMap (Decode.field "locationId" Decode.int)
        |> DecodeExtra.andMap (Decode.field "locationName" Decode.string)
        |> DecodeExtra.andMap (Decode.field "itemId" Decode.int)
        |> DecodeExtra.andMap (Decode.field "itemName" Decode.string)
        |> DecodeExtra.andMap (Decode.field "itemClass" itemClassDecoder)
        |> DecodeExtra.andMap (Decode.field "senderAlias" Decode.string)
        |> DecodeExtra.andMap (Decode.field "senderName" Decode.string)
        |> DecodeExtra.andMap (Decode.field "receiverAlias" Decode.string)
        |> DecodeExtra.andMap (Decode.field "receiverName" Decode.string)
        |> DecodeExtra.andMap (Decode.field "gameName" Decode.string)


---
-- Update helpers & utility functions
---


updateFromLocalStorage : Dict String String -> Model -> ( Model, Cmd Msg )
updateFromLocalStorage storage model =
    Dict.foldl
        (\key value ->
            andThen (updateFromLocalStorageValue key value)
        )
        ( model, Cmd.none )
        storage


updateFromLocalStorageValue : String -> String -> Model -> ( Model, Cmd Msg )
updateFromLocalStorageValue key value model =
    case key of
        "apdk-animations-enabled" ->
            ( { model | animationsEnabled = value == "1" }
            , Cmd.none
            )

        "apdk-color-scheme" ->
            ( { model | colorScheme = value }
            , Cmd.none
            )

        "apdk-auto-fill-candidates-on-unlock" ->
            ( { model | autoFillCandidatesOnUnlock = value == "1" }
            , Cmd.none
            )

        "apdk-auto-remove-invalid-candidates" ->
            ( { model | autoRemoveInvalidCandidates = value == "1" }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


pushUndoStack : Model -> List (Dict ( Int, Int ) CellValue)
pushUndoStack model =
    model.current :: model.undoStack
        |> List.take 10


moveSelection : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
moveSelection ( rowOffset, colOffset ) model =
    let
        ( row, col ) =
            model.selectedCell

        newCell : ( Int, Int )
        newCell =
            List.range 1 5
                |> List.map (\mult -> ( row + rowOffset * mult, col + colOffset * mult ))
                |> List.filter (\cell -> Dict.member cell model.solution)
                |> List.head
                |> Maybe.withDefault ( row, col )
    in
    ( { model
        | selectedCell = newCell
      }
        |> updateHighlightedCells
    , Cmd.batch
        [ moveCellIntoView (cellHtmlId newCell)
        , Browser.Dom.focus (cellHtmlId newCell)
            |> Task.attempt (always NoOp)
        ]
    )


updateHighlightedCells : Model -> Model
updateHighlightedCells model =
    case model.highlightMode of
        HighlightNone ->
            { model | highlightedCells = Set.empty }

        HighlightBoard ->
            { model
                | highlightedCells =
                    Dict.get model.selectedCell model.cellBoards
                        |> Maybe.withDefault []
                        |> List.concatMap getAreaCells
                        |> Set.fromList
            }

        HighlightArea ->
            { model
                | highlightedCells =
                    List.foldl
                        Set.union
                        Set.empty
                        [ Dict.get model.selectedCell model.cellBlocks
                            |> Maybe.withDefault []
                            |> List.concatMap getAreaCells
                            |> Set.fromList
                        , Dict.get model.selectedCell model.cellRows
                            |> Maybe.withDefault []
                            |> List.concatMap getAreaCells
                            |> Set.fromList
                        , Dict.get model.selectedCell model.cellCols
                            |> Maybe.withDefault []
                            |> List.concatMap getAreaCells
                            |> Set.fromList
                        ]
            }

        HighlightNumber ->
            { model | highlightedCells = Set.empty }


getCandidateMode : Model -> Bool
getCandidateMode model =
    if Set.member "Shift" model.heldKeys then
        not model.candidateMode

    else
        model.candidateMode


removeInvalidCandidates : Model -> Model
removeInvalidCandidates model =
    { model
        | current =
            Dict.foldl
                (\cell cellErrors current ->
                    case ( cellErrors, Dict.get cell current ) of
                        ( CandidateErrors errorNumbers, Just (Multiple values) ) ->
                            Dict.insert
                                cell
                                (Set.diff values errorNumbers
                                    |> Multiple
                                )
                                current

                        _ ->
                            current
                )
                model.current
                model.errors
    }


andThen : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen fun ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            fun model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


andThenIf : Bool -> (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThenIf condition fun ( model, cmd ) =
    if condition then
        andThen fun ( model, cmd )

    else
        ( model, cmd )


updateState : Model -> ( Model, Cmd Msg )
updateState model =
    case model.gameState of
        Playing ->
            ( model, Cmd.none )
                |> andThen updateStateCellChangesLoop
                |> andThen updateStateErrors
                |> andThenIf model.autoRemoveInvalidCandidates updateStateRemoveInvalidCandidates
                |> andThenIf model.autoRemoveInvalidCandidates updateStateErrors
                |> andThen updateStateScoutLocations

        _ ->
            ( model, Cmd.none )


updateStateItems : Model -> ( Model, Cmd Msg )
updateStateItems model =
    List.foldl
        (andThen << updateStateItem)
        ( { model | pendingItems = [] }
        , Cmd.none
        )
        model.pendingItems


updateStateCellChangesLoop : Model -> ( Model, Cmd Msg )
updateStateCellChangesLoop initialModel =
    ( initialModel, Cmd.none )
        |> andThen updateStateItems
        |> andThen updateStateCellChanges
        |> andThen updateStateSolvedBlocks
        |> andThen updateStateCheckLocations
        |> andThen
            (\updatedModel ->
                if Set.isEmpty updatedModel.pendingCellChanges && List.isEmpty updatedModel.pendingItems then
                    ( updatedModel, Cmd.none )

                else
                    updateStateCellChangesLoop updatedModel
            )


updateStateCellChanges : Model -> ( Model, Cmd Msg )
updateStateCellChanges model =
    Set.foldl
        (andThen << updateStateCellChange)
        ( { model | pendingCellChanges = Set.empty }
        , Cmd.none
        )
        model.pendingCellChanges


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


updateStateRemoveInvalidCandidates : Model -> ( Model, Cmd Msg )
updateStateRemoveInvalidCandidates model =
    ( removeInvalidCandidates model
    , Cmd.none
    )


updateStateCheckLocations : Model -> ( Model, Cmd Msg )
updateStateCheckLocations model =
    Set.foldl
        (andThen << updateStateCheckLocation)
        ( { model | pendingCheckLocations = Set.empty }
        , Cmd.none
        )
        model.pendingCheckLocations


updateStateScoutLocations : Model -> ( Model, Cmd Msg )
updateStateScoutLocations model =
    ( { model
        | pendingScoutLocations = Set.empty
        , scoutedItems =
            if model.gameIsLocal && model.locationScouting == ScoutingAuto then
                Set.foldl
                    (\id scoutedItems ->
                        case Dict.get id model.unlockMap of
                            Just item ->
                                Dict.insert id (createHint id item) scoutedItems

                            Nothing ->
                                scoutedItems
                    )
                    model.scoutedItems
                    model.pendingScoutLocations

            else
                model.scoutedItems
      }
    , if not model.gameIsLocal && model.locationScouting == ScoutingAuto then
        scoutLocations (Set.toList model.pendingScoutLocations)

      else
        Cmd.none
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
                        if (not <| Set.member blockId model.solvedLocations)
                            && List.all (cellIsSolved model) (getAreaCells block)
                            && List.all (cellIsVisible model) (getAreaCells block)
                        then
                            ( { model
                                | pendingCheckLocations =
                                    Set.insert blockId model.pendingCheckLocations
                                , pendingSolvedBlocks =
                                    Set.insert
                                        ( block.startRow, block.startCol )
                                        model.pendingSolvedBlocks
                                , solvedLocations =
                                    Set.insert blockId model.solvedLocations
                              }
                            , Cmd.none
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
                            && List.all (cellIsSolved model) (getAreaCells row)
                            && List.all (cellIsVisible model) (getAreaCells row)
                        then
                            ( { model
                                | pendingCheckLocations =
                                    Set.insert rowId model.pendingCheckLocations
                                , pendingSolvedRows =
                                    Set.insert
                                        ( row.startRow, row.startCol )
                                        model.pendingSolvedRows
                                , solvedLocations =
                                    Set.insert rowId model.solvedLocations
                              }
                            , Cmd.none
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
                            && List.all (cellIsSolved model) (getAreaCells col)
                            && List.all (cellIsVisible model) (getAreaCells col)
                        then
                            ( { model
                                | pendingCheckLocations =
                                    Set.insert colId model.pendingCheckLocations
                                , pendingSolvedCols =
                                    Set.insert
                                        ( col.startRow, col.startCol )
                                        model.pendingSolvedCols
                                , solvedLocations =
                                    Set.insert colId model.solvedLocations
                              }
                            , Cmd.none
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
            (\board ->
                andThen
                    (\model ->
                        let
                            boardId : Int
                            boardId =
                                cellToBoardId ( board.startRow, board.startCol )
                        in
                        if (not <| Set.member boardId model.solvedLocations)
                            && List.all (cellIsSolved model) (getAreaCells board)
                            && List.all (cellIsVisible model) (getAreaCells board)
                        then
                            ( { model
                                | pendingCheckLocations =
                                    Set.insert boardId model.pendingCheckLocations
                                , solvedLocations =
                                    Set.insert boardId model.solvedLocations
                              }
                            , Cmd.none
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
            unlockBlock block model

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
                |> Maybe.map getAreaCells
                |> Maybe.withDefault []
                |> Set.fromList

        newVisibleCells : Set ( Int, Int )
        newVisibleCells =
            Set.union blockCells model.visibleCells

        unlockedAreas :
            Dict ( Int, Int ) (List Area)
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
                            getAreaCells area
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
                        [ "Unlocked Block "
                        , rowToLabel (Tuple.first block)
                        , String.fromInt (Tuple.second block)
                        ]
                    )
                    model.messages

            else
                model.messages
        , pendingCellChanges = Set.union blockCells model.pendingCellChanges
        , pendingScoutLocations =
            model.pendingScoutLocations
                |> Set.insert (cellToBlockId block)
                |> Set.union (unlockedRows)
                |> Set.union (unlockedCols)
                |> Set.union (unlockedBoards)
        , unlockedBlocks = Set.insert block model.unlockedBlocks
        , visibleCells = newVisibleCells
      }
        |> autoFillCandidatesOnUnlock (Set.diff blockCells model.visibleCells)
    , if model.animationsEnabled then
        triggerAnimation
            (Set.diff blockCells model.visibleCells
                |> Set.toList
                |> encodeTriggerAnimation "shatter"
            )

      else
        Cmd.none
    )


autoFillCandidatesOnUnlock : Set ( Int, Int ) -> Model -> Model
autoFillCandidatesOnUnlock cells model =
    if model.autoFillCandidatesOnUnlock then
        { model
            | current =
                Set.foldl
                    (\cell current ->
                        if Set.member cell model.givens then
                            current

                        else
                            Dict.insert
                                cell
                                (getValidCellCandidates model cell
                                    |> Multiple
                                )
                                current
                    )
                    model.current
                    cells
        }

    else
        model


getBoardErrors : Model -> Dict ( Int, Int ) CellError
getBoardErrors model =
    List.foldl
        (\area errors ->
            let
                areaCells : List ( Int, Int )
                areaCells =
                    getAreaCells area
            in
            List.foldl
                (\cell acc ->
                    let
                        cellValue : Maybe CellValue
                        cellValue =
                            getCellValue model cell
                    in
                    case cellValue of
                        Just (Given v) ->
                            let
                                numbersInArea : Set Int
                                numbersInArea =
                                    areaCells
                                        |> List.filter ((/=) cell)
                                        |> List.filter (\areaCell -> Set.member areaCell model.visibleCells)
                                        |> List.filterMap (getCellValue model)
                                        |> List.map cellValueToInts
                                        |> List.foldl Set.union Set.empty
                            in
                            if Set.member v numbersInArea then
                                Dict.insert
                                    cell
                                    ErrorContext
                                    acc

                            else
                                acc

                        Just (Single v) ->
                            let
                                numbersInArea : Set Int
                                numbersInArea =
                                    areaCells
                                        |> List.filter ((/=) cell)
                                        |> List.filter (\areaCell -> Set.member areaCell model.visibleCells)
                                        |> List.filterMap (getCellValue model)
                                        |> List.filterMap cellValueToInt
                                        |> Set.fromList

                                candidatesInArea : Set Int
                                candidatesInArea =
                                    areaCells
                                        |> List.filter ((/=) cell)
                                        |> List.filter (\areaCell -> Set.member areaCell model.visibleCells)
                                        |> List.filterMap (getCellValue model)
                                        |> List.map cellValueToInts
                                        |> List.foldl Set.union Set.empty
                            in
                            if Set.member v numbersInArea then
                                Dict.insert
                                    cell
                                    NumberError
                                    acc

                            else if Set.member v candidatesInArea then
                                Dict.insert
                                    cell
                                    ErrorContext
                                    acc

                            else
                                acc

                        Just (Multiple numbers) ->
                            let
                                numbersInArea : Set Int
                                numbersInArea =
                                    areaCells
                                        |> List.filter ((/=) cell)
                                        |> List.filter (\areaCell -> Set.member areaCell model.visibleCells)
                                        |> List.filterMap (getCellValue model)
                                        |> List.filterMap cellValueToInt
                                        |> Set.fromList
                            in
                            Dict.update
                                cell
                                (\maybeSet ->
                                    case maybeSet of
                                        Just (CandidateErrors set) ->
                                            Just <| CandidateErrors <| Set.union set numbersInArea

                                        _ ->
                                            Just <| CandidateErrors numbersInArea
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
                , messages =
                    if model.gameIsLocal then
                        addLocalMessage "Unlocked a Solve Selected Cell." model.messages

                    else
                        model.messages
              }
            , Cmd.none
            )

        SolveRandomCell ->
            ( { model
                | solveRandomCellUses = model.solveRandomCellUses + 1
                , messages =
                    if model.gameIsLocal then
                        addLocalMessage "Unlocked a Solve Random Cell." model.messages

                    else
                        model.messages
              }
            , Cmd.none
            )

        RemoveRandomCandidate ->
            ( { model
                | removeRandomCandidateUses = model.removeRandomCandidateUses + 1
                , messages =
                    if model.gameIsLocal then
                        addLocalMessage "Unlocked a Remove Random Candidate." model.messages

                    else
                        model.messages
              }
            , Cmd.none
            )

        NothingItem ->
            ( model
            , Cmd.none
            )


updateStateSolvedArea :
    Dict ( Int, Int ) (List Area)
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
                |> Maybe.map getAreaCells
                |> Maybe.withDefault []
    in
    ( { model
        | current =
            Dict.filter
                (\cell _ -> not <| List.member cell cells)
                model.current
        , givens = Set.union (Set.fromList cells) model.givens
        , pendingCellChanges = Set.union (Set.fromList cells) model.pendingCellChanges
        , solvedLocations = Set.insert (toId ( row, col )) model.solvedLocations
      }
    , if model.animationsEnabled then
        triggerAnimation (encodeTriggerAnimation "shine" cells)

      else
        Cmd.none
    )


updateStateCheckLocation : Int -> Model -> ( Model, Cmd Msg )
updateStateCheckLocation id model =
    if model.gameIsLocal then
        case Dict.get id model.unlockMap of
            Just item ->
                ( { model
                    | pendingItems = item :: model.pendingItems
                    , scoutedItems = Dict.insert id (createHint id item) model.scoutedItems
                  }
                , Cmd.none
                )

            Nothing ->
                ( model
                , Cmd.none
                )

    else
        ( model
        , checkLocation id
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
    case ( getCellValue model cell, Dict.get cell model.solution ) of
        ( Just (Given v), Just sol ) ->
            v == sol

        ( Just (Single v), Just sol ) ->
            v == sol

        _ ->
            False


cellIsVisible : Model -> ( Int, Int ) -> Bool
cellIsVisible model cell =
    Set.member cell model.visibleCells


cellIsGiven : Model -> ( Int, Int ) -> Bool
cellIsGiven model cell =
    Set.member cell model.givens


getCellValue : Model -> ( Int, Int ) -> Maybe CellValue
getCellValue model cell =
    if Set.member cell model.givens then
        Dict.get cell model.solution
            |> Maybe.map Given

    else
        Dict.get cell model.current


getValidCellCandidates : Model -> ( Int, Int ) -> Set Int
getValidCellCandidates model cell =
    List.foldl
        (\area numbers ->
            let
                numbersInArea : Set Int
                numbersInArea =
                    getAreaCells area
                        |> List.filter ((/=) cell)
                        |> List.filter (\areaCell -> Set.member areaCell model.visibleCells)
                        |> List.filterMap (getCellValue model)
                        |> List.filterMap cellValueToInt
                        |> Set.fromList
            in
            Set.diff numbers numbersInArea
        )
        (List.range 1 model.blockSize
            |> Set.fromList
        )
        (List.concat
            [ Dict.get cell model.cellBlocks
                |> Maybe.withDefault []
            , Dict.get cell model.cellRows
                |> Maybe.withDefault []
            , Dict.get cell model.cellCols
                |> Maybe.withDefault []
            ]
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


numberToString : Int -> String
numberToString number =
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
    500


blockSizeToDimensions : Int -> ( Int, Int )
blockSizeToDimensions blockSize =
    case blockSize of
        4 ->
            ( 2, 2 )

        6 ->
            ( 2, 3 )

        8 ->
            ( 2, 4 )

        9 ->
            ( 3, 3 )

        12 ->
            ( 3, 4 )

        16 ->
            ( 4, 4 )

        _ ->
            ( 1, 1 )


buildCellAreasMap : List Area -> Dict ( Int, Int ) (List Area)
buildCellAreasMap areas =
    List.foldl
        (\area acc ->
            let
                areaCells : List ( Int, Int )
                areaCells =
                    getAreaCells area
            in
            List.foldl
                (\cell acc2 ->
                    let
                        existingAreas : List Area
                        existingAreas =
                            Dict.get cell acc2
                                |> Maybe.withDefault []
                    in
                    Dict.insert cell (area :: existingAreas) acc2
                )
                acc
                areaCells
        )
        Dict.empty
        areas


getAreaCells : Area -> List ( Int, Int )
getAreaCells area =
    List.concatMap
        (\row ->
            List.map
                (Tuple.pair row)
                (List.range area.startCol area.endCol)
        )
        (List.range area.startRow area.endRow)


maxNumberOfBoards : Int -> Int
maxNumberOfBoards blockSize =
    if blockSize == 16 then
        36

    else if blockSize == 12 then
        64

    else
        100


numberOfBoardsTicks : Int -> List Int
numberOfBoardsTicks boardsPerCluster =
    case boardsPerCluster of
        1 ->
            [ 4, 9, 16, 25, 36, 49, 64, 81, 100 ]

        5 ->
            [ 5, 10, 20, 45, 80, 100 ]

        8 ->
            [ 8, 24, 40, 64, 96 ]

        13 ->
            [ 13, 26, 39, 65, 91 ]

        100 ->
            [ 3, 5, 8, 13, 18, 25, 32, 41, 50, 72, 98 ]

        _ ->
            []


itemFromId : Int -> Item
itemFromId id =
    if id >= 1000000 then
        Block (cellFromId id)

    else if id == 1 then
        SolveRandomCell

    else if id == 2 then
        RemoveRandomCandidate

    else if id == 101 then
        ProgressiveBlock

    else if id == 201 then
        SolveSelectedCell

    else
        NothingItem


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


createHint : Int -> Item -> Hint
createHint locationId item =
    { locationId = locationId
    , locationName = ""
    , itemId = 0
    , itemName = itemName item
    , itemClass = itemClassification item
    , senderAlias = ""
    , senderName = ""
    , receiverAlias = ""
    , receiverName = ""
    , gameName = ""
    }


itemName : Item -> String
itemName item =
    case item of
        ProgressiveBlock ->
            "Progressive Block"

        Block ( row, col ) ->
            "Block " ++ rowToLabel row ++ String.fromInt col

        SolveSelectedCell ->
            "Solve Selected Cell"

        SolveRandomCell ->
            "Solve Random Cell"

        RemoveRandomCandidate ->
            "Remove Random Candidate"

        NothingItem ->
            "Nothing"


itemClassification : Item -> ItemClass
itemClassification item =
    case item of
        ProgressiveBlock ->
            Progression

        Block _ ->
            Progression

        SolveSelectedCell ->
            Useful

        SolveRandomCell ->
            Filler

        RemoveRandomCandidate ->
            Filler

        NothingItem ->
            Filler


---
-- View functions
---


view : Model -> Html Msg
view model =
    Html.main_
        [ HA.class "main-container"
        ]
        (List.append
            [ Html.node "style"
                []
                [ Html.text
                    (String.concat
                        [ ":root { color-scheme: "
                        , model.colorScheme
                        , "; }"
                        ]
                    )
                ]
            ]
            (case model.gameState of
                MainMenu ->
                    [ viewMenu model
                    ]

                Connecting ->
                    [ Html.div
                        [ HA.style "padding" "var(--spacing-l)"
                        ]
                        [ Html.text "Connecting..." ]
                    ]

                Generating ->
                    [ Html.div
                        [ HA.style "padding" "var(--spacing-l)"
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
                    ]

                Playing ->
                    [ viewBoard model
                    , viewInfoPanel model
                    ]
            )
        )


viewMenu : Model -> Html Msg
viewMenu model =
    Html.div
        [ HA.class "column gap-xl"
        , HA.style "padding" "var(--spacing-l)"
        ]
        [ Html.div
            [ HA.class "column gap-m"
            , HA.style "align-items" "start"
            ]
            [ Html.h2
                [ HA.style "margin" "0" ]
                [ Html.text "Connect to Archipelago" ]
            , Html.form
                [ HA.class "row gap-m wrap"
                , HE.onSubmit ConnectPressed
                ]
                [ Html.label
                    [ HA.class "column"
                    ]
                    [ Html.text "Host:"
                    , Html.input
                        [ HA.type_ "text"
                        , HA.placeholder "archipelago.gg:12345"
                        , HA.value model.host
                        , HE.onInput HostInputChanged
                        ]
                        []
                    ]
                , Html.label
                    [ HA.class "column"
                    ]
                    [ Html.text "Slot Name:"
                    , Html.input
                        [ HA.type_ "text"
                        , HA.placeholder "Player1"
                        , HA.value model.player
                        , HE.onInput PlayerInputChanged
                        ]
                        []
                    ]
                , Html.label
                    [ HA.class "column"
                    ]
                    [ Html.text "Password:"
                    , Html.input
                        [ HA.type_ "text"
                        , HA.placeholder ""
                        , HA.value model.password
                        , HE.onInput PasswordInputChanged
                        ]
                        []
                    ]
                , Html.button
                    [ HA.class "button"
                    , HA.style "align-self" "end"
                    ]
                    [ Html.text "Connect"]
                ]
            ]
        , Html.div
            [ HA.class "column gap-m"
            , HA.style "align-items" "start"
            ]
            [ Html.h2
                [ HA.style "margin" "0" ]
                [ Html.text "Play Local Game" ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.text "Block Size:"
                , Html.div
                    [ HA.class "row gap-m wrap"
                    ]
                    (List.map
                        (\size ->
                            viewNumberRadioButton size model.blockSize "block-size" BlockSizeChanged
                        )
                        [ 4, 6, 8, 9, 12, 16 ]
                    )
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.text "Boards per Cluster: "
                , Html.div
                    [ HA.class "row gap-m wrap"
                    ]
                    (List.map
                        (\number ->
                            viewNumberRadioButton
                                number
                                model.boardsPerCluster
                                "boards-per-cluster"
                                BoardsPerClusterChanged
                        )
                        [ 1, 5, 8, 13, 100 ]
                    )
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.text "Number of Boards: "
                , Html.text (String.fromInt model.numberOfBoards)
                , viewRangeSlider
                    model.numberOfBoards
                    1
                    (maxNumberOfBoards model.blockSize)
                    NumberOfBoardsChanged
                    (Just "number-of-boards-ticks")
                , Html.datalist
                    [ HA.id "number-of-boards-ticks"
                    ]
                    (List.map
                        (\tick ->
                            Html.option
                                [ HA.value (String.fromInt tick) ]
                                []
                        )
                        (numberOfBoardsTicks model.boardsPerCluster)
                    )
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.text "Difficulty:"
                , Html.div
                    [ HA.class "row gap-m wrap"
                    ]
                    [ viewRadioButton 1 model.difficulty "difficulty" DifficultyChanged (\_ -> "Beginner")
                    , viewRadioButton 2 model.difficulty "difficulty" DifficultyChanged (\_ -> "Easy")
                    , viewRadioButton 3 model.difficulty "difficulty" DifficultyChanged (\_ -> "Medium")
                    , viewRadioButton 4 model.difficulty "difficulty" DifficultyChanged (\_ -> "Hard")
                    ]
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.text "Progression:"
                , Html.div
                    [ HA.class "row gap-m wrap"
                    ]
                    [ viewRadioButton Shuffled model.progression "progression" ProgressionChanged (\_ -> "Shuffled")
                    , viewRadioButton Fixed model.progression "progression" ProgressionChanged (\_ -> "Fixed")
                    ]
                ]
            , Html.div
                [ HA.class "row gap-s"
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
                [ HA.class "button"
                , HE.onClick PlayLocalPressed
                ]
                [ Html.text "Play" ]
            ]
        ]


viewRadioButton : a -> a -> String -> (a -> Msg) -> (a -> String) -> Html Msg
viewRadioButton value selected name msg toLabel =
    Html.label
        [ HA.class "row gap-s"
        , HA.style "align-items" "baseline"
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


viewRangeSlider : Int -> Int -> Int -> (Int -> Msg) -> Maybe String -> Html Msg
viewRangeSlider value min max msg list =
    Html.input
        [ HA.type_ "range"
        , HA.min (String.fromInt min)
        , HA.max (String.fromInt max)
        , HA.value (String.fromInt value)
        , HAE.attributeMaybe HA.list list
        , HA.style "width" "250px"
        , HE.onInput (String.toInt >> Maybe.withDefault value >> msg)
        ]
        []


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
        [ HA.class "grid"
        , HA.class <| "block-" ++ String.fromInt model.blockSize
        , HA.style "grid-template-rows" ("repeat(" ++ String.fromInt (rows + 1) ++ ", 1.5em)")
        , HA.style "grid-template-columns" ("repeat(" ++ String.fromInt (cols + 1) ++ ", 1.5em)")
        , HE.preventDefaultOn "keydown" (keyDownDecoder model)
        , HE.on "keyup" keyUpDecoder
        ]
        [ Html.div
            [ HA.class "grid-corner" ]
            []
        , Html.div
            [ HA.class "grid-columns-header" ]
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
            [ HA.class "grid-rows-header" ]
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
            [ HA.class "grid-cells" ]
            (List.concat
                [ List.map
                    (viewCell model)
                    (List.range 1 (rows)
                        |> List.concatMap
                            (\row ->
                                List.range 1 (cols)
                                    |> List.map (Tuple.pair row)
                            )
                    )
                , List.map
                    (\block ->
                        Html.div
                            [ HA.class "block"
                            , HA.style "grid-area"
                                (String.concat
                                    [ String.fromInt block.startRow
                                    , " / "
                                    , String.fromInt block.startCol
                                    , " / "
                                    , String.fromInt (block.endRow + 1)
                                    , " / "
                                    , String.fromInt (block.endCol + 1)
                                    ]
                                )
                            ]
                            []
                    )
                    model.puzzleAreas.blocks
                , List.map
                    (\board ->
                        Html.div
                            [ HA.class "board"
                            , HA.style "grid-area"
                                (String.concat
                                    [ String.fromInt board.startRow
                                    , " / "
                                    , String.fromInt board.startCol
                                    , " / "
                                    , String.fromInt (board.endRow + 1)
                                    , " / "
                                    , String.fromInt (board.endCol + 1)
                                    ]
                                )
                            ]
                            []
                    )
                    model.puzzleAreas.boards
                ]
            )
        , viewZoomControls
        ]



viewCell : Model -> ( Int, Int ) -> Html Msg
viewCell model ( row, col ) =
    let
        cellIsAt : ( Int, Int ) -> Bool
        cellIsAt ( r, c ) =
            Dict.member ( r, c ) model.solution

        isVisible : Bool
        isVisible =
            Set.member ( row, col ) model.visibleCells

        isDimmed : Bool
        isDimmed =
            if model.highlightMode == HighlightNumber then
                if not (cellIsVisible model model.selectedCell) then
                    False

                else if not isVisible then
                    case getCellValue model model.selectedCell of
                        Just selectedCellValue ->
                            not (Set.isEmpty (cellValueToInts selectedCellValue))

                        Nothing ->
                            False

                else
                    case ( getCellValue model model.selectedCell, cellValue ) of
                        ( Just selectedCellValue, Just currentCellValue ) ->
                            Set.intersect
                                (cellValueToInts selectedCellValue)
                                (cellValueToInts currentCellValue)
                                |> Set.isEmpty

                        ( Just _, Nothing ) ->
                            True

                        _ ->
                            False

            else
                not (Set.isEmpty model.highlightedCells)
                    && not (Set.member ( row, col ) model.highlightedCells)

        highlightNumbers : Set Int
        highlightNumbers =
            if model.highlightMode == HighlightNumber
                && not isDimmed
                && cellIsVisible model model.selectedCell
            then
                getCellValue model model.selectedCell
                    |> Maybe.map cellValueToInts
                    |> Maybe.withDefault Set.empty

            else
                Set.empty

        cellError : Maybe CellError
        cellError =
            Dict.get ( row, col ) model.errors

        errorIsContext : Bool
        errorIsContext =
            case cellError of
                Just ErrorContext ->
                    True

                _ ->
                    False

        errorIsNumber : Bool
        errorIsNumber =
            case cellError of
                Just NumberError ->
                    True

                _ ->
                    False

        cellIsMultiple : Bool
        cellIsMultiple =
            cellValue
                |> Maybe.map isMultiple
                |> Maybe.withDefault False

        cellValue : Maybe CellValue
        cellValue =
            getCellValue model ( row, col )
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
                (Maybe.andThen cellValueToInt cellValue)
            , HA.classList
                [ ( "selected", model.selectedCell == ( row, col ) )
                , ( "given", cellIsGiven model ( row, col ) && isVisible )
                , ( "multi", cellIsMultiple )
                , ( "hidden", not isVisible )
                , ( "error", errorIsNumber && (not cellIsMultiple) && isVisible )
                , ( "error-context", errorIsContext && (not cellIsMultiple) && isVisible )
                , ( "dimmed", isDimmed )
                ]
            , HE.onClick (CellSelected ( row, col ))
            ]
            (if isVisible then
                case cellValue of
                    Just value ->
                        case value of
                            Given v ->
                                [ Html.text (numberToString v) ]

                            Single v ->
                                [ Html.text (numberToString v) ]

                            Multiple numbers ->
                                viewMultipleNumbers model.blockSize cellError highlightNumbers numbers

                    Nothing ->
                        []

             else
                []
            )

    else
        Html.text ""


viewMultipleNumbers : Int -> Maybe CellError -> Set Int -> Set Int -> List (Html Msg)
viewMultipleNumbers blockSize cellError highlightNumbers numbers =
    let
        errorNumbers : Set Int
        errorNumbers =
            case cellError of
                Just (CandidateErrors nums) ->
                    nums

                _ ->
                    Set.empty
    in
    List.map
        (\number ->
            let
                blockWidth : Int
                blockWidth =
                    Tuple.second (blockSizeToDimensions blockSize)

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
                    (Set.member number errorNumbers)
                    (HA.class "error")
                , HAE.attributeIf
                    (not (Set.isEmpty highlightNumbers) && not (Set.member number highlightNumbers))
                    (HA.class "dimmed")
                ]
                [ Html.text (numberToString number) ]
        )
        (Set.toList numbers)


viewZoomControls : Html Msg
viewZoomControls =
    Html.div
        [ HA.class "zoom-controls"
        ]
        [ Html.button
            [ HA.class "zoom-button"
            , HE.onClick ZoomInPressed
            ]
            [ Html.text "+" ]
        , Html.button
            [ HA.class "zoom-button"
            , HE.onClick ZoomOutPressed
            ]
            [ Html.text "−" ]
        , Html.button
            [ HA.class "zoom-button"
            , HE.onClick ZoomResetPressed
            ]
            [ Html.text "=" ]
        ]


viewInfoPanel : Model -> Html Msg
viewInfoPanel model =
    Html.div
        [ HA.class "info-panel"
        ]
        [ viewInfoPanelInput model
        , viewInfoPanelHelpers model
        , viewInfoPanelItems model
        , viewInfoPanelSelected model
        , viewInfoPanelSettings model
        , viewInfoPanelDebug model
        , viewInfoPanelMessages model
        ]


viewInfoPanelInput : Model -> Html Msg
viewInfoPanelInput model =
    let
        validCellCandidates : Set Int
        validCellCandidates =
            getValidCellCandidates model model.selectedCell
    in
    Html.details
        [ HA.class "info-panel-details"
        , HA.attribute "open" "true"
        ]
        [ Html.summary
            []
            [ Html.text "Input" ]
        , Html.div
            [ HA.class "column gap-l"
            ]
            [ Html.label
                [ HA.class "column gap-s"
                ]
                [ Html.text "Input mode (Toggle [Space], Hold [Shift])"
                , Html.div
                    [ HA.class "row gap-m"
                    ]
                    [ viewRadioButton
                        False
                        (getCandidateMode model)
                        "input-mode"
                        CandidateModeChanged
                        (\_ -> "Number")
                    , viewRadioButton
                        True
                        (getCandidateMode model)
                        "input-mode"
                        CandidateModeChanged
                        (\_ -> "Candidates")
                    ]
                ]
            , Html.div
                [ HA.class "row gap-m"
                , HA.class <| "block-" ++ String.fromInt model.blockSize
                , HA.style "font-size" "1.5em"
                , HA.style "flex-wrap" "wrap"
                ]
                (List.append
                    (List.map
                        (\n ->
                            Html.button
                                [ HE.onClick (NumberPressed n)
                                , HA.class "cell"
                                , HA.class <| "val-" ++ String.fromInt n
                                , HAE.attributeIf
                                    (not <| Set.member n validCellCandidates)
                                    (HA.class "error")
                                ]
                                [ Html.text (numberToString n) ]
                        )
                        (List.range 1 model.blockSize)
                    )
                    [ Html.button
                        [ HE.onClick DeletePressed
                        , HA.class "cell"
                        , HA.style "width" "1.5em"
                        , HA.style "height" "1.5em"
                        ]
                        [ Html.text "×" ]
                    ]
                )
            , Html.div
                [ HA.class "col gap-s" ]
                [ Html.text "Highlight mode [Tab]"
                , Html.div
                    [ HA.class "row gap-m"
                    ]
                    [ viewRadioButton
                        HighlightNone
                        model.highlightMode
                        "highlight-mode"
                        HighlightModeChanged
                        (\_ -> "None")
                    , viewRadioButton
                        HighlightBoard
                        model.highlightMode
                        "highlight-mode"
                        HighlightModeChanged
                        (\_ -> "Board")
                    , viewRadioButton
                        HighlightArea
                        model.highlightMode
                        "highlight-mode"
                        HighlightModeChanged
                        (\_ -> "Area")
                    , viewRadioButton
                        HighlightNumber
                        model.highlightMode
                        "highlight-mode"
                        HighlightModeChanged
                        (\_ -> "Number")
                    ]
                ]
            ]
        ]


viewInfoPanelSelected : Model -> Html Msg
viewInfoPanelSelected model =
    Html.details
        [ HA.class "info-panel-details"
        , HA.attribute "open" "true"
        ]
        [ Html.summary
            []
            [ Html.text "Selected Cell / Hints" ]
        , Html.div
            [ HA.class "column gap-m"
            ]
            (List.concat
                [ [ viewCellInfo model model.selectedCell ]
                , List.map
                    (viewBlockInfo model)
                    (Dict.get model.selectedCell model.cellBlocks
                        |> Maybe.withDefault []
                        |> List.sortBy .startRow
                    )

                , List.map
                    (viewRowInfo model)
                    (Dict.get model.selectedCell model.cellRows
                        |> Maybe.withDefault []
                        |> List.sortBy .startCol
                    )

                , List.map
                    (viewColInfo model)
                    (Dict.get model.selectedCell model.cellCols
                        |> Maybe.withDefault []
                        |> List.sortBy .startRow
                    )

                , List.map
                    (viewBoardInfo model)
                    (Dict.get model.selectedCell model.cellBoards
                        |> Maybe.withDefault []
                        |> List.sortBy .startRow
                    )
                , if model.gameIsLocal then
                    []

                  else
                    [ Html.div
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
                    ]
                ]
            )
        ]


viewInfoPanelHelpers : Model -> Html Msg
viewInfoPanelHelpers model =
    Html.details
        [ HA.class "info-panel-details"
        , HA.attribute "open" "true"
        ]
        [ Html.summary
            []
            [ Html.text "Helpers" ]
        , Html.div
            [ HA.class "column gap-m"
            ]
            [ Html.div
                [ HA.class "row gap-m"
                , HA.style "align-items" "center"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HA.disabled (List.isEmpty model.undoStack)
                    , HE.onClick UndoPressed
                    ]
                    [ Html.text "Undo" ]
                , Html.text "[Z]"
                ]
            , Html.div
                [ HA.class "row gap-m"
                , HA.style "align-items" "center"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HE.onClick SelectSingleCandidateCellPressed
                    ]
                    [ Html.text "Select first single-candidate cell" ]
                , Html.text "[S]"
                ]
            , Html.div
                [ HA.class "row gap-m"
                , HA.style "align-items" "center"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HE.onClick RemoveInvalidCandidatesPressed
                    ]
                    [ Html.text "Remove all invalid candidates" ]
                , Html.text "[W]"
                , Html.label
                    [ HA.class "row gap-s"
                    , HA.style "align-items" "center"
                    ]
                    [ Html.input
                        [ HA.type_ "checkbox"
                        , HA.checked model.autoRemoveInvalidCandidates
                        , HE.onCheck AutoRemoveInvalidCandidatesChanged
                        ]
                        []
                    , Html.text "Auto"
                    ]
                ]
            , Html.div
                [ HA.class "row gap-m"
                , HA.style "align-items" "center"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HE.onClick FillCellCandidatesPressed
                    ]
                    [ Html.text "Add candidates to cell" ]
                , Html.text "[Q]"
                , Html.label
                    [ HA.class "row gap-s"
                    , HA.style "align-items" "center"
                    ]
                    [ Html.input
                        [ HA.type_ "checkbox"
                        , HA.checked model.autoFillCandidatesOnUnlock
                        , HE.onCheck AutoFillCandidatesOnUnlockChanged
                        ]
                        []
                    , Html.text "Auto on Unlock"
                    ]
                ]
            , Html.div
                [ HA.class "row gap-m"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HE.onClick FillBoardCandidatesPressed
                    ]
                    [ Html.text "Add candidates to board" ]
                ]
            , Html.div
                [ HA.class "row gap-m"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HE.onClick ClearBoardPressed
                    ]
                    [ Html.text "Clear board" ]
                ]
            ]
        ]


viewInfoPanelItems : Model -> Html Msg
viewInfoPanelItems model =
    Html.details
        [ HA.class "info-panel-details"
        , HA.attribute "open" "true"
        ]
        [ Html.summary
            []
            [ Html.text "Items" ]
        , Html.div
            [ HA.class "row gap-m"
            , HA.style "flex-wrap" "wrap"
            ]
            [ Html.button
                [ HA.class "button"
                , HAE.attributeIf
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
                [ HA.class "button"
                , HAE.attributeIf
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
            , Html.button
                [ HA.class "button"
                , HAE.attributeIf
                    (model.removeRandomCandidateUses > 0)
                    (HE.onClick RemoveRandomCandidatePressed)
                , HA.disabled (model.removeRandomCandidateUses <= 0)
                ]
                [ Html.text
                    (String.concat
                        [ "Remove Random Candidate ("
                        , String.fromInt model.removeRandomCandidateUses
                        , " uses)"
                        ]
                    )
                ]
            ]
        ]


viewInfoPanelSettings : Model -> Html Msg
viewInfoPanelSettings model =
    Html.details
        [ HA.class "info-panel-details"
        ]
        [ Html.summary
            []
            [ Html.text "Settings" ]
        , Html.div
            [ HA.class "column gap-m"
            , HA.style "align-items" "flex-start"
            ]
            [ Html.label
                [ HA.class "row gap-s" ]
                [ Html.text "Color scheme:"
                , Html.select
                    [ HE.onInput ColorSchemeChanged
                    ]
                    [ Html.option
                        [ HA.value "light dark"
                        , HA.selected (model.colorScheme == "light dark")
                        ]
                        [ Html.text "Browser default" ]
                    , Html.option
                        [ HA.value "light"
                        , HA.selected (model.colorScheme == "light")
                        ]
                        [ Html.text "Light" ]
                    , Html.option
                        [ HA.value "dark"
                        , HA.selected (model.colorScheme == "dark")
                        ]
                        [ Html.text "Dark" ]
                    ]
                ]
            , Html.label
                [ HA.class "row gap-s"
                , HA.style "align-items" "center"
                ]
                [ Html.input
                    [ HA.type_ "checkbox"
                    , HA.checked model.animationsEnabled
                    , HE.onCheck EnableAnimationsChanged
                    ]
                    []
                , Html.text "Enable animations"
                ]
              -- Auto scouting
            ]
        ]


viewInfoPanelDebug : Model -> Html Msg
viewInfoPanelDebug model =
    Html.details
        [ HA.class "info-panel-details"
        ]
        [ Html.summary
            []
            [ Html.text "Debug / Cheats" ]
        , Html.div
            [ HA.class "column gap-m"
            , HA.style "align-items" "flex-start"
            ]
            [ Html.button
                [ HA.class "button"
                , HE.onClick UnlockSelectedBlockPressed
                ]
                [ Html.text "Unlock selected block" ]
            , Html.button
                [ HA.class "button"
                , HE.onClick AddDebugItemsPressed
                ]
                [ Html.text "Add 1000 of each item" ]
            , Html.div
                [ HA.class "row gap-m"
                , HA.style "align-items" "center"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HE.onClick SolveSingleCandidatesPressed
                    ]
                    [ Html.text "Solve single-candidate cells in board" ]
                ]
            ]
        ]


viewInfoPanelMessages : Model -> Html Msg
viewInfoPanelMessages model =
    Html.details
        [ HA.class "info-panel-details"
        , HA.attribute "open" "true"
        , HA.style "flex-grow" "1"
        , HA.style "justify-content" "flex-end"
        ]
        [ Html.summary
            []
            [ Html.text "Messages" ]
        , Html.div
            [ HA.class "column gap-m"
            ]
            [ Html.div
                [ HA.style "max-height" "400px"
                , HA.style "overflow-y" "auto"
                , HA.style "display" "flex"
                , HA.style "flex-direction" "column-reverse"
                , HA.class "gap-m"
                ]
                (List.map viewMessage model.messages)
            , Html.Extra.viewIf
                (not model.gameIsLocal)
                (Html.form
                    [ HA.class "row gap-m"
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
        ]


viewCellInfo : Model -> ( Int, Int ) -> Html Msg
viewCellInfo model ( row, col ) =
    Html.div
        []
        [ viewCellLabel "Cell" row col
        ]


viewBlockInfo : Model -> Area -> Html Msg
viewBlockInfo model block =
    Html.div
        [ HA.class "column"
        ]
        [ viewCellLabel "Block" block.startRow block.startCol
        , viewReward model (cellToBlockId ( block.startRow, block.startCol )) block
        , viewBlockUnlockInfo model block
        ]


viewBlockUnlockInfo : Model -> Area -> Html Msg
viewBlockUnlockInfo model block =
    if model.gameIsLocal
        || Set.member ( block.startRow, block.startCol ) model.unlockedBlocks
        || model.progression /= Shuffled
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
                            , item.senderAlias
                            , ", "
                            , item.gameName
                            , ")"
                            ]
                        )
                    ]

            Nothing ->
                Html.div
                    [ HA.class "row gap-m"
                    ]
                    [ Html.text "Unlock: ???"
                    , if model.progression == Shuffled then
                        Html.button
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

                      else
                        Html.text ""
                    ]


viewRowInfo : Model -> Area -> Html Msg
viewRowInfo model row =
    Html.div
        [ HA.class "column"
        ]
        [ viewCellLabel "Row" row.startRow row.startCol
        , viewReward model (cellToRowId ( row.startRow, row.startCol )) row
        ]


viewColInfo : Model -> Area -> Html Msg
viewColInfo model col =
    Html.div
        [ HA.class "column"
        ]
        [ viewCellLabel "Column" col.startRow col.startCol
        , viewReward model (cellToColId ( col.startRow, col.startCol )) col
        ]


viewBoardInfo : Model -> Area -> Html Msg
viewBoardInfo model board =
    Html.div
        [ HA.class "column"
        ]
        [ viewCellLabel "Board" board.startRow board.startCol
        , viewReward model (cellToBoardId ( board.startRow, board.startCol )) board
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


viewReward : Model -> Int -> Area -> Html Msg
viewReward model id area =
    case Dict.get id model.scoutedItems of
        Just hint ->
            Html.div
                []
                [ Html.text
                    (String.concat
                        [ "Reward: "
                        , hint.itemName
                        , " ("
                        , String.join
                            ", "
                            (List.filter (not << String.isEmpty)
                                [ itemClassToString hint.itemClass
                                , hint.receiverAlias
                                , hint.gameName
                                ]
                            )
                        , ")"
                        ]
                    )
                , if Set.member id model.solvedLocations then
                    Html.text " ✅"

                  else
                    Html.text ""
                ]

        Nothing ->
            Html.div
                [ HA.class "row gap-m"
                , HA.style "align-items" "center"
                ]
                [ Html.text "Reward: ???"
                , if model.locationScouting == ScoutingManual then
                    Html.button
                        [ HA.class "button"
                        , HE.onClick (ScoutLocationPressed id)
                        , HA.disabled
                            (List.any
                                (not << cellIsVisible model)
                                (getAreaCells area)
                            )
                        ]
                        [ Html.text "Scout" ]

                  else
                    Html.text ""
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
