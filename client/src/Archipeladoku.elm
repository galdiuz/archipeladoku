port module Archipeladoku exposing (..)

import Array exposing (Array)
import Bitwise
import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import File.Download
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
import Random.List
import Set exposing (Set)
import Set.Extra
import Task
import Time
import Yaml.Encode


port centerViewOnCell : ( Int, Int ) -> Cmd msg
port checkLocation : Int -> Cmd msg
port connect : Encode.Value -> Cmd msg
port generateBoard : Encode.Value -> Cmd msg
port goal : () -> Cmd msg
port hintForItem : String -> Cmd msg
port log : String -> Cmd msg
port moveCellIntoView : ( Int, Int ) -> Cmd msg
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
    , boardData : Encode.Value
    , boardsPerCluster : Int
    , candidateMode : Bool
    , cellBlocks : Dict ( Int, Int ) (List Area)
    , cellBoards : Dict ( Int, Int ) (List Area)
    , cellCols : Dict ( Int, Int ) (List Area)
    , cellRows : Dict ( Int, Int ) (List Area)
    , colorScheme : String
    , current : Dict ( Int, Int ) CellValue
    , discoTrapMap : Dict Int Int
    , discoTrapRatio : Int
    , discoTrapRatioInput : String
    , discoTrapTimer : Int
    , difficulty : Int
    , emojiTrapMap : Dict Int String
    , emojiTrapRatio : Int
    , emojiTrapRatioInput : String
    , emojiTrapTimer : Int
    , emojiTrapVariant : EmojiTrapVariant
    , errors : Dict ( Int, Int ) CellError
    , generationProgress : ( String, Float )
    , gameIsLocal : Bool
    , gameState : GameState
    , givens : Set ( Int, Int )
    , heldKeys : Set String
    , highlightedCells : Set ( Int, Int )
    , highlightedNumbers : Set Int
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
    , numberOfBoardsInput : String
    , password : String
    , pendingCellChanges : Set ( Int, Int )
    , pendingCheckLocations : Set Int
    , pendingItems : List Item
    , pendingScoutLocations : Set Int
    , pendingSolvedBlocks : Set ( Int, Int )
    , pendingSolvedCols : Set ( Int, Int )
    , pendingSolvedRows : Set ( Int, Int )
    , player : String
    , playerNameOption : String
    , preFillNothingsPercent : Int
    , preFillNothingsPercentInput : String
    , progression : Progression
    , progressionBalancing : Int
    , progressionBalancingInput : String
    , puzzleAreas : PuzzleAreas
    , removeRandomCandidateRatio : Int
    , removeRandomCandidateRatioInput : String
    , removeRandomCandidateUses : Int
    , scoutedItems : Dict Int Hint
    , seed : Random.Seed
    , seedInput : Int
    , selectedCell : ( Int, Int )
    , shiftDebounce : Int
    , solution : Dict ( Int, Int ) Int
    , solveRandomCellRatio : Int
    , solveRandomCellRatioInput : String
    , solveRandomCellUses : Int
    , solveSelectedCellRatio : Int
    , solveSelectedCellRatioInput : String
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
    | CancelTrapsPressed
    | CandidateModeChanged Bool
    | CellSelected ( Int, Int )
    | ClearBoardPressed
    | ColorSchemeChanged String
    | ConnectPressed
    | DeletePressed
    | DifficultyChanged Int
    | DiscoTrapRatioChanged Int
    | DiscoTrapRatioInputBlurred
    | DiscoTrapRatioInputChanged String
    | EmojiTrapRatioChanged Int
    | EmojiTrapRatioInputBlurred
    | EmojiTrapRatioInputChanged String
    | EmojiTrapVariantChanged String
    | EnableAnimationsChanged Bool
    | FillBoardCandidatesPressed
    | FillCellCandidatesPressed
    | GenerateYamlPressed
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
    | LocationScoutingChanged LocationScouting
    | MessageInputChanged String
    | MoveSelectionPressed ( Int, Int )
    | NoOp
    | NumberOfBoardsChanged Int
    | NumberOfBoardsInputBlurred
    | NumberOfBoardsInputChanged String
    | NumberPressed Int
    | PasswordInputChanged String
    | PlayLocalPressed
    | PlayerInputChanged String
    | PlayerNameOptionChanged String
    | PreFillNothingsPercentChanged Int
    | PreFillNothingsPercentInputBlurred
    | PreFillNothingsPercentInputChanged String
    | ProgressionChanged Progression
    | ProgressionBalancingChanged Int
    | ProgressionBalancingInputBlurred
    | ProgressionBalancingInputChanged String
    | RemoveInvalidCandidatesPressed
    | RemoveRandomCandidatePressed
    | RemoveRandomCandidateRatioChanged Int
    | RemoveRandomCandidateRatioInputBlurred
    | RemoveRandomCandidateRatioInputChanged String
    | ScoutLocationPressed Int
    | SecondPassed
    | SeedInputChanged String
    | SelectSingleCandidateCellPressed
    | SelectSolvableBoardPressed
    | SendMessagePressed
    | ShiftDebouncePassed Int
    | ShiftHeld
    | ShiftReleased
    | SolveRandomCellPressed
    | SolveRandomCellRatioChanged Int
    | SolveRandomCellRatioInputBlurred
    | SolveRandomCellRatioInputChanged String
    | SolveSelectedCellPressed
    | SolveSelectedCellRatioChanged Int
    | SolveSelectedCellRatioInputBlurred
    | SolveSelectedCellRatioInputChanged String
    | SolveSingleCandidatesPressed
    | ToggleCandidateModePressed
    | ToggleHighlightModePressed
    | TriggerDiscoTrapPressed
    | TriggerEmojiTrapPressed
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
      , boardData = Encode.null
      , boardsPerCluster = 5
      , candidateMode = False
      , cellBlocks = Dict.empty
      , cellBoards = Dict.empty
      , cellCols = Dict.empty
      , cellRows = Dict.empty
      , colorScheme = "light dark"
      , current = Dict.empty
      , discoTrapMap = Dict.empty
      , discoTrapRatio = 20
      , discoTrapRatioInput = "20"
      , discoTrapTimer = 0
      , difficulty = 2
      , emojiTrapMap = Dict.empty
      , emojiTrapRatio = 20
      , emojiTrapRatioInput = "20"
      , emojiTrapTimer = 0
      , emojiTrapVariant = EmojiTrapRandom
      , errors = Dict.empty
      , generationProgress = ( "", 0 )
      , gameIsLocal = False
      , gameState = MainMenu
      , givens = Set.empty
      , heldKeys = Set.empty
      , highlightedCells = Set.empty
      , highlightedNumbers = Set.empty
      , highlightMode = HighlightNone
      , hints = Dict.empty
      , hintCost = 0
      , hintPoints = 0
      , host = ""
      , locationScouting = ScoutingManual
      , lockedBlocks = []
      , messageInput = ""
      , messages = []
      , numberOfBoards = 5
      , numberOfBoardsInput = "5"
      , password = ""
      , pendingCellChanges = Set.empty
      , pendingCheckLocations = Set.empty
      , pendingItems = []
      , pendingScoutLocations = Set.empty
      , pendingSolvedBlocks = Set.empty
      , pendingSolvedCols = Set.empty
      , pendingSolvedRows = Set.empty
      , player = ""
      , playerNameOption = "Player{number}"
      , preFillNothingsPercent = 50
      , preFillNothingsPercentInput = "50"
      , progression = Shuffled
      , progressionBalancing = 50
      , progressionBalancingInput = "50"
      , puzzleAreas =
            { blocks = []
            , boards = []
            , rows = []
            , cols = []
            }
      , removeRandomCandidateRatio = 300
      , removeRandomCandidateRatioInput = "300"
      , removeRandomCandidateUses = 0
      , scoutedItems = Dict.empty
      , seed = Random.initialSeed (flags.seed + 1)
      , seedInput = flags.seed
      , selectedCell = ( 1, 1 )
      , shiftDebounce = 0
      , solution = Dict.empty
      , solveRandomCellRatio = 150
      , solveRandomCellRatioInput = "150"
      , solveRandomCellUses = 0
      , solveSelectedCellRatio = 100
      , solveSelectedCellRatioInput = "100"
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
        , if model.discoTrapTimer > 0 || model.emojiTrapTimer > 0 then
            Time.every 1000 (\_ -> SecondPassed)

          else
            Sub.none
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
            , setLocalStorage ( "apdk-auto-fill-candidates-on-unlock", if value then "1" else "0" )
            )

        AutoRemoveInvalidCandidatesChanged value ->
            ( { model | autoRemoveInvalidCandidates = value }
            , setLocalStorage ( "apdk-auto-remove-invalid-candidates", if value then "1" else "0" )
            )
                |> andThen updateState

        BlockSizeChanged size ->
            ( { model
                | blockSize = size
                , numberOfBoards = min model.numberOfBoards (maxNumberOfBoards size)
                , numberOfBoardsInput = String.fromInt (min model.numberOfBoards (maxNumberOfBoards size))
              }
            , Cmd.none
            )

        BoardsPerClusterChanged value ->
            ( { model | boardsPerCluster = value }
            , Cmd.none
            )

        CancelTrapsPressed ->
            ( { model
                | discoTrapTimer = 0
                , emojiTrapTimer = 0
              }
            , Cmd.none
            )
                |> andThen updateBoardData

        CandidateModeChanged value ->
            ( { model | candidateMode = value }
            , Cmd.none
            )

        CellSelected ( row, col ) ->
            if Dict.member ( row, col ) model.solution then
                ( { model | selectedCell = ( row, col ) }
                    |> updateHighlight
                , Cmd.none
                )
                    |> andThen updateBoardData

            else
                ( model
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
                |> andThen updateBoardData

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

        DiscoTrapRatioChanged value ->
            ( { model
                | discoTrapRatio = value
                , discoTrapRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        DiscoTrapRatioInputBlurred ->
            let
                value : Int
                value =
                    model.discoTrapRatioInput
                        |> String.toInt
                        |> Maybe.withDefault model.discoTrapRatio
                        |> clamp 0 maxRatio
            in
            ( { model
                | discoTrapRatio = value
                , discoTrapRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        DiscoTrapRatioInputChanged value ->
            ( { model
                | discoTrapRatio =
                    String.toInt value
                        |> Maybe.withDefault model.discoTrapRatio
                , discoTrapRatioInput = value
              }
            , Cmd.none
            )

        EmojiTrapRatioChanged value ->
            ( { model
                | emojiTrapRatio = value
                , emojiTrapRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        EmojiTrapRatioInputBlurred ->
            let
                value : Int
                value =
                    model.emojiTrapRatioInput
                        |> String.toInt
                        |> Maybe.withDefault model.emojiTrapRatio
                        |> clamp 0 maxRatio
            in
            ( { model
                | emojiTrapRatio = value
                , emojiTrapRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        EmojiTrapRatioInputChanged value ->
            ( { model
                | emojiTrapRatio =
                    String.toInt value
                        |> Maybe.withDefault model.emojiTrapRatio
                , emojiTrapRatioInput = value
              }
            , Cmd.none
            )

        EmojiTrapVariantChanged value ->
            ( { model | emojiTrapVariant = emojiTrapVariantFromString value }
            , setLocalStorage ( "apdk-emoji-trap-variant", value )
            )
                |> andThen updateEmojiTrapMap

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
                |> andThen updateBoardData

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
                    |> andThen updateBoardData

            else
                ( model
                , Cmd.none
                )

        GenerateYamlPressed ->
            ( model
            , File.Download.string
                "archipeladoku.yaml"
                "text/yaml"
                (buildOptionsYaml model)
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
                |> updateHighlight
            , Cmd.none
            )
                |> andThen updateBoardData

        HintItemPressed name ->
            ( model
            , hintForItem name
            )

        HostInputChanged value ->
            ( { model | host = value }
            , setLocalStorage ( "apdk-host", value )
            )

        LocationScoutingChanged value ->
            ( { model | locationScouting = value }
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
            ( { model
                | numberOfBoards = value
                , numberOfBoardsInput = String.fromInt value
              }
            , Cmd.none
            )

        NumberOfBoardsInputBlurred ->
            let
                value : Int
                value =
                    model.numberOfBoardsInput
                        |> String.toInt
                        |> Maybe.withDefault model.numberOfBoards
                        |> clamp 0 (maxNumberOfBoards model.blockSize)
            in
            ( { model
                | numberOfBoards = value
                , numberOfBoardsInput = String.fromInt value
              }
            , Cmd.none
            )

        NumberOfBoardsInputChanged value ->
            ( { model
                | numberOfBoards =
                    String.toInt value
                        |> Maybe.withDefault model.numberOfBoards
                , numberOfBoardsInput = value
              }
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
            , setLocalStorage ( "apdk-password", value )
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
                    , discoTrapRatio = model.discoTrapRatio
                    , emojiTrapRatio = model.emojiTrapRatio
                    , numberOfBoards = model.numberOfBoards
                    , progression = model.progression
                    , removeRandomCandidateRatio = model.removeRandomCandidateRatio
                    , seed = model.seedInput
                    , solveRandomCellRatio = model.solveRandomCellRatio
                    , solveSelectedCellRatio = model.solveSelectedCellRatio
                    }
                )
            )

        PlayerInputChanged value ->
            ( { model | player = value }
            , setLocalStorage ( "apdk-player", value )
            )

        PlayerNameOptionChanged value ->
            ( { model | playerNameOption = value }
            , Cmd.none
            )

        PreFillNothingsPercentChanged value ->
            ( { model
                | preFillNothingsPercent = value
                , preFillNothingsPercentInput = String.fromInt value
              }
            , Cmd.none
            )

        PreFillNothingsPercentInputBlurred ->
            let
                value : Int
                value =
                    model.preFillNothingsPercentInput
                        |> String.toInt
                        |> Maybe.withDefault model.preFillNothingsPercent
                        |> clamp 0 100
            in
            ( { model
                | preFillNothingsPercent = value
                , preFillNothingsPercentInput = String.fromInt value
              }
            , Cmd.none
            )

        PreFillNothingsPercentInputChanged value ->
            ( { model
                | preFillNothingsPercent =
                    String.toInt value
                        |> Maybe.withDefault model.preFillNothingsPercent
                , preFillNothingsPercentInput = value
              }
            , Cmd.none
            )

        ProgressionChanged value ->
            ( { model | progression = value }
            , Cmd.none
            )

        ProgressionBalancingChanged value ->
            ( { model
                | progressionBalancing = value
                , progressionBalancingInput = String.fromInt value
              }
            , Cmd.none
            )

        ProgressionBalancingInputBlurred ->
            let
                value : Int
                value =
                    model.progressionBalancingInput
                        |> String.toInt
                        |> Maybe.withDefault model.progressionBalancing
                        |> clamp 0 99
            in
            ( { model
                | progressionBalancing = value
                , progressionBalancingInput = String.fromInt value
              }
            , Cmd.none
            )

        ProgressionBalancingInputChanged value ->
            ( { model
                | progressionBalancing =
                    String.toInt value
                        |> Maybe.withDefault model.progressionBalancing
                , progressionBalancingInput = value
              }
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

        RemoveRandomCandidateRatioChanged value ->
            ( { model
                | removeRandomCandidateRatio = value
                , removeRandomCandidateRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        RemoveRandomCandidateRatioInputBlurred ->
            let
                value : Int
                value =
                    model.removeRandomCandidateRatioInput
                        |> String.toInt
                        |> Maybe.withDefault model.removeRandomCandidateRatio
                        |> clamp 0 maxRatio
            in
            ( { model
                | removeRandomCandidateRatio = value
                , removeRandomCandidateRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        RemoveRandomCandidateRatioInputChanged value ->
            ( { model
                | removeRandomCandidateRatio =
                    String.toInt value
                        |> Maybe.withDefault model.removeRandomCandidateRatio
                , removeRandomCandidateRatioInput = value
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

        SecondPassed ->
            ( { model
                | discoTrapTimer =
                    if model.discoTrapTimer > 0 then
                        model.discoTrapTimer - 1

                    else
                        0
                , emojiTrapTimer =
                    if model.emojiTrapTimer > 0 then
                        model.emojiTrapTimer - 1

                    else
                        0
              }
            , Cmd.none
            )
                |> andThen
                    (\m ->
                        if m.discoTrapTimer > 0 && modBy 2 m.discoTrapTimer == 0 then
                            updateDiscoTrapMap m

                        else
                            updateBoardData m
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
                targetCells : List ( Int, Int )
                targetCells =
                    Dict.foldl
                        (\cell cellValue acc ->
                            case cellValue of
                                Multiple candidates ->
                                    if Set.size candidates == 1
                                        && Set.member cell model.visibleCells
                                        && not (Set.member cell model.givens)
                                    then
                                        cell :: acc

                                    else
                                        acc

                                _ ->
                                    acc
                        )
                        []
                        model.current

                targetCell : Maybe ( Int, Int )
                targetCell =
                    targetCells
                        |> List.sortBy (distanceBetweenCells model.selectedCell)
                        |> List.head
            in
            case targetCell of
                Just cell ->
                    ( { model | selectedCell = cell }
                        |> updateHighlight
                    , moveCellIntoView cell
                    )
                        |> andThen updateBoardData

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        SelectSolvableBoardPressed ->
            let
                targetCell : Maybe ( Int, Int )
                targetCell =
                    model.puzzleAreas.boards
                        |> List.filter
                            (\board ->
                                (&&)
                                    (List.any
                                        (\cell -> not (Set.member cell model.givens))
                                        (getAreaCells board)
                                    )
                                    (List.all
                                        (cellIsVisible model)
                                        (getAreaCells board)
                                    )
                            )
                        |> List.head
                        |> Maybe.map
                            (\board ->
                                ( board.startRow + (board.endRow - board.startRow) // 2
                                , board.startCol + (board.endCol - board.startCol) // 2
                                )
                            )
            in
            case targetCell of
                Just cell ->
                    ( { model | selectedCell = cell }
                        |> updateHighlight
                    , centerViewOnCell cell
                    )
                        |> andThen updateBoardData

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

        SolveRandomCellRatioChanged value ->
            ( { model
                | solveRandomCellRatio = value
                , solveRandomCellRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        SolveRandomCellRatioInputBlurred ->
            let
                value : Int
                value =
                    model.solveRandomCellRatioInput
                        |> String.toInt
                        |> Maybe.withDefault model.solveRandomCellRatio
                        |> clamp 0 maxRatio
            in
            ( { model
                | solveRandomCellRatio = value
                , solveRandomCellRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        SolveRandomCellRatioInputChanged value ->
            ( { model
                | solveRandomCellRatio =
                    String.toInt value
                        |> Maybe.withDefault model.solveRandomCellRatio
                , solveRandomCellRatioInput = value
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

        SolveSelectedCellRatioChanged value ->
            ( { model
                | solveSelectedCellRatio = value
                , solveSelectedCellRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        SolveSelectedCellRatioInputBlurred ->
            let
                value : Int
                value =
                    model.solveSelectedCellRatioInput
                        |> String.toInt
                        |> Maybe.withDefault model.solveSelectedCellRatio
                        |> clamp 0 maxRatio
            in
            ( { model
                | solveSelectedCellRatio = value
                , solveSelectedCellRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        SolveSelectedCellRatioInputChanged value ->
            ( { model
                | solveSelectedCellRatio =
                    String.toInt value
                        |> Maybe.withDefault model.solveSelectedCellRatio
                , solveSelectedCellRatioInput = value
              }
            , Cmd.none
            )

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
                |> updateHighlight
            , Cmd.none
            )
                |> andThen updateBoardData

        TriggerDiscoTrapPressed ->
            ( model
            , Cmd.none
            )
                |> andThen triggerDiscoTrap

        TriggerEmojiTrapPressed ->
            ( model
            , Cmd.none
            )
                |> andThen triggerEmojiTrap

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
                |> andThen updateBoardData

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
    , discoTrapRatio : Int
    , emojiTrapRatio : Int
    , numberOfBoards : Int
    , progression : Progression
    , removeRandomCandidateRatio : Int
    , seed : Int
    , solveRandomCellRatio : Int
    , solveSelectedCellRatio : Int
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
    = CandidateErrors (Dict Int (Set ( Int, Int )))
    | NumberError (Set ( Int, Int ))


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
    | DiscoTrap
    | EmojiTrap
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


type EmojiTrapVariant
    = EmojiTrapAnimals
    | EmojiTrapFruits
    | EmojiTrapRandom


---
-- Encoding/decoding
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
                        , ( "KeyG", SelectSolvableBoardPressed )
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


encodeTuple : (a -> Encode.Value) -> (b -> Encode.Value) -> ( a, b ) -> Encode.Value
encodeTuple encodeA encodeB ( a, b ) =
    Encode.list identity
        [ encodeA a
        , encodeB b
        ]


encodeGenerateArgs : GenerateArgs -> Encode.Value
encodeGenerateArgs args =
    Encode.object
        [ ( "blockSize", Encode.int args.blockSize )
        , ( "boardsPerCluster", Encode.int args.boardsPerCluster )
        , ( "difficulty", Encode.int args.difficulty )
        , ( "discoTrapRatio", Encode.int args.discoTrapRatio )
        , ( "emojiTrapRatio", Encode.int args.emojiTrapRatio )
        , ( "numberOfBoards", Encode.int args.numberOfBoards )
        , ( "progression", encodeProgression args.progression )
        , ( "removeRandomCandidateRatio", Encode.int args.removeRandomCandidateRatio )
        , ( "seed", Encode.int args.seed )
        , ( "solveRandomCellRatio", Encode.int args.solveRandomCellRatio )
        , ( "solveSelectedCellRatio", Encode.int args.solveSelectedCellRatio )
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
        [ ( "cells", Encode.list (encodeTuple Encode.int Encode.int) cells )
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


buildOptionsYaml : Model -> String
buildOptionsYaml model =
    Yaml.Encode.toString 4
        (Yaml.Encode.record
            [ ( "name", Yaml.Encode.string model.playerNameOption )
            , ( "description", Yaml.Encode.string "Archipeladoku options generated from client." )
            , ( "game", Yaml.Encode.string "Archipeladoku" )
            , ( "requires"
              , Yaml.Encode.record
                    [ ( "version", Yaml.Encode.string "0.6.3" )
                    ]
              )
            , ( "Archipeladoku"
              , Yaml.Encode.record
                    [ ( "progression_balancing", yamlRecordValue <| String.fromInt model.progressionBalancing )
                    , ( "accessibility", yamlRecordValue "full" )
                    , ( "block_size", yamlRecordValue <| String.fromInt model.blockSize )
                    , ( "boards_per_cluster", yamlRecordValue <| String.fromInt model.boardsPerCluster )
                    , ( "number_of_boards", yamlRecordValue <| String.fromInt model.numberOfBoards )
                    , ( "difficulty", yamlRecordValue <| difficultyToString model.difficulty )
                    , ( "progression", yamlRecordValue <| progressionToString model.progression )
                    , ( "location_scouting", yamlRecordValue <| locationScoutingToString model.locationScouting )
                    , ( "solve_selected_cell_ratio", yamlRecordValue <| String.fromInt model.solveSelectedCellRatio )
                    , ( "solve_random_cell_ratio", yamlRecordValue <| String.fromInt model.solveRandomCellRatio )
                    , ( "remove_random_candidate_ratio", yamlRecordValue <| String.fromInt model.removeRandomCandidateRatio )
                    , ( "emoji_trap_ratio", yamlRecordValue <| String.fromInt model.emojiTrapRatio )
                    , ( "pre_fill_nothings_percent", yamlRecordValue <| String.fromInt model.preFillNothingsPercent )
                    , ( "local_items", Yaml.Encode.list Yaml.Encode.string [] )
                    , ( "non_local_items", Yaml.Encode.list Yaml.Encode.string [] )
                    , ( "start_inventory", Yaml.Encode.record [] )
                    , ( "start_hints", Yaml.Encode.list Yaml.Encode.string [] )
                    , ( "start_location_hints", Yaml.Encode.list Yaml.Encode.string [] )
                    , ( "exclude_locations", Yaml.Encode.list Yaml.Encode.string [] )
                    , ( "priority_locations", Yaml.Encode.list Yaml.Encode.string [] )
                    , ( "item_links", Yaml.Encode.list Yaml.Encode.string [] )
                    , ( "plando_items", Yaml.Encode.list Yaml.Encode.string [] )
                    ]
              )
            ]
        )


yamlRecordValue : String -> Yaml.Encode.Encoder
yamlRecordValue value =
    Yaml.Encode.record
        [ ( value, Yaml.Encode.int 50 )
        ]


difficultyToString : Int -> String
difficultyToString difficulty =
    case difficulty of
        1 ->
            "beginner"

        2 ->
            "easy"

        3 ->
            "medium"

        4 ->
            "hard"

        _ ->
            "unknown"


progressionToString : Progression -> String
progressionToString progression =
    case progression of
        Fixed ->
            "fixed"

        Shuffled ->
            "shuffled"


locationScoutingToString : LocationScouting -> String
locationScoutingToString locationScouting =
    case locationScouting of
        ScoutingAuto ->
            "auto"

        ScoutingManual ->
            "manual"

        ScoutingDisabled ->
            "disabled"


encodeData : Model -> Encode.Value
encodeData model =
    Encode.object
        [ ( "cells"
          , Encode.list
                identity
                (List.map
                    (\( row, col ) ->
                        Encode.list
                            identity
                            [ Encode.int row
                            , Encode.int col
                            , encodeCellValue model ( row, col )
                            ]
                    )
                    (Dict.keys model.solution)
                )
          )
        , ( "blocks"
          , Encode.list
                identity
                (List.map
                    (\block ->
                        Encode.object
                            [ ( "startRow", Encode.int block.startRow )
                            , ( "startCol", Encode.int block.startCol )
                            , ( "endRow", Encode.int block.endRow )
                            , ( "endCol", Encode.int block.endCol )
                            ]
                    )
                    model.puzzleAreas.blocks
                )
          )
        , ( "boards"
          , Encode.list
                identity
                (List.map
                    (\board ->
                        Encode.object
                            [ ( "startRow", Encode.int board.startRow )
                            , ( "startCol", Encode.int board.startCol )
                            , ( "endRow", Encode.int board.endRow )
                            , ( "endCol", Encode.int board.endCol )
                            ]
                    )
                    model.puzzleAreas.boards
                )
          )
        , ( "errors", encodeErrors model.errors )
        , ( "selectedCell", encodeSelectedCell model.selectedCell )
        , ( "blockSize", Encode.int model.blockSize )
        , ( "colorMap", encodeColorMap model )
        , ( "numberMap", encodeNumberMap model)
        , ( "colorScheme", Encode.string model.colorScheme )
        , ( "discoTrap", Encode.bool (model.discoTrapTimer > 0) )
        ]


cellSelectedDecoder : Decode.Decoder Msg
cellSelectedDecoder =
    Decode.map2
        (\row col -> CellSelected ( row, col ) )
        (Decode.at [ "detail", "row" ] Decode.int)
        (Decode.at [ "detail" ,"col" ] Decode.int)


encodeSelectedCell : ( Int, Int ) -> Encode.Value
encodeSelectedCell ( row, col ) =
    Encode.object
        [ ( "row", Encode.int row )
        , ( "col", Encode.int col )
        ]


encodeCellValue : Model -> ( Int, Int ) -> Encode.Value
encodeCellValue model cell =
    if cellIsVisible model cell then
        case getCellValue model cell of
            Just ( Given n ) ->
                Encode.object
                    [ ( "type", Encode.string "given" )
                    , ( "number", Encode.int n )
                    , ( "dimmed"
                      , case model.highlightMode of
                            HighlightNone ->
                                Encode.bool False

                            HighlightBoard ->
                                Set.member cell model.highlightedCells
                                    |> not
                                    |> Encode.bool

                            HighlightArea ->
                                Set.member cell model.highlightedCells
                                    |> not
                                    |> Encode.bool

                            HighlightNumber ->
                                Set.member n model.highlightedNumbers
                                    |> not
                                    |> xor (Set.isEmpty model.highlightedNumbers)
                                    |> Encode.bool
                      )
                    ]

            Just ( Single n ) ->
                Encode.object
                    [ ( "type", Encode.string "single" )
                    , ( "number", Encode.int n )
                    , ( "dimmed"
                      , case model.highlightMode of
                            HighlightNone ->
                                Encode.bool False

                            HighlightBoard ->
                                Set.member cell model.highlightedCells
                                    |> not
                                    |> Encode.bool

                            HighlightArea ->
                                Set.member cell model.highlightedCells
                                    |> not
                                    |> Encode.bool

                            HighlightNumber ->
                                Set.member n model.highlightedNumbers
                                    |> not
                                    |> xor (Set.isEmpty model.highlightedNumbers)
                                    |> Encode.bool
                      )
                    ]

            Just ( Multiple nums ) ->
                Encode.object
                    [ ( "type", Encode.string "candidates" )
                    , ( "numbers", Encode.list Encode.int (Set.toList nums) )
                    , ( "dimmed"
                      , case model.highlightMode of
                            HighlightNone ->
                                Encode.bool False

                            HighlightBoard ->
                                Set.member cell model.highlightedCells
                                    |> not
                                    |> Encode.bool

                            HighlightArea ->
                                Set.member cell model.highlightedCells
                                    |> not
                                    |> Encode.bool

                            HighlightNumber ->
                                Set.intersect nums model.highlightedNumbers
                                    |> Set.isEmpty
                                    |> xor (Set.isEmpty model.highlightedNumbers)
                                    |> Encode.bool
                      )
                    , ( "dimmedNumbers"
                      , case model.highlightMode of
                            HighlightNumber ->
                                Encode.list
                                    Encode.int
                                    (Set.toList (Set.diff nums model.highlightedNumbers))

                            _ ->
                                Encode.list Encode.int []
                      )
                    ]

            Nothing ->
                Encode.object
                    [ ( "type", Encode.string "empty" )
                    , ( "dimmed"
                      , case model.highlightMode of
                            HighlightNone ->
                                Encode.bool False

                            HighlightBoard ->
                                Set.member cell model.highlightedCells
                                    |> not
                                    |> Encode.bool

                            HighlightArea ->
                                Set.member cell model.highlightedCells
                                    |> not
                                    |> Encode.bool

                            HighlightNumber ->
                                Set.isEmpty model.highlightedNumbers
                                    |> not
                                    |> Encode.bool
                      )
                    ]

    else
        Encode.object
            [ ( "type", Encode.string "hidden" )
            , ( "dimmed"
              , case model.highlightMode of
                    HighlightNone ->
                        Encode.bool False

                    HighlightBoard ->
                        Set.member cell model.highlightedCells
                            |> not
                            |> Encode.bool

                    HighlightArea ->
                        Set.member cell model.highlightedCells
                            |> not
                            |> Encode.bool

                    HighlightNumber ->
                        Set.isEmpty model.highlightedNumbers
                            |> not
                            |> Encode.bool
              )
            ]


encodeErrors : Dict ( Int, Int ) CellError -> Encode.Value
encodeErrors errorsDict =
    Encode.list
        (\( ( row, col ), cellError ) ->
            Encode.object
                [ ( "row", Encode.int row )
                , ( "col", Encode.int col )
                , ( "details"
                  , case cellError of
                        CandidateErrors errors ->
                            Encode.object
                                [ ( "type", Encode.string "candidates" )
                                , ( "errors"
                                  , Encode.list
                                        (\( n, cells ) ->
                                            Encode.object
                                                [ ( "number", Encode.int n )
                                                , ( "cells"
                                                  , Encode.list
                                                        (encodeTuple Encode.int Encode.int)
                                                        (Set.toList cells)
                                                  )
                                                ]
                                        )
                                        (Dict.toList errors)
                                  )
                                ]

                        NumberError cells ->
                            Encode.object
                                [ ( "type", Encode.string "number" )
                                , ( "cells"
                                  , Encode.list
                                        (encodeTuple Encode.int Encode.int)
                                        (Set.toList cells)
                                  )
                                ]
                  )
                ]
        )
        (Dict.toList errorsDict)


encodeColorMap : Model -> Encode.Value
encodeColorMap model =
    if model.discoTrapTimer > 0 then
        Encode.list
            (encodeTuple Encode.int Encode.int)
            (List.map
                identity
                (Dict.toList model.discoTrapMap)
            )

    else
        Encode.list Encode.int []


encodeNumberMap : Model -> Encode.Value
encodeNumberMap model =
    if model.emojiTrapTimer > 0 then
        Encode.list
            (encodeTuple Encode.int Encode.string)
            (List.map
                identity
                (Dict.toList model.emojiTrapMap)
            )

    else
        Encode.list Encode.int []





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

        "apdk-auto-fill-candidates-on-unlock" ->
            ( { model | autoFillCandidatesOnUnlock = value == "1" }
            , Cmd.none
            )

        "apdk-auto-remove-invalid-candidates" ->
            ( { model | autoRemoveInvalidCandidates = value == "1" }
            , Cmd.none
            )

        "apdk-color-scheme" ->
            ( { model | colorScheme = value }
            , Cmd.none
            )

        "apdk-emoji-trap-variant" ->
            ( { model | emojiTrapVariant = emojiTrapVariantFromString value }
            , Cmd.none
            )

        "apdk-host" ->
            ( { model | host = value }
            , Cmd.none
            )

        "apdk-password" ->
            ( { model | password = value }
            , Cmd.none
            )

        "apdk-player" ->
            ( { model | player = value }
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
        |> updateHighlight
    , Cmd.batch
        [ moveCellIntoView newCell
        , Browser.Dom.focus (cellHtmlId newCell)
            |> Task.attempt (always NoOp)
        ]
    )
        |> andThen updateBoardData


updateHighlight : Model -> Model
updateHighlight model =
    case model.highlightMode of
        HighlightNone ->
            { model
                | highlightedCells = Set.empty
                , highlightedNumbers = Set.empty
            }

        HighlightBoard ->
            { model
                | highlightedCells =
                    Dict.get model.selectedCell model.cellBoards
                        |> Maybe.withDefault []
                        |> List.concatMap getAreaCells
                        |> Set.fromList
                , highlightedNumbers = Set.empty
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
                , highlightedNumbers = Set.empty
            }

        HighlightNumber ->
            { model
                | highlightedCells = Set.empty
                , highlightedNumbers =
                    if not (cellIsVisible model model.selectedCell) then
                        Set.empty

                    else
                        case getCellValue model model.selectedCell of
                            Just (Given number) ->
                                Set.singleton number

                            Just (Single number) ->
                                Set.singleton number

                            Just (Multiple numbers) ->
                                numbers

                            _ ->
                                Set.empty
            }


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
                                (Set.diff values (Set.fromList <| Dict.keys errorNumbers)
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


applySteps : List (Model -> ( Model, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
applySteps steps initialModel =
    List.foldl
        (\step ( currentModel, currentCmd ) ->
            let
                ( nextModel, nextCmd ) =
                    step currentModel
            in
            (nextModel, Cmd.batch [ currentCmd, nextCmd ])
        )
        ( initialModel, Cmd.none )
        steps


applyList : (a -> Model -> ( Model, Cmd Msg )) -> List a -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applyList updateFn list ( initialModel, initialCmd ) =
    let
        ( finalModel, finalCmds ) =
            List.foldl
                (\item ( currentModel, currentCmds ) ->
                    let
                        ( nextModel, nextCmd ) =
                            updateFn item currentModel
                    in
                    ( nextModel, nextCmd :: currentCmds )
                )
                ( initialModel, [ initialCmd ] )
                list
    in
    ( finalModel
    , Cmd.batch finalCmds
    )


applySet : (a -> Model -> ( Model, Cmd Msg )) -> Set a -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applySet updateFn set ( initialModel, initialCmd ) =
    let
        ( finalModel, finalCmds ) =
            Set.foldl
                (\item ( currentModel, currentCmds ) ->
                    let
                        ( nextModel, nextCmd ) =
                            updateFn item currentModel
                    in
                    ( nextModel, nextCmd :: currentCmds )
                )
                ( initialModel, [ initialCmd ] )
                set
    in
    ( finalModel
    , Cmd.batch finalCmds
    )


updateState : Model -> ( Model, Cmd Msg )
updateState model =
    case model.gameState of
        Playing ->
            if model.autoRemoveInvalidCandidates then
                applySteps
                    [ updateStateChanges
                    , updateStateErrors
                    , updateStateRemoveInvalidCandidates
                    , updateStateErrors
                    , updateStateScoutLocations
                    , updateStateGoal
                    , updateStateHighlight
                    , updateBoardData
                    ]
                    model

            else
                applySteps
                    [ updateStateChanges
                    , updateStateErrors
                    , updateStateScoutLocations
                    , updateStateGoal
                    , updateStateHighlight
                    , updateBoardData
                    ]
                    model

        _ ->
            ( model, Cmd.none )


updateStateItems : Model -> ( Model, Cmd Msg )
updateStateItems model =
    ( { model | pendingItems = [] }
    , Cmd.none
    )
        |> applyList updateStateItem model.pendingItems


updateStateChanges : Model -> ( Model, Cmd Msg )
updateStateChanges model =
    updateStateChangesLoop ( model, Cmd.none )


updateStateChangesLoop : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateStateChangesLoop ( model, cmd ) =
    if Set.isEmpty model.pendingCellChanges && List.isEmpty model.pendingItems then
        ( model, cmd )

    else
        let
            ( newModel, newCmd ) =
                applySteps
                    [ updateStateItems
                    , updateStateCellChanges
                    , updateStateSolvedBlocks
                    , updateStateCheckLocations
                    ]
                    model
        in
        updateStateChangesLoop
            ( newModel
            , Cmd.batch [ cmd, newCmd ]
            )


updateStateCellChanges : Model -> ( Model, Cmd Msg )
updateStateCellChanges model =
    ( { model | pendingCellChanges = Set.empty }
    , Cmd.none
    )
        |> applySet updateStateCellChange model.pendingCellChanges


updateStateSolvedBlocks : Model -> ( Model, Cmd Msg )
updateStateSolvedBlocks model =
    ( { model
        | pendingSolvedBlocks = Set.empty
        , pendingSolvedCols = Set.empty
        , pendingSolvedRows = Set.empty
      }
    , Cmd.none
    )
        |> applySet (updateStateSolvedArea model.cellBlocks cellToBlockId) model.pendingSolvedBlocks
        |> applySet (updateStateSolvedArea model.cellRows cellToRowId) model.pendingSolvedRows
        |> applySet (updateStateSolvedArea model.cellCols cellToColId) model.pendingSolvedCols


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
    ( { model | pendingCheckLocations = Set.empty }
    , Cmd.none
    )
        |> applySet updateStateCheckLocation model.pendingCheckLocations


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


updateStateGoal : Model -> ( Model, Cmd Msg )
updateStateGoal model =
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


updateStateHighlight : Model -> ( Model, Cmd Msg )
updateStateHighlight model =
    ( updateHighlight model
    , Cmd.none
    )


updateBoardData : Model -> ( Model, Cmd Msg )
updateBoardData model =
    ( { model | boardData = encodeData model }
    , Cmd.none
    )


updateStateCellChange : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
updateStateCellChange updatedCell initialModel =
    ( initialModel
    , Cmd.none
    )
        |> applyList
            (\block model ->
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
            (Dict.get updatedCell initialModel.cellBlocks
                |> Maybe.withDefault []
            )
        |> applyList
            (\row model ->
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
            (Dict.get updatedCell initialModel.cellRows
                |> Maybe.withDefault []
            )
        |> applyList
            (\col model ->
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
            (Dict.get updatedCell initialModel.cellCols
                |> Maybe.withDefault []
            )
        |> applyList
            (\board model ->
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
            (Dict.get updatedCell initialModel.cellBoards
                |> Maybe.withDefault []
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
                        getConflictingCells : Int -> Set ( Int, Int )
                        getConflictingCells number =
                            areaCells
                                |> List.filter ((/=) cell)
                                |> List.filter (\areaCell -> Set.member areaCell model.visibleCells)
                                |> List.filter
                                    (\areaCell ->
                                        getCellValue model areaCell
                                            |> Maybe.andThen cellValueToInt
                                            |> Maybe.map ((==) number)
                                            |> Maybe.withDefault False
                                    )
                                |> Set.fromList
                    in
                    case getCellValue model cell of
                        Just (Given v) ->
                            acc

                        Just (Single v) ->
                            let
                                conflictingCells : Set ( Int, Int )
                                conflictingCells =
                                    getConflictingCells v
                            in
                            if Set.isEmpty conflictingCells then
                                acc

                            else
                                Dict.update
                                    cell
                                    (\error ->
                                        case error of
                                            Just (NumberError existingConflicts) ->
                                                Just <| NumberError <| Set.union existingConflicts conflictingCells

                                            _ ->
                                                Just <| NumberError conflictingCells
                                    )
                                    acc

                        Just (Multiple numbers) ->
                            let
                                candidateErrors : Dict Int (Set ( Int, Int ))
                                candidateErrors =
                                    numbers
                                        |> Set.toList
                                        |> List.filterMap
                                            (\number ->
                                                let
                                                    conflictingCells : Set ( Int, Int )
                                                    conflictingCells =
                                                        getConflictingCells number
                                                in
                                                if Set.isEmpty conflictingCells then
                                                    Nothing

                                                else
                                                    Just ( number, conflictingCells )
                                            )
                                        |> Dict.fromList
                            in
                            if Dict.isEmpty candidateErrors then
                                acc

                            else
                                Dict.update
                                    cell
                                    (\error ->
                                        case error of
                                            Just (CandidateErrors existingErrors) ->
                                                Dict.merge
                                                    (\k a -> Dict.insert k a)
                                                    (\k a b -> Dict.insert k (Set.union a b))
                                                    (\k b -> Dict.insert k b)
                                                    existingErrors
                                                    candidateErrors
                                                    Dict.empty
                                                    |> CandidateErrors
                                                    |> Just

                                            _ ->
                                                Just <| CandidateErrors candidateErrors
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

        DiscoTrap ->
            ( { model
                | messages =
                    if model.gameIsLocal then
                        addLocalMessage "Unlocked a Disco Trap." model.messages

                    else
                        model.messages
              }
            , Cmd.none
            )
                |> andThen triggerDiscoTrap

        EmojiTrap ->
            ( { model
                | messages =
                    if model.gameIsLocal then
                        addLocalMessage "Unlocked an Emoji Trap." model.messages

                    else
                        model.messages
              }
            , Cmd.none
            )
                |> andThen triggerEmojiTrap

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


numberToString : Model -> Int -> String
numberToString model number =
    if model.emojiTrapTimer > 0 && Dict.member number model.emojiTrapMap then
        Dict.get number model.emojiTrapMap
            |> Maybe.withDefault ""

    else if number < 10 then
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


distanceBetweenCells : ( Int, Int ) -> ( Int, Int ) -> Float
distanceBetweenCells ( row1, col1 ) ( row2, col2 ) =
    List.sum
        [ abs (row1 - row2) ^ 2
        , abs (col1 - col2) ^ 2
        ]
        |> toFloat
        |> sqrt


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


maxRatio : Int
maxRatio =
    5000


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

    else if id == 401 then
        EmojiTrap

    else if id == 402 then
        DiscoTrap

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

        DiscoTrap ->
            "Disco Trap"

        EmojiTrap ->
            "Emoji Trap"

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

        DiscoTrap ->
            Trap

        EmojiTrap ->
            Trap

        NothingItem ->
            Filler


trapDuration : Int
trapDuration =
    60


triggerDiscoTrap : Model -> ( Model, Cmd Msg )
triggerDiscoTrap model =
    ( { model
        | discoTrapTimer = trapDuration
      }
    , Cmd.none
    )
        |> andThen updateDiscoTrapMap


updateDiscoTrapMap : Model -> ( Model, Cmd Msg )
updateDiscoTrapMap model =
    let
        ( discoTrapMap, newSeed ) =
            Random.step
                (List.range 1 model.blockSize
                    |> Random.List.shuffle
                    |> Random.map
                        (List.indexedMap
                            (\idx number ->
                                ( idx + 1, number )
                            )
                            >> Dict.fromList
                        )
                )
                model.seed
    in
    ( { model
        | discoTrapMap = discoTrapMap
        , seed = newSeed
      }
    , Cmd.none
    )
        |> andThen updateBoardData


triggerEmojiTrap : Model -> ( Model, Cmd Msg )
triggerEmojiTrap model =
    ( { model
        | emojiTrapTimer = trapDuration
      }
    , Cmd.none
    )
        |> andThen updateEmojiTrapMap


updateEmojiTrapMap : Model -> ( Model, Cmd Msg )
updateEmojiTrapMap model =
    let
        emojiSetGenerator : Random.Generator (List String)
        emojiSetGenerator =
            case model.emojiTrapVariant of
                EmojiTrapAnimals ->
                    Random.uniform animalEmojis []

                EmojiTrapFruits ->
                    Random.uniform fruitEmojis []

                EmojiTrapRandom ->
                    Random.uniform animalEmojis [ fruitEmojis ]

        ( emojiTrapMap, newSeed ) =
            Random.step
                (emojiSetGenerator
                    |> Random.andThen (Random.List.choices model.blockSize)
                    |> Random.map
                        (Tuple.first
                            >> List.indexedMap
                                (\idx emoji ->
                                    ( idx + 1, emoji )
                                )
                            >> Dict.fromList
                        )
                )
                model.seed
    in
    ( { model
        | emojiTrapMap = emojiTrapMap
        , seed = newSeed
      }
    , Cmd.none
    )
        |> andThen updateBoardData


animalEmojis : List String
animalEmojis =
    [ "🐶", "🐱", "🐭", "🐹", "🐰", "🦊", "🐻", "🐼", "🐨", "🐯"
    , "🦁", "🐮", "🐷", "🐸", "🐵", "🦄", "🐔", "🐧", "🐦", "🐤"
    , "🦉", "🦇", "🐺", "🐗", "🐴", "🦓", "🦍", "🦧", "🐘", "🦛"
    , "🦏", "🐪", "🦒", "🦘", "🦥", "🦨", "🦡", "🐝", "🦎", "🦀"
    , "🦋", "🐌", "🐞", "🐢", "🐍", "🐑", "🐊", "🐙"
    ]


fruitEmojis : List String
fruitEmojis =
    [ "🍎", "🍊", "🍌", "🍉", "🍇", "🍓", "🍒", "🍍", "🥭", "🥝"
    , "🥑", "🥥", "🍐", "🍋", "🍈", "🍏", "🍑", "🍅", "🍆"
    ]


emojiTrapVariantToString : EmojiTrapVariant -> String
emojiTrapVariantToString variant =
    case variant of
        EmojiTrapAnimals ->
            "animals"

        EmojiTrapFruits ->
            "fruits"

        EmojiTrapRandom ->
            "random"


emojiTrapVariantFromString : String -> EmojiTrapVariant
emojiTrapVariantFromString str =
    case str of
        "animals" ->
            EmojiTrapAnimals

        "fruits" ->
            EmojiTrapFruits

        _ ->
            EmojiTrapRandom


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
        [ HA.class "main-menu"
        ]
        [ Html.h1
            []
            [ Html.text "Archipeladoku" ]
        , viewMenuConnect model
        , viewMenuOptions model
        ]


viewMenuConnect : Model -> Html Msg
viewMenuConnect model =
    Html.div
        [ HA.class "main-menu-panel"
        ]
        [ Html.h2
            []
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
                    [ HA.class "input"
                    , HA.type_ "text"
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
                    [ HA.class "input"
                    , HA.type_ "text"
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
                    [ HA.class "input"
                    , HA.type_ "text"
                    , HA.placeholder "Leave blank if no password"
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



viewMenuOptions : Model -> Html Msg
viewMenuOptions model =
    Html.div
        [ HA.class "main-menu-panel"
        ]
        [ Html.h2
            []
            [ Html.text "Play Local Game / Generate YAML" ]
        , Html.div
            [ HA.style "display" "grid"
            , HA.style "grid-template-columns" "repeat(auto-fit, minmax(300px, 1fr))"
            , HA.style "gap" "var(--spacing-l)"
            ]
            [ viewMenuOptionsBoard model
            , viewMenuOptionsFiller model
            , viewMenuOptionsArchipelago model
            , viewMenuOptionsLocalPlay model
            ]
        , Html.div
            [ HA.style "display" "grid"
            , HA.style "grid-template-columns" "repeat(auto-fit, minmax(300px, 1fr))"
            , HA.style "gap" "var(--spacing-l)"
            ]
            [ Html.button
                [ HA.class "button"
                , HE.onClick GenerateYamlPressed
                ]
                [ Html.text "Generate YAML" ]
            , Html.button
                [ HA.class "button"
                , HE.onClick PlayLocalPressed
                ]
                [ Html.text "Play Local Game" ]
            ]
        ]


viewMenuOptionsBoard : Model -> Html Msg
viewMenuOptionsBoard model =
    Html.details
        [ HA.class "info-panel-details"
        , HA.attribute "open" "true"
        ]
        [ Html.summary
            []
            [ Html.text "Board Options" ]
        , Html.div
            [ HA.class "column gap-m"
            ]
            [ Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    ]
                    [ Html.text "Block Size:"
                    , viewOptionHint
                        "block-size-hint"
                        "The size of each block, and the width/height of each board. A standard Sudoku is 9."
                    ]
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
                [ Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    ]
                    [ Html.text "Boards per Cluster: "
                    , viewOptionHint
                        "boards-per-cluster-hint"
                        "How many boards to put in each cluster of overlapping boards. 1 disables clustering."
                    ]
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
                [ Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    ]
                    [ Html.text "Number of Boards:"
                    , viewOptionHint
                        "number-of-boards-hint"
                        "The total number of boards in the puzzle."
                    ]
                , Html.div
                    [ HA.class "row gap-s"
                    , HA.style "align-items" "center"
                    ]
                    [ Html.input
                        [ HA.class "input"
                        , HA.type_ "number"
                        , HA.style "width" "3em"
                        , HA.min "1"
                        , HA.max (String.fromInt <| maxNumberOfBoards model.blockSize)
                        , HA.value model.numberOfBoardsInput
                        , HE.onBlur NumberOfBoardsInputBlurred
                        , HE.onInput NumberOfBoardsInputChanged
                        ]
                        []
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
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    ]
                    [ Html.text "Difficulty:"
                    , viewOptionHint
                        "difficulty-hint"
                        (String.join
                            "\n"
                            [ "The overall difficulty level. Potential solving techniques required:"
                            , " - Beginner: Naked/hidden singles."
                            , " - Easy: Pointing pairs, box line reduction."
                            , " - Medium: Naked pairs/triples."
                            , " - Hard: Hidden pairs/triples."
                            ]
                        )
                    ]
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
                [ Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    ]
                    [ Html.text "Progression:"
                    , viewOptionHint
                        "progression-style-hint"
                        (String.join
                            "\n"
                            [  "How blocks are unlocked during the game:"
                            ,  " - Fixed: Blocks are unlocked by progressive block items in a fixed order. Smoother progression."
                            ,  " - Shuffled: Blocks are unlocked by specific block items. More chaotic progression."
                            ]
                        )
                    ]
                , Html.div
                    [ HA.class "row gap-m wrap"
                    ]
                    [ viewRadioButton Shuffled model.progression "progression" ProgressionChanged (\_ -> "Shuffled")
                    , viewRadioButton Fixed model.progression "progression" ProgressionChanged (\_ -> "Fixed")
                    ]
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    ]
                    [ Html.text "Location Scouting:"
                    , viewOptionHint
                        "location-scouting-hint"
                        (String.join
                            "\n"
                            [  "How scouting of locations (creating a hint) are handled:"
                            ,  " - Auto: Locations are automatically scouted when fully reavealed."
                            ,  " - Manual: Locations can be scouted when fully revealed by pressing a button."
                            ,  " - Disabled: Locations cannot be scouted."
                            ]
                        )
                    ]
                , Html.div
                    [ HA.class "row gap-m wrap"
                    ]
                    [ viewRadioButton
                        ScoutingAuto
                        model.locationScouting
                        "location-scouting"
                        LocationScoutingChanged
                        (\_ -> "Auto")
                    , viewRadioButton
                        ScoutingManual
                        model.locationScouting
                        "location-scouting"
                        LocationScoutingChanged
                        (\_ -> "Manual")
                    , viewRadioButton
                        ScoutingDisabled
                        model.locationScouting
                        "location-scouting"
                        LocationScoutingChanged
                        (\_ -> "Disabled")
                    ]
                ]
            ]
        ]


viewMenuOptionsFiller : Model -> Html Msg
viewMenuOptionsFiller model =
    Html.details
        [ HA.class "info-panel-details"
        , HA.attribute "open" "true"
        ]
        [ Html.summary
            []
            [ Html.text "Filler Options" ]
        , Html.div
            [ HA.class "column gap-m"
            ]
            [ Html.datalist
                [ HA.id "filler-ratio-ticks"
                ]
                (List.map
                    (\tick ->
                        Html.option
                            [ HA.value (String.fromInt tick) ]
                            []
                    )
                    (List.range 0 10
                        |> List.map ((*) 50)
                    )
                )
            , viewRatioInputs
                { label = "Solve Selected Cell Ratio:"
                , hint = "Ratio of Solve Selected Cell items to number of boards."
                , hintId = "solve-selected-cell-ratio-hint"
                , value = model.solveSelectedCellRatio
                , inputValue = model.solveSelectedCellRatioInput
                , onBlur = SolveSelectedCellRatioInputBlurred
                , onInput = SolveSelectedCellRatioInputChanged
                , onRangeChange = SolveSelectedCellRatioChanged
                }
            , viewRatioInputs
                { label = "Solve Random Cell Ratio:"
                , hint = "Ratio of Solve Random Cell items to number of boards."
                , hintId = "solve-random-cell-ratio-hint"
                , value = model.solveRandomCellRatio
                , inputValue = model.solveRandomCellRatioInput
                , onBlur = SolveRandomCellRatioInputBlurred
                , onInput = SolveRandomCellRatioInputChanged
                , onRangeChange = SolveRandomCellRatioChanged
                }
            , viewRatioInputs
                { label = "Remove Random Candidate Ratio:"
                , hint = "Ratio of Remove Random Candidate items to number of boards."
                , hintId = "remove-random-candidate-ratio-hint"
                , value = model.removeRandomCandidateRatio
                , inputValue = model.removeRandomCandidateRatioInput
                , onBlur = RemoveRandomCandidateRatioInputBlurred
                , onInput = RemoveRandomCandidateRatioInputChanged
                , onRangeChange = RemoveRandomCandidateRatioChanged
                }
            , viewRatioInputs
                { label = "Disco Trap Ratio:"
                , hint = "Ratio of Disco Trap items to number of boards."
                , hintId = "disco-trap-ratio-hint"
                , value = model.discoTrapRatio
                , inputValue = model.discoTrapRatioInput
                , onBlur = DiscoTrapRatioInputBlurred
                , onInput = DiscoTrapRatioInputChanged
                , onRangeChange = DiscoTrapRatioChanged
                }
            , viewRatioInputs
                { label = "Emoji Trap Ratio:"
                , hint = "Ratio of Emoji Trap items to number of boards."
                , hintId = "emoji-trap-ratio-hint"
                , value = model.emojiTrapRatio
                , inputValue = model.emojiTrapRatioInput
                , onBlur = EmojiTrapRatioInputBlurred
                , onInput = EmojiTrapRatioInputChanged
                , onRangeChange = EmojiTrapRatioChanged
                }
            ]
        ]


viewMenuOptionsLocalPlay : Model -> Html Msg
viewMenuOptionsLocalPlay model =
    Html.details
        [ HA.class "info-panel-details"
        , HA.attribute "open" "true"
        ]
        [ Html.summary
            []
            [ Html.text "Local Game Options" ]
        , Html.div
            [ HA.class "column gap-m"
            ]
            [ Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.div
                    [ HA.class "row gap-s"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    ]
                    [ Html.text "Seed:"
                    , viewOptionHint
                        "seed-hint"
                        "The seed for the random number generator used to generate the puzzle. A seed will always produce the same puzzle if given the same options."
                    ]
                , Html.input
                    [ HA.class "input"
                    , HA.type_ "number"
                    , HA.min "0"
                    , HA.max (String.fromInt Random.maxInt)
                    , HA.value (String.fromInt model.seedInput)
                    , HE.onInput SeedInputChanged
                    ]
                    []
                ]
            ]
        ]


viewMenuOptionsArchipelago : Model -> Html Msg
viewMenuOptionsArchipelago model =
    Html.details
        [ HA.class "info-panel-details"
        , HA.attribute "open" "true"
        ]
        [ Html.summary
            []
            [ Html.text "Archipelago Options" ]
        , Html.div
            [ HA.class "column gap-m"
            ]
            [ Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    ]
                    [ Html.text "Player Name:"
                    , viewOptionHint
                        "player-name-hint"
                        (String.join
                            "\n"
                            [ "Your name in-game, limited to 16 characters."
                            , " - {player} will be replaced with the player's slot number."
                            , " - {PLAYER} will be replaced with the player's slot number, if that slot number is greater than 1."
                            , " - {number} will be replaced with the counter value of the name."
                            , " - {NUMBER} will be replaced with the counter value of the name, if the counter value is greater than 1."
                            ]
                        )
                    ]
                , Html.input
                    [ HA.class "input"
                    , HA.type_ "text"
                    , HA.value model.playerNameOption
                    , HE.onInput PlayerNameOptionChanged
                    ]
                    []
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    ]
                    [ Html.text "Pre-fill Nothings Percent"
                    , viewOptionHint
                        "pre-fill-nothings-percent-hint"
                        (String.join
                            "\n"
                            [ "Percentage of Nothing items that should be pre-filled, forcing them to be placed in an Archipeladoku game and thus excluding them from other games."
                            , "Caution: This reduces the number of filler items in the item pool. Having few fillers can lead to increased generation times or even generation failures."
                            ]
                        )
                    ]
                , Html.div
                    [ HA.class "row gap-s"
                    , HA.style "align-items" "center"
                    ]
                    [ Html.input
                        [ HA.class "input"
                        , HA.type_ "number"
                        , HA.style "width" "3em"
                        , HA.min "1"
                        , HA.max "100"
                        , HA.value model.preFillNothingsPercentInput
                        , HE.onBlur PreFillNothingsPercentInputBlurred
                        , HE.onInput PreFillNothingsPercentInputChanged
                        ]
                        []
                    , viewRangeSlider
                        model.preFillNothingsPercent
                        0
                        100
                        PreFillNothingsPercentChanged
                        (Just "pre-fill-nothings-percent-ticks")
                    , Html.datalist
                        [ HA.id "pre-fill-nothings-percent-ticks"
                        ]
                        (List.map
                            (\tick ->
                                Html.option
                                    [ HA.value (String.fromInt tick) ]
                                    []
                            )
                            [ 0, 25, 50, 75, 100 ]
                        )
                    ]
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    ]
                    [ Html.text "Progression Balancing:"
                    , viewOptionHint
                        "progression-balancing-hint"
                        "A system that can move progression earlier, to try and prevent the player from getting stuck and bored early. A lower setting means more getting stuck. A higher setting means less getting stuck."
                    ]
                , Html.div
                    [ HA.class "row gap-s"
                    , HA.style "align-items" "center"
                    ]
                    [ Html.input
                        [ HA.class "input"
                        , HA.type_ "number"
                        , HA.style "width" "3em"
                        , HA.min "0"
                        , HA.max "99"
                        , HA.value model.progressionBalancingInput
                        , HE.onBlur ProgressionBalancingInputBlurred
                        , HE.onInput ProgressionBalancingInputChanged
                        ]
                        []
                    , viewRangeSlider
                        model.progressionBalancing
                        0
                        99
                        ProgressionBalancingChanged
                        (Just "progression-balancing-ticks")
                    , Html.datalist
                        [ HA.id "progression-balancing-ticks"
                        ]
                        (List.map
                            (\tick ->
                                Html.option
                                    [ HA.value (String.fromInt tick) ]
                                    []
                            )
                            [ 0, 50, 99 ]
                        )
                    ]
                ]
            ]
        ]


viewOptionHint : String -> String -> Html Msg
viewOptionHint id text =
    Html.div
        []
        [ Html.button
            [ HA.class "option-hint-button"
            , HA.attribute "popovertarget" id
            ]
            [ Html.text "?" ]
        , Html.div
            [ HA.id id
            , HA.class "option-hint"
            , HA.attribute "popover" "auto"
            ]
            [ Html.text text ]
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
        , HA.style "flex-grow" "1"
        , HE.onInput (String.toInt >> Maybe.withDefault value >> msg)
        ]
        []


viewRatioInputs :
    { label : String
    , hint : String
    , hintId : String
    , value : Int
    , inputValue : String
    , onBlur : Msg
    , onInput : String -> Msg
    , onRangeChange : Int -> Msg
    }
    -> Html Msg
viewRatioInputs args =
    Html.div
        [ HA.class "column gap-s"
        ]
        [ Html.div
            [ HA.class "row gap-m"
            , HA.style "align-items" "center"
            , HA.style "justify-content" "space-between"
            ]
            [ Html.text args.label
            , viewOptionHint args.hintId args.hint
            ]
        , Html.div
            [ HA.class "row gap-s"
            , HA.style "align-items" "center"
            ]
            [ Html.input
                [ HA.class "input"
                , HA.type_ "number"
                , HA.style "width" "4em"
                , HA.min "0"
                , HA.max (String.fromInt maxRatio)
                , HA.value args.inputValue
                , HE.onBlur args.onBlur
                , HE.onInput args.onInput
                ]
                []
            , Html.text "%"
            , viewRangeSlider
                args.value
                0
                500
                args.onRangeChange
                (Just "filler-ratio-ticks")
            ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    Html.div
        [ HA.class "grid"
        ]
        [ Html.node "archipeladoku-board"
            [ HA.property "data" model.boardData
            , HE.preventDefaultOn "keydown" (keyDownDecoder model)
            , HE.on "keyup" keyUpDecoder
            , HE.on "cellselected" cellSelectedDecoder
            , HA.tabindex 0
            ]
            []
        , viewZoomControls
        , viewTrapTimers model
        ]


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


viewTrapTimers : Model -> Html Msg
viewTrapTimers model =
    if model.discoTrapTimer == 0 && model.emojiTrapTimer == 0 then
        Html.text ""

    else
        Html.div
            [ HA.class "trap-timer-panel"
            ]
            (List.map
                (\trap ->
                    if trap.timeLeft == 0 then
                        Html.text ""

                    else
                        Html.div
                            [ HA.class "column gap-s"
                            ]
                            [ Html.text
                                (String.concat
                                    [ trap.label
                                    , ": "
                                    , String.fromInt trap.timeLeft
                                    , "s"
                                    ]
                                )
                            , Html.div
                                [ HA.class "trap-timer-track" ]
                                [ Html.div
                                    [ HA.class "trap-timer-fill"
                                    , HA.style "width"
                                        (String.fromInt
                                            (trap.timeLeft * 100 // trapDuration)
                                            ++ "%"
                                        )
                                    ]
                                    []
                                ]
                            ]
                )
                [ { label = "Disco trap"
                  , timeLeft = model.discoTrapTimer
                  }
                , { label = "Emoji trap"
                  , timeLeft = model.emojiTrapTimer
                  }
                ]
            )


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
                , HA.style "flex-wrap" "wrap"
                ]
                (List.append
                    (List.map
                        (\n ->
                            let
                                colorNumber : Int
                                colorNumber =
                                    if model.discoTrapTimer > 0 then
                                        Dict.get n model.discoTrapMap
                                            |> Maybe.withDefault n

                                    else
                                        n
                            in
                            Html.div
                                [ HA.class "column gap-s"
                                , HA.style "align-items" "center"
                                ]
                                [ Html.button
                                    [ HE.onClick (NumberPressed n)
                                    , HA.class "cell"
                                    , HA.class <| "val-" ++ String.fromInt colorNumber
                                    , HAE.attributeIf
                                        (not <| Set.member n validCellCandidates)
                                        (HA.class "error")
                                    , HA.style "font-size" "1.5em"
                                    ]
                                    [ Html.text (numberToString model n) ]
                                , if model.emojiTrapTimer > 0 then
                                    Html.text
                                        (String.concat
                                            [ "["
                                            , numberToString
                                                { model | emojiTrapTimer = 0 }
                                                n
                                            , "]"
                                            ]
                                        )

                                  else
                                    Html.text ""
                                ]
                        )
                        (List.range 1 model.blockSize)
                    )
                    [ Html.button
                        [ HE.onClick DeletePressed
                        , HA.class "cell"
                        , HA.style "font-size" "1.5em"
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
                    [ Html.text "Select single-candidate cell" ]
                , Html.text "[S]"
                ]
            , Html.div
                [ HA.class "row gap-m"
                , HA.style "align-items" "center"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HE.onClick SelectSolvableBoardPressed
                    ]
                    [ Html.text "Select solvable board"
                    ]
                , Html.text "[G]"
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
            , Html.label
                [ HA.class "row gap-s" ]
                [ Html.text "Emoji trap variant:"
                , Html.select
                    [ HE.onInput EmojiTrapVariantChanged
                    ]
                    [ Html.option
                        [ HA.value "animals"
                        , HA.selected (model.emojiTrapVariant == EmojiTrapAnimals)
                        ]
                        [ Html.text "Animals" ]
                    , Html.option
                        [ HA.value "fruits"
                        , HA.selected (model.emojiTrapVariant == EmojiTrapFruits)
                        ]
                        [ Html.text "Fruits" ]
                    , Html.option
                        [ HA.value "random"
                        , HA.selected (model.emojiTrapVariant == EmojiTrapRandom)
                        ]
                        [ Html.text "Random" ]
                    ]
                ]
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
            , Html.button
                [ HA.class "button"
                , HE.onClick SolveSingleCandidatesPressed
                ]
                [ Html.text "Solve single-candidate cells in board" ]
            , Html.button
                [ HA.class "button"
                , HE.onClick TriggerDiscoTrapPressed
                ]
                [ Html.text "Trigger Disco Trap" ]
            , Html.button
                [ HA.class "button"
                , HE.onClick TriggerEmojiTrapPressed
                ]
                [ Html.text "Trigger Emoji Trap" ]
            , Html.button
                [ HA.class "button"
                , HE.onClick CancelTrapsPressed
                ]
                [ Html.text "Cancel Traps" ]
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
                            [ HA.class "button"
                            , HE.onClick
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
