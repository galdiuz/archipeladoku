port module Archipeladoku exposing (..)

import Array exposing (Array)
import Bitwise
import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import File
import File.Download
import File.Select
import Html exposing (Html)
import Html.Attributes as HA
import Html.Attributes.Extra as HAE
import Html.Events as HE
import Html.Extra
import Html.Keyed
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra
import Json.Decode.Field as Field
import Json.Encode as Encode
import List.Extra
import Maybe.Extra
import Order.Extra
import Process
import Random
import Random.List
import Set exposing (Set)
import Set.Extra
import String.Extra
import Task
import Time
import Yaml.Decode
import Yaml.Encode


port centerViewOnCell : ( Int, Int ) -> Cmd msg
port checkLocation : Int -> Cmd msg
port checkLocations : List Int -> Cmd msg
port clearLocalStorage : () -> Cmd msg
port clearSavedGames : () -> Cmd msg
port connect : Encode.Value -> Cmd msg
port generateBoard : Encode.Value -> Cmd msg
port goal : () -> Cmd msg
port hintForItem : String -> Cmd msg
port log : String -> Cmd msg
port moveCellIntoView : ( Int, Int ) -> Cmd msg
port saveGameState : Encode.Value -> Cmd msg
port scoutLocations : { locations : List Int, createHint : Bool } -> Cmd msg
port sendMessage : String -> Cmd msg
port sendPlayingStatus : () -> Cmd msg
port setDeathLink : Bool -> Cmd msg
port setLocalStorage : (String, String) -> Cmd msg
port triggerAnimation : Encode.Value -> Cmd msg
port triggerFirework : () -> Cmd msg
port zoom : Encode.Value -> Cmd msg
port zoomReset : () -> Cmd msg

port receiveCheckedLocations : (List Int -> msg) -> Sub msg
port receiveConnectionStatus : (Bool -> msg) -> Sub msg
port receiveDeathLink : (Decode.Value -> msg) -> Sub msg
port receiveGeneratedBoard : (Decode.Value -> msg) -> Sub msg
port receiveGenerationProgress : (Decode.Value -> msg) -> Sub msg
port receiveHintCost : (Int -> msg) -> Sub msg
port receiveHintPoints : (Int -> msg) -> Sub msg
port receiveHints : (Decode.Value -> msg) -> Sub msg
port receiveItems : (List Int -> msg) -> Sub msg
port receiveKeyboardLayout : (Decode.Value -> msg) -> Sub msg
port receiveLocalGameSave : (Decode.Value -> msg) -> Sub msg
port receiveMessage : (Decode.Value -> msg) -> Sub msg
port receiveOnlineGameSave : (Decode.Value -> msg) -> Sub msg
port receiveScoutedItems : (Decode.Value -> msg) -> Sub msg
port receiveSlotData : (Decode.Value -> msg) -> Sub msg


type alias Model =
    { animationsEnabled : Bool
    , autoApplyServerChecks : Bool
    , autoFillCandidatesOnUnlock : Bool
    , autoRemoveInvalidCandidates : Bool
    , blockBundles : Dict ( Int, Int ) Int
    , blockSize : Int
    , boardData : Encode.Value
    , boardsPerCluster : Int
    , bundleBlocks : Dict Int (List ( Int, Int ))
    , bundleSize : Int
    , bundleSizeInput : String
    , candidateLayout : Int
    , candidateMode : Bool
    , cellBlocks : Dict ( Int, Int ) (List Area)
    , cellBoards : Dict ( Int, Int ) (List Area)
    , cellCols : Dict ( Int, Int ) (List Area)
    , cellRows : Dict ( Int, Int ) (List Area)
    , colorScheme : String
    , connectionHistory : List ConnectionHistoryEntry
    , createHintsWhenScouting : Bool
    , current : Dict ( Int, Int ) CellValue
    , deathLinkEnabled : Bool
    , deathLinkInput : Bool
    , deathLinkTriggers : Int
    , disabledLocations : Set String
    , disabledLocationsChecked : Set String
    , discoTrapMap : Dict Int Int
    , discoTrapOffset : Int
    , discoTrapRatio : Int
    , discoTrapRatioInput : String
    , discoTrapReceived : Int
    , discoTrapTimer : Int
    , discoTrapTriggers : Int
    , difficulty : Int
    , duplicateProgression : Int
    , duplicateProgressionInput : String
    , emojiTrapMap : Dict Int String
    , emojiTrapRatio : Int
    , emojiTrapRatioInput : String
    , emojiTrapReceived : Int
    , emojiTrapTimer : Int
    , emojiTrapTriggers : Int
    , emojiTrapVariant : EmojiTrapVariant
    , errors : Dict ( Int, Int ) CellError
    , fireworkOnNothing : Bool
    , fireworksTimer : Int
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
    , hintedLocations : Set Int
    , hintPoints : Int
    , host : String
    , inputModifierDebounce : Int
    , keyBindings : Dict String (List String)
    , keyboardLayout : Dict String String
    , lastSaveTime : Int
    , listeningForBinding : Maybe ( BindableAction, Int )
    , localGameSave : Maybe SavedGame
    , locationScouting : LocationScouting
    , lockedBlocks : List ( Int, Int )
    , messageCounter : Int
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
    , pendingSolvedBoards : Set ( Int, Int )
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
    , removeRandomCandidateReceived : Int
    , removeRandomCandidateUsed : Int
    , scoutedItems : Dict Int Hint
    , seed : Random.Seed
    , seedInput : Int
    , selectedCell : ( Int, Int )
    , serverCheckedLocations : Set Int
    , showInputErrors : Bool
    , showKeybindingsMenu : Bool
    , showToastMessages : Bool
    , solution : Dict ( Int, Int ) Int
    , solveRandomCellRatio : Int
    , solveRandomCellRatioInput : String
    , solveRandomCellReceived : Int
    , solveRandomCellUsed : Int
    , solveSelectedCellRatio : Int
    , solveSelectedCellRatioInput : String
    , solveSelectedCellReceived : Int
    , solveSelectedCellUsed : Int
    , solvedLocations : Set Int
    , timezone : Time.Zone
    , toastMessages : List ToastMessage
    , trapDuration : Int
    , tunnelVisionTrapRatio : Int
    , tunnelVisionTrapRatioInput : String
    , tunnelVisionTrapReceived : Int
    , tunnelVisionTrapTimer : Int
    , tunnelVisionTrapTriggers : Int
    , undoStack : List (Dict ( Int, Int ) CellValue)
    , unlockedBlocks : Set ( Int, Int )
    , unlockMap : Dict Int Item
    , version : String
    , visibleCells : Set ( Int, Int )
    }


type Msg
    = AddDebugItemsPressed
    | AutoFillCandidatesOnUnlockChanged Bool
    | AutoRemoveInvalidCandidatesChanged Bool
    | BlockSizeChanged Int
    | BoardsPerClusterChanged Int
    | CancelTrapsPressed
    | CandidateLayoutChanged String
    | CandidateModeChanged Bool
    | CellSelected ( Int, Int )
    | ClearBoardPressed
    | ClearCellPressed
    | ClearConnectionHistoryPressed
    | ClearSavedGamesPressed
    | ColorSchemeChanged String
    | ConnectionHistoryQuickFillPressed ConnectionHistoryEntry
    | ConnectPressed
    | DeathLinkInputChanged Bool
    | DeathLinkTriggered Decode.Value
    | DifficultyChanged Int
    | DisabledLocationChanged String Bool
    | DiscoTrapRatioChanged Int
    | DiscoTrapRatioInputBlurred
    | DiscoTrapRatioInputChanged String
    | DuplicateProgressionChanged Int
    | DuplicateProgressionInputBlurred
    | DuplicateProgressionInputChanged String
    | BundleSizeChanged Int
    | BundleSizeInputBlurred
    | BundleSizeInputChanged String
    | CreateHintsWhenScoutingChanged Bool
    | EmojiTrapRatioChanged Int
    | EmojiTrapRatioInputBlurred
    | EmojiTrapRatioInputChanged String
    | EmojiTrapVariantChanged String
    | EnableAnimationsChanged Bool
    | EnableDeathLinkChanged Bool
    | FillBoardCandidatesPressed
    | FillCellCandidatesPressed
    | FireworkOnNothingChanged Bool
    | GenerateYamlPressed
    | GotCheckedLocations (List Int)
    | GotConnectionStatus Bool
    | GotGeneratedBoard Decode.Value
    | GotGenerationProgress Decode.Value
    | GotHintCost Int
    | GotHintPoints Int
    | GotHints Decode.Value
    | GotItems (List Int)
    | GotKeyboardLayout Decode.Value
    | GotLocalGameSave Decode.Value
    | GotMessage Decode.Value
    | GotYamlContent String
    | GotYamlFile File.File
    | GotOnlineGameSave Decode.Value
    | GotSaveGameTime Bool Time.Posix
    | GotScoutedItems Decode.Value
    | GotSlotData Decode.Value
    | GotTimezone Time.Zone
    | HighlightModeChanged HighlightMode
    | HintItemPressed String
    | HintLocationPressed Int
    | HostInputChanged String
    | InputModifierDebouncePassed Int
    | InputModifierHeld
    | InputModifierReleased
    | KeyBindingCaptured String
    | LoadYamlPressed
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
    | RebindSlotPressed BindableAction Int
    | RemoveInvalidCandidatesPressed
    | RemoveRandomCandidatePressed
    | RemoveRandomCandidateRatioChanged Int
    | RemoveRandomCandidateRatioInputBlurred
    | RemoveRandomCandidateRatioInputChanged String
    | ResetClientSettingsPressed
    | ResetKeybindingsPressed
    | ResumeLocalGamePressed SavedGame
    | ScoutAllPressed
    | ScoutLocationPressed Int
    | SecondPassed
    | SeedInputChanged String
    | SelectSingleCandidateCellPressed
    | SelectSolvableBoardPressed
    | SendMessagePressed
    | ShowInputErrorsChanged Bool
    | ShowToastMessagesChanged Bool
    | SolveRandomCellPressed
    | SolveRandomCellRatioChanged Int
    | SolveRandomCellRatioInputBlurred
    | SolveRandomCellRatioInputChanged String
    | SolveSelectedCellPressed
    | SolveSelectedCellRatioChanged Int
    | SolveSelectedCellRatioInputBlurred
    | SolveSelectedCellRatioInputChanged String
    | SolveSingleCandidatesPressed
    | SyncSolvedFromServerPressed
    | SyncSolvedToServerPressed
    | ToggleCandidateModePressed
    | ToggleHighlightModePressed
    | ToggleKeybindingsMenuPressed
    | TrapDurationChanged String
    | TriggerDiscoTrapPressed
    | TriggerEmojiTrapPressed
    | TriggerFireworksPressed
    | TriggerTunnelVisionTrapPressed
    | TunnelVisionTrapRatioChanged Int
    | TunnelVisionTrapRatioInputBlurred
    | TunnelVisionTrapRatioInputChanged String
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
      , autoApplyServerChecks = False
      , autoFillCandidatesOnUnlock = False
      , autoRemoveInvalidCandidates = False
      , blockBundles = Dict.empty
      , blockSize = 9
      , boardData = Encode.null
      , boardsPerCluster = 5
      , bundleBlocks = Dict.empty
      , bundleSize = 1
      , bundleSizeInput = "1"
      , candidateLayout = 0
      , candidateMode = False
      , cellBlocks = Dict.empty
      , cellBoards = Dict.empty
      , cellCols = Dict.empty
      , cellRows = Dict.empty
      , colorScheme = "light dark"
      , connectionHistory = []
      , createHintsWhenScouting = True
      , current = Dict.empty
      , deathLinkEnabled = False
      , deathLinkInput = False
      , deathLinkTriggers = 0
      , disabledLocations = Set.empty
      , disabledLocationsChecked = Set.empty
      , discoTrapMap = Dict.empty
      , discoTrapOffset = 0
      , discoTrapRatio = 20
      , discoTrapRatioInput = "20"
      , discoTrapReceived = 0
      , discoTrapTimer = 0
      , discoTrapTriggers = 0
      , difficulty = 2
      , duplicateProgression = 0
      , duplicateProgressionInput = "0"
      , emojiTrapMap = Dict.empty
      , emojiTrapRatio = 20
      , emojiTrapRatioInput = "20"
      , emojiTrapReceived = 0
      , emojiTrapTimer = 0
      , emojiTrapTriggers = 0
      , emojiTrapVariant = EmojiTrapRandom
      , errors = Dict.empty
      , fireworkOnNothing = True
      , fireworksTimer = 0
      , generationProgress = ( "Initializing", 0 )
      , gameIsLocal = False
      , gameState = MainMenu
      , givens = Set.empty
      , heldKeys = Set.empty
      , highlightedCells = Set.empty
      , highlightedNumbers = Set.empty
      , highlightMode = HighlightNone
      , hints = Dict.empty
      , hintCost = 0
      , hintedLocations = Set.empty
      , hintPoints = 0
      , host = ""
      , inputModifierDebounce = 0
      , keyBindings = defaultKeyBindings
      , keyboardLayout = Dict.empty
      , lastSaveTime = 0
      , listeningForBinding = Nothing
      , localGameSave = Nothing
      , locationScouting = ScoutingManual
      , lockedBlocks = []
      , messageCounter = 0
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
      , pendingSolvedBoards = Set.empty
      , pendingSolvedCols = Set.empty
      , pendingSolvedRows = Set.empty
      , player = ""
      , playerNameOption = "Player{number}"
      , preFillNothingsPercent = 100
      , preFillNothingsPercentInput = "100"
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
      , removeRandomCandidateReceived = 0
      , removeRandomCandidateUsed = 0
      , scoutedItems = Dict.empty
      , seed = Random.initialSeed (flags.seed + 1)
      , seedInput = flags.seed
      , selectedCell = ( 1, 1 )
      , serverCheckedLocations = Set.empty
      , showInputErrors = True
      , showKeybindingsMenu = False
      , showToastMessages = True
      , solution = Dict.empty
      , solveRandomCellRatio = 150
      , solveRandomCellRatioInput = "150"
      , solveRandomCellReceived = 0
      , solveRandomCellUsed = 0
      , solveSelectedCellRatio = 100
      , solveSelectedCellRatioInput = "100"
      , solveSelectedCellReceived = 0
      , solveSelectedCellUsed = 0
      , solvedLocations = Set.empty
      , timezone = Time.utc
      , toastMessages = []
      , trapDuration = 60
      , tunnelVisionTrapRatio = 20
      , tunnelVisionTrapRatioInput = "20"
      , tunnelVisionTrapReceived = 0
      , tunnelVisionTrapTimer = 0
      , tunnelVisionTrapTriggers = 0
      , undoStack = []
      , unlockedBlocks = Set.empty
      , unlockMap = Dict.empty
      , version = flags.version
      , visibleCells = Set.empty
      }
    , Task.perform GotTimezone Time.here
    )
        |> andThen (updateFromLocalStorage flags.localStorage)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveCheckedLocations GotCheckedLocations
        , receiveConnectionStatus GotConnectionStatus
        , receiveDeathLink DeathLinkTriggered
        , receiveGeneratedBoard GotGeneratedBoard
        , receiveGenerationProgress GotGenerationProgress
        , receiveHintCost GotHintCost
        , receiveHintPoints GotHintPoints
        , receiveHints GotHints
        , receiveItems GotItems
        , receiveKeyboardLayout GotKeyboardLayout
        , receiveLocalGameSave GotLocalGameSave
        , receiveMessage GotMessage
        , receiveOnlineGameSave GotOnlineGameSave
        , receiveScoutedItems GotScoutedItems
        , receiveSlotData GotSlotData
        , if List.any ((<) 0) (timers model) || not (List.isEmpty model.toastMessages) then
            Time.every 1000 (\_ -> SecondPassed)

          else
            Sub.none
        , if model.showKeybindingsMenu && model.listeningForBinding == Nothing then
            Browser.Events.onKeyDown escapeToCloseDecoder

          else
            Sub.none
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddDebugItemsPressed ->
            ( { model
                | removeRandomCandidateReceived = model.removeRandomCandidateReceived + 1000
                , solveSelectedCellReceived = model.solveSelectedCellReceived + 1000
                , solveRandomCellReceived = model.solveRandomCellReceived + 1000
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
                |> andThen (updateState True)

        BlockSizeChanged size ->
            ( { model
                | blockSize = size
                , numberOfBoards = min model.numberOfBoards (maxNumberOfBoards size)
                , numberOfBoardsInput = String.fromInt (min model.numberOfBoards (maxNumberOfBoards size))
                , bundleSize = clamp 1 size model.bundleSize
                , bundleSizeInput = String.fromInt (clamp 1 size model.bundleSize)
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
                , fireworksTimer = 0
                , tunnelVisionTrapTimer = 0
              }
            , Cmd.none
            )
                |> andThen updateBoardData

        CandidateLayoutChanged stringValue ->
            let
                value : Int
                value =
                    candidateLayoutFromString stringValue
            in
            ( { model | candidateLayout = value }
            , setLocalStorage ( "apdk-candidate-layout", String.fromInt value )
            )
                |> andThen updateBoardData

        CandidateModeChanged value ->
            ( { model | candidateMode = value }
            , Cmd.none
            )

        CellSelected ( row, col ) ->
            if Dict.member ( row, col ) model.solution then
                ( { model
                    | selectedCell = ( row, col )
                    , autoApplyServerChecks = False
                  }
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
                        |> List.concatMap .cells
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
                , deathLinkTriggers = model.deathLinkTriggers + 1
              }
            , Cmd.none
            )
                |> andThen (updateState True)

        ClearCellPressed ->
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
                    |> andThen (updateState True)

            else
                ( model
                , Cmd.none
                )

        ClearConnectionHistoryPressed ->
            ( { model | connectionHistory = [] }
            , setLocalStorage ( "apdk-connection-history", "[]" )
            )

        ClearSavedGamesPressed ->
            ( { model | localGameSave = Nothing }
            , clearSavedGames ()
            )

        ColorSchemeChanged scheme ->
            ( { model | colorScheme = scheme }
            , setLocalStorage ( "apdk-color-scheme", scheme )
            )
                |> andThen updateBoardData

        ConnectionHistoryQuickFillPressed entry ->
            ( { model | host = entry.host, password = entry.password, player = entry.player }
            , Cmd.batch
                [ setLocalStorage ( "apdk-host", entry.host )
                , setLocalStorage ( "apdk-password", entry.password )
                , setLocalStorage ( "apdk-player", entry.player )
                ]
            )

        ConnectPressed ->
            ( { model
                | gameState = Connecting
                , autoApplyServerChecks = False
                , serverCheckedLocations = Set.empty
              }
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

        CreateHintsWhenScoutingChanged value ->
            ( { model | createHintsWhenScouting = value }
            , setLocalStorage ( "apdk-create-hints-when-scouting", if value then "1" else "0" )
            )

        DeathLinkInputChanged value ->
            ( { model | deathLinkInput = value }
            , Cmd.none
            )

        DeathLinkTriggered value ->
            let
                message : String
                message =
                    case Decode.decodeValue deathLinkDecoder value of
                        Ok deathLink ->
                            case deathLink.cause of
                                Just cause ->
                                    String.concat
                                        [ "Death Link triggered by "
                                        , deathLink.source
                                        , ": "
                                        , cause
                                        , "."
                                        ]

                                Nothing ->
                                    String.concat
                                        [ "Death Link triggered by "
                                        , deathLink.source
                                        , "."
                                        ]

                        Err _ ->
                            "Death Link triggered"
            in
            ( { model
                | current = Dict.empty
                , deathLinkTriggers = model.deathLinkTriggers + 1
                , undoStack = []
              }
                |> addLocalMessage True message
            , Cmd.none
            )
                |> andThen (updateState True)

        DifficultyChanged value ->
            ( { model | difficulty = value }
            , Cmd.none
            )

        DisabledLocationChanged locationType checked ->
            ( { model
                | disabledLocationsChecked =
                    if checked then
                        Set.insert locationType model.disabledLocationsChecked

                    else
                        Set.remove locationType model.disabledLocationsChecked
              }
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

        DuplicateProgressionChanged value ->
            ( { model
                | duplicateProgression = value
                , duplicateProgressionInput = String.fromInt value
              }
            , Cmd.none
            )

        DuplicateProgressionInputBlurred ->
            let
                value : Int
                value =
                    model.duplicateProgressionInput
                        |> String.toInt
                        |> Maybe.withDefault model.duplicateProgression
                        |> clamp 0 100
            in
            ( { model
                | duplicateProgression = value
                , duplicateProgressionInput = String.fromInt value
              }
            , Cmd.none
            )

        DuplicateProgressionInputChanged value ->
            ( { model
                | duplicateProgression =
                    String.toInt value
                        |> Maybe.withDefault model.duplicateProgression
                , duplicateProgressionInput = value
              }
            , Cmd.none
            )

        BundleSizeChanged value ->
            ( { model
                | bundleSize = value
                , bundleSizeInput = String.fromInt value
              }
            , Cmd.none
            )

        BundleSizeInputBlurred ->
            let
                value : Int
                value =
                    model.bundleSizeInput
                        |> String.toInt
                        |> Maybe.withDefault model.bundleSize
                        |> clamp 1 model.blockSize
            in
            ( { model
                | bundleSize = value
                , bundleSizeInput = String.fromInt value
              }
            , Cmd.none
            )

        BundleSizeInputChanged value ->
            ( { model
                | bundleSize =
                    String.toInt value
                        |> Maybe.withDefault model.bundleSize
                , bundleSizeInput = value
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
                |> andThen updateBoardData

        EnableDeathLinkChanged value ->
            ( { model | deathLinkEnabled = value }
            , setDeathLink value
            )

        FillBoardCandidatesPressed ->
            let
                boardCells : Set ( Int, Int )
                boardCells =
                    Dict.get model.selectedCell model.cellBoards
                        |> Maybe.withDefault []
                        |> List.concatMap .cells
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

        FireworkOnNothingChanged value ->
            ( { model | fireworkOnNothing = value }
            , setLocalStorage ( "apdk-firework-on-nothing", if value then "1" else "0" )
            )

        GenerateYamlPressed ->
            ( model
            , File.Download.string
                "archipeladoku.yaml"
                "text/yaml"
                (buildOptionsYaml model)
            )

        GotYamlContent content ->
            ( case Yaml.Decode.fromString decodeOptionsYaml content of
                Ok opts ->
                    applyYamlOptions opts model

                Err _ ->
                    model
            , Cmd.none
            )

        GotYamlFile file ->
            ( model
            , Task.perform GotYamlContent (File.toString file)
            )

        GotCheckedLocations locationIds ->
            ( { model
                | serverCheckedLocations =
                    Set.union model.serverCheckedLocations (Set.fromList locationIds)
              }
            , Cmd.none
            )
                |> andThen (updateState True)

        GotConnectionStatus status ->
            ( { model
                | autoApplyServerChecks =
                    if status && model.gameState == Connecting then
                        True

                    else
                        model.autoApplyServerChecks
                , gameState =
                    case ( status, model.gameState ) of
                        ( True, Connecting ) ->
                            Generating

                        ( False, Connecting ) ->
                            MainMenu

                        ( False, Generating ) ->
                            MainMenu

                        ( False, Playing ) ->
                            Disconnected

                        _ ->
                            model.gameState
              }
                |> addLocalMessage (not status && model.gameState == Playing) "Disconnected from server."
            , Cmd.none
            )
                |> andThenIf (status && model.gameState == Connecting) addConnectionHistory

        GotGeneratedBoard value ->
            case ( model.gameState, Decode.decodeValue generatedBoardDecoder value ) of
                ( Generating, Ok board ) ->
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
                        , solvedLocations = Set.empty
                        , unlockedBlocks = Set.empty
                        , unlockMap = board.unlockMap
                        , bundleBlocks =
                            if model.gameIsLocal then
                                board.bundles

                            else
                                model.bundleBlocks
                        , blockBundles =
                            if model.gameIsLocal then
                                buildBlockBundles board.bundles

                            else
                                model.blockBundles
                        , disabledLocations =
                            if model.gameIsLocal then
                                board.disabledLocations

                            else
                                model.disabledLocations
                      }
                    , if model.gameIsLocal then
                        Cmd.none

                      else
                        sendPlayingStatus ()
                    )
                        |> andThen (unlockInitialBlocks)
                        |> andThen (updateState True)

                _ ->
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
                    ( addKnownItems hints model
                        |> markHintedLocations hints
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
                |> andThen (updateState True)

        GotKeyboardLayout value ->
            ( { model
                | keyboardLayout =
                    Decode.decodeValue (Decode.dict Decode.string) value
                        |> Result.withDefault model.keyboardLayout
              }
            , Cmd.none
            )

        GotLocalGameSave value ->
            case Decode.decodeValue savedGameDecoder value of
                Ok savedGame ->
                    ( { model | localGameSave = Just savedGame }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                    , Cmd.none
                    )

        GotMessage value ->
            case Decode.decodeValue messageDecoder value of
                Ok message ->
                    ( addMessage message model
                    , Cmd.none
                    )

                Err err ->
                    ( model
                    , log (Decode.errorToString err)
                    )

        GotOnlineGameSave value ->
            case Decode.decodeValue savedGameDecoder value of
                Ok savedGame ->
                    ( loadSavedGame savedGame model
                    , Cmd.none
                    )
                        |> andThen addConnectionHistory
                        |> andThen (unlockInitialBlocks)
                        |> andThen (updateState False)

                Err err ->
                    ( { model | gameState = MainMenu }
                    , Cmd.none
                    )

        GotSaveGameTime debounce posix ->
            let
                maxSaveFrequency : Int
                maxSaveFrequency =
                    5000
            in
            if Time.posixToMillis posix - model.lastSaveTime >= maxSaveFrequency then
                ( { model | lastSaveTime = Time.posixToMillis posix }
                , saveGameState (encodeSavedGame posix model)
                )

            else if debounce then
                ( model
                , maxSaveFrequency - (Time.posixToMillis posix - model.lastSaveTime) + 100
                    |> toFloat
                    |> Process.sleep
                    |> Task.andThen (\_ -> Time.now)
                    |> Task.perform (GotSaveGameTime False)
                )

            else
                ( model
                , Cmd.none
                )

        GotScoutedItems value ->
            case Decode.decodeValue (Decode.list hintDecoder) value of
                Ok hints ->
                    ( addKnownItems hints model
                    , Cmd.none
                    )

                Err err ->
                    ( model
                    , Cmd.none
                    )

        GotSlotData value ->
            case Decode.decodeValue slotDataDecoder value of
                Ok slotData ->
                    ( { model
                        | deathLinkEnabled = slotData.deathLink
                        , locationScouting = slotData.locationScouting
                        , progression = slotData.progression
                        , seedInput = slotData.seed
                        , bundleSize = slotData.bundleSize
                        , bundleBlocks = slotData.bundleBlocks
                        , blockBundles = buildBlockBundles slotData.bundleBlocks
                        , disabledLocations = slotData.disabledLocations
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                    , Cmd.none
                    )

        GotTimezone timezone ->
            ( { model | timezone = timezone }
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

        HintLocationPressed id ->
            ( model
            , scoutLocations { locations = [ id ], createHint = True }
            )

        HostInputChanged value ->
            ( { model | host = value }
            , setLocalStorage ( "apdk-host", value )
            )

        InputModifierDebouncePassed debounceId ->
            if model.inputModifierDebounce == debounceId then
                ( { model | heldKeys = Set.remove "inputModifier" model.heldKeys }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        InputModifierHeld ->
            ( { model
                | heldKeys = Set.insert "inputModifier" model.heldKeys
                , inputModifierDebounce = model.inputModifierDebounce + 1
              }
            , Cmd.none
            )

        InputModifierReleased ->
            ( model
            , Process.sleep 100
                |> Task.perform (\_ -> InputModifierDebouncePassed model.inputModifierDebounce)
            )

        KeyBindingCaptured code ->
            case model.listeningForBinding of
                Just ( action, slotIndex ) ->
                    if code == "Escape" then
                        ( { model | listeningForBinding = Nothing }
                        , Cmd.none
                        )

                    else
                        let
                            newBindings : Dict String (List String)
                            newBindings =
                                setBindingSlot action slotIndex code model.keyBindings
                        in
                        ( { model
                            | keyBindings = newBindings
                            , listeningForBinding = Nothing
                          }
                        , setLocalStorage ( "apdk-keybindings", keyBindingsToString newBindings )
                        )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        LoadYamlPressed ->
            ( model
            , File.Select.file [ "text/yaml", "application/x-yaml", ".yaml" ] GotYamlFile
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
                        |> clamp 1 (maxNumberOfBoards model.blockSize)
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
                        |> clamp 1 (maxNumberOfBoards model.blockSize)
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
                    |> andThen (updateState True)

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
                    , bundleSize = model.bundleSize
                    , difficulty = model.difficulty
                    , disabledLocations = Set.toList model.disabledLocationsChecked
                    , discoTrapRatio = model.discoTrapRatio
                    , duplicateProgression = model.duplicateProgression
                    , emojiTrapRatio = model.emojiTrapRatio
                    , numberOfBoards = model.numberOfBoards
                    , progression = model.progression
                    , removeRandomCandidateRatio = model.removeRandomCandidateRatio
                    , seed = model.seedInput
                    , solveRandomCellRatio = model.solveRandomCellRatio
                    , solveSelectedCellRatio = model.solveSelectedCellRatio
                    , tunnelVisionTrapRatio = model.tunnelVisionTrapRatio
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

        RebindSlotPressed action slotIndex ->
            if model.listeningForBinding == Just ( action, slotIndex ) then
                let
                    newBindings : Dict String (List String)
                    newBindings =
                        clearBindingSlot action slotIndex model.keyBindings
                in
                ( { model
                    | keyBindings = newBindings
                    , listeningForBinding = Nothing
                  }
                , setLocalStorage ( "apdk-keybindings", keyBindingsToString newBindings )
                )

            else
                ( { model | listeningForBinding = Just ( action, slotIndex ) }
                , Cmd.none
                )

        RemoveInvalidCandidatesPressed ->
            ( removeInvalidCandidates model
            , Cmd.none
            )
                |> andThen (updateState True)

        RemoveRandomCandidatePressed ->
            let
                boardCells : Set ( Int, Int )
                boardCells =
                    Dict.get model.selectedCell model.cellBoards
                        |> Maybe.withDefault []
                        |> List.concatMap .cells
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
                                                && not (Set.member cell model.givens)
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
            case ( maybeTarget, model.removeRandomCandidateReceived > model.removeRandomCandidateUsed ) of
                ( _, False ) ->
                    ( model, Cmd.none )

                ( Just ( cell, number ), True ) ->
                    ( { model
                        | current = Dict.update cell (toggleNumber number) model.current
                        , pendingCellChanges = Set.insert cell model.pendingCellChanges
                        , removeRandomCandidateUsed = model.removeRandomCandidateUsed + 1
                        , seed = newSeed
                      }
                        |> addLocalMessage
                            True
                            (String.concat
                                [ "Used Remove Random Candidate item to remove candidate "
                                , numberToString { model | emojiTrapTimer = 0 } number
                                , " from Cell "
                                , rowToLabel (Tuple.first cell)
                                , String.fromInt (Tuple.second cell)
                                ]
                            )
                    , Cmd.none
                    )
                        |> andThen (updateState True)

                ( Nothing, True ) ->
                    ( model
                        |> addLocalMessage
                            True
                            "Remove Random Candidate item could not be used because there are no valid candidates to remove in the selected board."
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

        ResetClientSettingsPressed ->
            let
                -- Take the defaults from init to keep them defined in one place.
                defaults : Model
                defaults =
                    Tuple.first (init Encode.null)
            in
            ( { model
                | animationsEnabled = defaults.animationsEnabled
                , autoFillCandidatesOnUnlock = defaults.autoFillCandidatesOnUnlock
                , autoRemoveInvalidCandidates = defaults.autoRemoveInvalidCandidates
                , candidateLayout = defaults.candidateLayout
                , colorScheme = defaults.colorScheme
                , createHintsWhenScouting = defaults.createHintsWhenScouting
                , emojiTrapVariant = defaults.emojiTrapVariant
                , fireworkOnNothing = defaults.fireworkOnNothing
                , keyBindings = defaults.keyBindings
                , showInputErrors = defaults.showInputErrors
                , showToastMessages = defaults.showToastMessages
                , trapDuration = defaults.trapDuration
              }
            , clearLocalStorage ()
            )

        ResetKeybindingsPressed ->
            ( { model | keyBindings = defaultKeyBindings }
            , setLocalStorage ( "apdk-keybindings", keyBindingsToString defaultKeyBindings )
            )

        ResumeLocalGamePressed save ->
            ( loadSavedGame save model
            , Cmd.none
            )
                |> andThen (updateState False)

        ScoutAllPressed ->
            scoutLocationIds (scoutableLocations model) model

        ScoutLocationPressed id ->
            scoutLocationIds (Set.singleton id) model

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
                , fireworksTimer =
                    if model.fireworksTimer > 0 then
                        model.fireworksTimer - 1

                    else
                        0
                , tunnelVisionTrapTimer =
                    if model.tunnelVisionTrapTimer > 0 then
                        model.tunnelVisionTrapTimer - 1

                    else
                        0
                , toastMessages =
                    model.toastMessages
                        |> List.filterMap
                            (\message ->
                                if message.timer > 1 then
                                    Just { message | timer = message.timer - 1 }

                                else
                                    Nothing
                            )
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
                                        board.cells
                                    )
                                    (List.all
                                        (cellIsVisible model)
                                        board.cells
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

        ShowInputErrorsChanged value ->
            ( { model | showInputErrors = value }
            , setLocalStorage ( "apdk-show-input-errors", if value then "1" else "0" )
            )

        ShowToastMessagesChanged value ->
            ( { model | showToastMessages = value }
            , setLocalStorage ( "apdk-show-toast-messages", if value then "1" else "0" )
            )

        SolveRandomCellPressed ->
            let
                boardCells : Set ( Int, Int )
                boardCells =
                    Dict.get model.selectedCell model.cellBoards
                        |> Maybe.withDefault []
                        |> List.concatMap .cells
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
            case ( maybeTargetCell, model.solveRandomCellReceived > model.solveRandomCellUsed ) of
                ( _, False ) ->
                    ( model, Cmd.none )

                ( Just targetCell, True ) ->
                    ( { model
                        | current = Dict.remove targetCell model.current
                        , givens = Set.insert targetCell model.givens
                        , pendingCellChanges = Set.insert targetCell model.pendingCellChanges
                        , seed = newSeed
                        , solveRandomCellUsed = model.solveRandomCellUsed + 1
                      }
                        |> addLocalMessage
                            True
                            (String.concat
                                [ "Used Solve Random Cell item at Cell "
                                , rowToLabel (Tuple.first targetCell)
                                , String.fromInt (Tuple.second targetCell)
                                ]
                            )
                    , Cmd.none
                    )
                        |> andThen (updateState True)

                ( Nothing, True ) ->
                    ( model
                        |> addLocalMessage
                            True
                            "Solve Random Cell item could not be used because there are no unsolved visible cells in the selected board."
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
            if model.solveSelectedCellReceived <= model.solveSelectedCellUsed then
                ( model, Cmd.none )

            else if not (Set.member model.selectedCell model.visibleCells) then
                ( addLocalMessage
                    True
                    (String.concat
                        [ "Solve Selected Cell item at Cell "
                        , rowToLabel (Tuple.first model.selectedCell)
                        , String.fromInt (Tuple.second model.selectedCell)
                        , " could not be used because the cell isn't unlocked."
                        ]
                    )
                    model
                , Cmd.none
                )

            else if Set.member model.selectedCell model.givens then
                ( addLocalMessage
                    True
                    (String.concat
                        [ "Solve Selected Cell item at Cell "
                        , rowToLabel (Tuple.first model.selectedCell)
                        , String.fromInt (Tuple.second model.selectedCell)
                        , " could not be used because the cell is already solved."
                        ]
                    )
                    model
                , Cmd.none
                )

            else
                ( { model
                    | current = Dict.remove model.selectedCell model.current
                    , givens = Set.insert model.selectedCell model.givens
                    , pendingCellChanges = Set.insert model.selectedCell model.pendingCellChanges
                    , solveSelectedCellUsed = model.solveSelectedCellUsed + 1
                  }
                    |> addLocalMessage
                        True
                        (String.concat
                            [ "Used Solve Selected Cell item at Cell "
                            , rowToLabel (Tuple.first model.selectedCell)
                            , String.fromInt (Tuple.second model.selectedCell)
                            ]
                        )
                , Cmd.none
                )
                    |> andThen (updateState True)

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
                        |> List.concatMap .cells
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
                |> andThen (updateState True)

        SyncSolvedFromServerPressed ->
            ( queueServerSolves model
            , Cmd.none
            )
                |> andThen (updateState True)

        SyncSolvedToServerPressed ->
            ( model
            , checkLocations (Set.toList model.solvedLocations)
            )

        ToggleCandidateModePressed ->
            ( { model | candidateMode = not model.candidateMode }
            , Cmd.none
            )

        ToggleHighlightModePressed ->
            ( { model
                | highlightMode =
                    if Set.member "inputModifier" model.heldKeys then
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

        ToggleKeybindingsMenuPressed ->
            ( { model | showKeybindingsMenu = not model.showKeybindingsMenu }
            , Cmd.none
            )

        TrapDurationChanged value ->
            ( { model
                | trapDuration =
                    String.toInt value
                        |> Maybe.withDefault model.trapDuration
              }
            , setLocalStorage ( "apdk-trap-duration", value )
            )

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

        TriggerFireworksPressed ->
            ( { model | fireworksTimer = model.trapDuration }
            , Cmd.none
            )

        TriggerTunnelVisionTrapPressed ->
            ( model
            , Cmd.none
            )
                |> andThen triggerTunnelVisionTrap

        TunnelVisionTrapRatioChanged value ->
            ( { model
                | tunnelVisionTrapRatio = value
                , tunnelVisionTrapRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        TunnelVisionTrapRatioInputBlurred ->
            let
                value : Int
                value =
                    model.tunnelVisionTrapRatioInput
                        |> String.toInt
                        |> Maybe.withDefault model.tunnelVisionTrapRatio
                        |> clamp 0 maxRatio
            in
            ( { model
                | tunnelVisionTrapRatio = value
                , tunnelVisionTrapRatioInput = String.fromInt value
              }
            , Cmd.none
            )

        TunnelVisionTrapRatioInputChanged value ->
            ( { model
                | tunnelVisionTrapRatio =
                    String.toInt value
                        |> Maybe.withDefault model.tunnelVisionTrapRatio
                , tunnelVisionTrapRatioInput = value
              }
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
                        |> andThen (updateState True)

        UnlockSelectedBlockPressed ->
            applyList
                (\block -> (unlockBlock True ( block.startRow, block.startCol ))
                )
                (Dict.get model.selectedCell model.cellBlocks
                    |> Maybe.withDefault []
                )
                ( model
                , Cmd.none
                )
                |> andThen (updateState True)
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
    , version : String
    }


defaultFlags : Flags
defaultFlags =
    { localStorage = Dict.empty
    , seed = 1
    , version = ""
    }


type alias ConnectionHistoryEntry =
    { host : String
    , password : String
    , player : String
    }


type alias GeneratedBoard =
    { blockSize : Int
    , blockUnlockOrder : List ( Int, Int )
    , bundles : Dict Int (List ( Int, Int ))
    , disabledLocations : Set String
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
    , cells : List ( Int, Int )
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
    , bundleSize : Int
    , difficulty : Int
    , disabledLocations : List String
    , discoTrapRatio : Int
    , duplicateProgression : Int
    , emojiTrapRatio : Int
    , numberOfBoards : Int
    , progression : Progression
    , removeRandomCandidateRatio : Int
    , seed : Int
    , solveRandomCellRatio : Int
    , solveSelectedCellRatio : Int
    , tunnelVisionTrapRatio : Int
    }


type Progression
    = Fixed
    | Shuffled


type GameState
    = MainMenu
    | Connecting
    | Generating
    | Playing
    | Disconnected


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
    | BlockBundle Int
    | SolveSelectedCell
    | SolveRandomCell
    | RemoveRandomCandidate
    | DiscoTrap
    | EmojiTrap
    | TunnelVisionTrap
    | NothingItem


type alias Hint =
    { locationId : Int
    , locationName : String
    , locationGameName : String
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


type alias DeathLink =
    { source : String
    , cause : Maybe String
    }


type alias Message =
    { nodes : List MessageNode
    , extra : MessageExtra
    }


type alias ToastMessage =
    { id : Int
    , message : Message
    , timer : Int
    }


type alias MessagePlayer =
    { name : String
    , alias : String
    }


type alias MessageItem =
    { name : String
    , sender : MessagePlayer
    , receiver : MessagePlayer
    }


type MessageNode
    = ItemMessageNode String
    | LocationMessageNode String
    | ColorMessageNode String String
    | TextualMessageNode String
    | PlayerMessageNode String


type MessageExtra
    = AdminCommandMessage
    | ChatMessage MessagePlayer
    | CollectedMessage MessagePlayer
    | ConnectedMessage MessagePlayer
    | CountdownMessage
    | DisconnectedMessage MessagePlayer
    | GoaledMessage MessagePlayer
    | ItemCheatedMessage MessageItem
    | ItemHintedMessage MessageItem
    | ItemSentMessage MessageItem
    | LocalMessage
    | ReleasedMessage MessagePlayer
    | ServerChatMessage
    | TagsUpdatedMessage MessagePlayer
    | TutorialMessage
    | UserCommandMessage


type alias SlotData =
    { bundleBlocks : Dict Int (List ( Int, Int ))
    , bundleSize : Int
    , deathLink : Bool
    , disabledLocations : Set String
    , locationScouting : LocationScouting
    , progression : Progression
    , seed : Int
    }


type LocationScouting
    = ScoutingAuto
    | ScoutingManual
    | ScoutingDisabled


type EmojiTrapVariant
    = EmojiTrapAnimals
    | EmojiTrapFruits
    | EmojiTrapShapes
    | EmojiTrapRandom


type alias PackedBoardCells =
    { coordinates : List Int
    , current : List Int
    , solution : List Int
    }


type alias UnpackedBoardCells =
    { current : Dict ( Int, Int ) CellValue
    , givens : Set ( Int, Int )
    , solution : Dict ( Int, Int ) Int
    }


type alias SavedGame =
    { blockSize : Int
    , bundleBlocks : Dict Int (List ( Int, Int ))
    , bundleSize : Int
    , coordinates : List Int
    , current : List Int
    , disabledLocations : Set String
    , discoTrapTriggers : Int
    , emojiTrapTriggers : Int
    , gameIsLocal : Bool
    , locationScouting : LocationScouting
    , lockedBlocks : List ( Int, Int )
    , progression : Progression
    , puzzleAreas : PuzzleAreas
    , removeRandomCandidateReceived : Int
    , removeRandomCandidateUsed : Int
    , seed : Int
    , solution : List Int
    , solveRandomCellReceived : Int
    , solveRandomCellUsed : Int
    , solveSelectedCellReceived : Int
    , solveSelectedCellUsed : Int
    , solvedLocations : Set Int
    , timestamp : Time.Posix
    , tunnelVisionTrapTriggers : Int
    , unlockMap : Dict Int Item
    , unlockedBlocks : Set ( Int, Int )
    }


type alias YamlOptions =
    { playerName : Maybe String
    , blockSize : Maybe Int
    , boardsPerCluster : Maybe Int
    , numberOfBoards : Maybe Int
    , difficulty : Maybe Int
    , progression : Maybe Progression
    , duplicateProgression : Maybe Int
    , bundleSize : Maybe Int
    , disabledLocations : Maybe (Set String)
    , locationScouting : Maybe LocationScouting
    , solveSelectedCellRatio : Maybe Int
    , solveRandomCellRatio : Maybe Int
    , removeRandomCandidateRatio : Maybe Int
    , emojiTrapRatio : Maybe Int
    , discoTrapRatio : Maybe Int
    , tunnelVisionTrapRatio : Maybe Int
    , preFillNothingsPercent : Maybe Int
    , progressionBalancing : Maybe Int
    }


type BindableAction
    = ClearBoard
    | ClearCell
    | EnterNumber Int
    | FillBoardCandidates
    | FillCellCandidates
    | HoldCandidateMode
    | MoveDown
    | MoveLeft
    | MoveRight
    | MoveUp
    | RemoveInvalidCandidates
    | SelectSingleCandidateCell
    | SelectSolvableBoard
    | ToggleCandidateMode
    | ToggleHighlightMode
    | Undo
    | UseRemoveRandomCandidate
    | UseSolveRandomCell
    | UseSolveSelectedCell
    | ZoomIn
    | ZoomOut
    | ZoomReset


type alias BindableActionData =
    { defaultCodes : List String
    , id : String
    , label : Model -> String
    , msg : Msg
    }


---
-- Encoding/decoding
---


codeDecoder : Decode.Decoder String
codeDecoder =
    Decode.field "code" Decode.string
        |> Decode.map normalizeCode


keyDownDecoder : Model -> Decode.Decoder ( Msg, Bool )
keyDownDecoder model =
    if model.showKeybindingsMenu then
        Decode.fail "keybindings menu open"

    else
        codeDecoder
            |> Decode.andThen
                (\code ->
                    case actionForCode code model.keyBindings of
                        Just action ->
                            Decode.succeed ( (bindableActionData action).msg, True )

                        Nothing ->
                            Decode.fail code
                )


keyBindingCaptureDecoder : Model -> Decode.Decoder ( Msg, Bool )
keyBindingCaptureDecoder model =
    case model.listeningForBinding of
        Just _ ->
            codeDecoder
                |> Decode.map (\code -> ( KeyBindingCaptured code, True ))

        Nothing ->
            Decode.fail "not listening for a binding"


escapeToCloseDecoder : Decode.Decoder Msg
escapeToCloseDecoder =
    Decode.field "code" Decode.string
        |> Decode.andThen
            (\code ->
                if code == "Escape" then
                    Decode.succeed ToggleKeybindingsMenuPressed

                else
                    Decode.fail code
            )


keyUpDecoder : Model -> Decode.Decoder Msg
keyUpDecoder model =
    codeDecoder
        |> Decode.andThen
            (\code ->
                if actionForCode code model.keyBindings == Just HoldCandidateMode then
                    Decode.succeed InputModifierReleased

                else
                    Decode.fail code
            )


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map3 Flags
        (Decode.field "localStorage" (Decode.dict Decode.string))
        (Decode.field "seed" Decode.int)
        (Decode.field "version" Decode.string)


keyBindingsToString : Dict String (List String) -> String
keyBindingsToString bindings =
    Encode.encode 0 (Encode.dict identity (Encode.list Encode.string) bindings)


keyBindingsFromString : String -> Maybe (Dict String (List String))
keyBindingsFromString string =
    Decode.decodeString (Decode.dict (Decode.list Decode.string)) string
        |> Result.toMaybe


encodeConnectionHistory : List ConnectionHistoryEntry -> Encode.Value
encodeConnectionHistory history =
    Encode.list
        (\entry ->
            Encode.object
                [ ( "host", Encode.string entry.host )
                , ( "password", Encode.string entry.password )
                , ( "player", Encode.string entry.player )
                ]
        )
        history


connectionHistoryDecoder : Decode.Decoder (List ConnectionHistoryEntry)
connectionHistoryDecoder =
    Decode.list
        (Decode.map3 ConnectionHistoryEntry
            (Decode.field "host" Decode.string)
            (Decode.field "password" Decode.string)
            (Decode.field "player" Decode.string)
        )


generatedBoardDecoder : Decode.Decoder GeneratedBoard
generatedBoardDecoder =
    Decode.map8 GeneratedBoard
        (Decode.field "blockSize" Decode.int)
        (Decode.field "blockUnlockOrder" (Decode.list blockUnlockOrderDecoder))
        (Decode.field "bundles" bundlesDecoder)
        (Decode.field "disabledLocations" (Decode.list Decode.string |> Decode.map Set.fromList))
        (Decode.field "givens" (cellsDictDecoder Decode.int))
        (Decode.field "puzzleAreas" puzzleAreasDecoder)
        (Decode.field "solution" (cellsDictDecoder Decode.int))
        (Decode.field "unlockMap" unlockMapDecoder)


bundlesDecoder : Decode.Decoder (Dict Int (List ( Int, Int )))
bundlesDecoder =
    Decode.list (Decode.list (tupleDecoder Decode.int Decode.int))
        |> Decode.map
            (\bundleList ->
                bundleList
                    |> List.indexedMap (\index cells -> ( index + 1, cells ))
                    |> Dict.fromList
            )


buildBlockBundles : Dict Int (List ( Int, Int )) -> Dict ( Int, Int ) Int
buildBlockBundles bundleBlocks =
    Dict.foldl
        (\bundle cells acc ->
            List.foldl (\cell -> Dict.insert cell bundle) acc cells
        )
        Dict.empty
        bundleBlocks


encodeBundleBlocks : Dict Int (List ( Int, Int )) -> Encode.Value
encodeBundleBlocks bundleBlocks =
    Dict.toList bundleBlocks
        |> Encode.list
            (\( index, cells ) ->
                Encode.list identity
                    [ Encode.int index
                    , Encode.list (encodeTuple Encode.int Encode.int) cells
                    ]
            )


bundleBlocksSaveDecoder : Decode.Decoder (Dict Int (List ( Int, Int )))
bundleBlocksSaveDecoder =
    Decode.list
        (Decode.map2 Tuple.pair
            (Decode.index 0 Decode.int)
            (Decode.index 1 (Decode.list (tupleDecoder Decode.int Decode.int)))
        )
        |> Decode.map Dict.fromList


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


encodeUnlockMap : Dict Int Item -> Encode.Value
encodeUnlockMap unlockMap =
    Dict.toList unlockMap
        |> Encode.list
            (\( id, item ) ->
                Encode.list identity
                    [ Encode.int id
                    , encodeItem item
                    ]
            )


areaDecoder : Decode.Decoder Area
areaDecoder =
    Decode.map4 buildArea
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Decode.int)
        (Decode.index 3 Decode.int)


encodeArea : Area -> Encode.Value
encodeArea area =
    Encode.list Encode.int
        [ area.startRow
        , area.startCol
        , area.endRow
        , area.endCol
        ]


puzzleAreasDecoder : Decode.Decoder PuzzleAreas
puzzleAreasDecoder =
    Decode.map4 PuzzleAreas
        (Decode.field "blocks" (Decode.list areaDecoder))
        (Decode.field "boards" (Decode.list areaDecoder))
        (Decode.field "rows" (Decode.list areaDecoder))
        (Decode.field "cols" (Decode.list areaDecoder))


encodePuzzleAreas : PuzzleAreas -> Encode.Value
encodePuzzleAreas puzzleAreas =
    Encode.object
        [ ( "blocks", Encode.list encodeArea puzzleAreas.blocks )
        , ( "boards", Encode.list encodeArea puzzleAreas.boards )
        , ( "rows", Encode.list encodeArea puzzleAreas.rows )
        , ( "cols", Encode.list encodeArea puzzleAreas.cols )
        ]


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
        , ( "bundleSize", Encode.int args.bundleSize )
        , ( "difficulty", Encode.int args.difficulty )
        , ( "disabledLocations", Encode.list Encode.string args.disabledLocations )
        , ( "discoTrapRatio", Encode.int args.discoTrapRatio )
        , ( "duplicateProgression", Encode.int args.duplicateProgression )
        , ( "emojiTrapRatio", Encode.int args.emojiTrapRatio )
        , ( "numberOfBoards", Encode.int args.numberOfBoards )
        , ( "progression", encodeProgression args.progression )
        , ( "removeRandomCandidateRatio", Encode.int args.removeRandomCandidateRatio )
        , ( "seed", Encode.int args.seed )
        , ( "solveRandomCellRatio", Encode.int args.solveRandomCellRatio )
        , ( "solveSelectedCellRatio", Encode.int args.solveSelectedCellRatio )
        , ( "tunnelVisionTrapRatio", Encode.int args.tunnelVisionTrapRatio )
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


encodeItem : Item -> Encode.Value
encodeItem item =
    Encode.int (itemToId item)


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


deathLinkDecoder : Decode.Decoder DeathLink
deathLinkDecoder =
    Decode.map2 DeathLink
        (Decode.field "source" Decode.string)
        (Decode.maybe (Decode.field "cause" Decode.string))


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
                            (Decode.field "player" messagePlayerDecoder)

                    "collected" ->
                        Decode.map CollectedMessage
                            (Decode.field "player" messagePlayerDecoder)

                    "connected" ->
                        Decode.map ConnectedMessage
                            (Decode.field "player" messagePlayerDecoder)

                    "countdown" ->
                        Decode.succeed CountdownMessage

                    "disconnected" ->
                        Decode.map DisconnectedMessage
                            (Decode.field "player" messagePlayerDecoder)

                    "goaled" ->
                        Decode.map GoaledMessage
                            (Decode.field "player" messagePlayerDecoder)

                    "itemCheated" ->
                        Decode.map ItemCheatedMessage
                            (Decode.field "item" messageItemDecoder)

                    "itemHinted" ->
                        Decode.map ItemHintedMessage
                            (Decode.field "item" messageItemDecoder)

                    "itemSent" ->
                        Decode.map ItemSentMessage
                            (Decode.field "item" messageItemDecoder)

                    "released" ->
                        Decode.map ReleasedMessage
                            (Decode.field "player" messagePlayerDecoder)

                    "serverChat" ->
                        Decode.succeed ServerChatMessage

                    "tagsUpdated" ->
                        Decode.map TagsUpdatedMessage
                            (Decode.field "player" messagePlayerDecoder)

                    "tutorial" ->
                        Decode.succeed TutorialMessage

                    "userCommand" ->
                        Decode.succeed UserCommandMessage

                    _ ->
                        Decode.fail ("Unknown message type: " ++ msgType)
            )


messagePlayerDecoder : Decode.Decoder MessagePlayer
messagePlayerDecoder =
    Decode.map2 MessagePlayer
        (Decode.field "name" Decode.string)
        (Decode.field "alias" Decode.string)


messageItemDecoder : Decode.Decoder MessageItem
messageItemDecoder =
    Decode.map3 MessageItem
        (Decode.field "name" Decode.string)
        (Decode.field "sender" messagePlayerDecoder)
        (Decode.field "receiver" messagePlayerDecoder)


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
    Field.optional "deathLink" Decode.int <| \deathLink ->
    Field.optional "locationScouting" locationScoutingDecoder <| \locationScouting ->
    Field.optional "progression" progressionDecoder <| \progression ->
    Field.require "seed" Decode.int <| \seed ->
    Field.optional "bundleSize" Decode.int <| \bundleSize ->
    Field.optional "bundles" bundlesDecoder <| \bundles ->
    Field.optional "disabledLocations" (Decode.list Decode.string) <| \disabledLocations ->
    Decode.succeed
        { bundleBlocks = Maybe.withDefault Dict.empty bundles
        , bundleSize = Maybe.withDefault 1 bundleSize
        , deathLink = Maybe.withDefault 0 deathLink == 1
        , disabledLocations = Set.fromList (Maybe.withDefault [] disabledLocations)
        , locationScouting = Maybe.withDefault ScoutingManual locationScouting
        , progression = Maybe.withDefault Shuffled progression
        , seed = seed
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
        |> DecodeExtra.andMap (Decode.field "locationGameName" Decode.string)
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
                    , ( "bundle_size", yamlRecordValue <| String.fromInt model.bundleSize )
                    , ( "duplicate_progression", yamlRecordValue <| String.fromInt model.duplicateProgression )
                    , ( "disabled_locations", Yaml.Encode.list Yaml.Encode.string (Set.toList model.disabledLocationsChecked) )
                    , ( "location_scouting", yamlRecordValue <| locationScoutingToString model.locationScouting )
                    , ( "solve_selected_cell_ratio", yamlRecordValue <| String.fromInt model.solveSelectedCellRatio )
                    , ( "solve_random_cell_ratio", yamlRecordValue <| String.fromInt model.solveRandomCellRatio )
                    , ( "remove_random_candidate_ratio", yamlRecordValue <| String.fromInt model.removeRandomCandidateRatio )
                    , ( "emoji_trap_ratio", yamlRecordValue <| String.fromInt model.emojiTrapRatio )
                    , ( "disco_trap_ratio", yamlRecordValue <| String.fromInt model.discoTrapRatio )
                    , ( "tunnel_vision_trap_ratio", yamlRecordValue <| String.fromInt model.tunnelVisionTrapRatio )
                    , ( "pre_fill_nothings_percent", yamlRecordValue <| String.fromInt model.preFillNothingsPercent )
                    , ( "death_link", yamlRecordValue (if model.deathLinkInput then "true" else "false") )
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

        5 ->
            "very_hard"

        _ ->
            "unknown"


difficultyFromString : String -> Result String Int
difficultyFromString str =
    case str of
        "beginner" ->
            Ok 1

        "easy" ->
            Ok 2

        "medium" ->
            Ok 3

        "hard" ->
            Ok 4

        "very_hard" ->
            Ok 5

        _ ->
            Err ("Unknown difficulty: " ++ str)


progressionToString : Progression -> String
progressionToString progression =
    case progression of
        Fixed ->
            "fixed"

        Shuffled ->
            "shuffled"


progressionFromString : String -> Result String Progression
progressionFromString str =
    case str of
        "fixed" ->
            Ok Fixed

        "shuffled" ->
            Ok Shuffled

        _ ->
            Err ("Unknown progression: " ++ str)


locationScoutingToString : LocationScouting -> String
locationScoutingToString locationScouting =
    case locationScouting of
        ScoutingAuto ->
            "auto"

        ScoutingManual ->
            "manual"

        ScoutingDisabled ->
            "disabled"


locationScoutingFromString : String -> Result String LocationScouting
locationScoutingFromString str =
    case str of
        "auto" ->
            Ok ScoutingAuto

        "manual" ->
            Ok ScoutingManual

        "disabled" ->
            Ok ScoutingDisabled

        _ ->
            Err ("Unknown location scouting: " ++ str)


decodeYamlOptionString : Yaml.Decode.Decoder String
decodeYamlOptionString =
    Yaml.Decode.oneOf
        [ Yaml.Decode.string
        , Yaml.Decode.int
            |> Yaml.Decode.map String.fromInt
        , Yaml.Decode.dict Yaml.Decode.value
            |> Yaml.Decode.map Dict.keys
            |> yamlAndThenMaybe List.head "Empty record"
        ]


decodeYamlOptionInt : Yaml.Decode.Decoder Int
decodeYamlOptionInt =
    decodeYamlOptionString
        |> Yaml.Decode.andThen
            (\s ->
                case String.toInt s of
                    Just n ->
                        Yaml.Decode.succeed n

                    Nothing ->
                        Yaml.Decode.fail ("Not an int: " ++ s)
            )


yamlAndThenMaybe : (a -> Maybe b) -> String -> Yaml.Decode.Decoder a -> Yaml.Decode.Decoder b
yamlAndThenMaybe maybeFunct errorMsg decoder =
    decoder
        |> Yaml.Decode.andThen
            (\value ->
                case maybeFunct value of
                    Just result ->
                        Yaml.Decode.succeed result

                    Nothing ->
                        Yaml.Decode.fail errorMsg
            )


yamlAndThenResult : (a -> Result String b) -> Yaml.Decode.Decoder a -> Yaml.Decode.Decoder b
yamlAndThenResult resultFunct decoder =
    decoder
        |> Yaml.Decode.andThen
            (\value ->
                case resultFunct value of
                    Ok result ->
                        Yaml.Decode.succeed result

                    Err err ->
                        Yaml.Decode.fail err
            )


decodeOptionsYaml : Yaml.Decode.Decoder YamlOptions
decodeOptionsYaml =
    let
        maybeField : String -> Yaml.Decode.Decoder a -> Yaml.Decode.Decoder (Maybe a)
        maybeField key decoder =
            Yaml.Decode.maybe (Yaml.Decode.field key decoder)

        apdkField : String -> Yaml.Decode.Decoder a -> Yaml.Decode.Decoder (Maybe a)
        apdkField key decoder =
            Yaml.Decode.maybe (Yaml.Decode.at [ "Archipeladoku", key ] decoder)

        yamlDifficultyDecoder : Yaml.Decode.Decoder Int
        yamlDifficultyDecoder =
            decodeYamlOptionString
                |> yamlAndThenResult difficultyFromString

        yamlProgressionDecoder : Yaml.Decode.Decoder Progression
        yamlProgressionDecoder =
            decodeYamlOptionString
                |> yamlAndThenResult progressionFromString

        yamlLocationScoutingDecoder : Yaml.Decode.Decoder LocationScouting
        yamlLocationScoutingDecoder =
            decodeYamlOptionString
                |> yamlAndThenResult locationScoutingFromString
    in
    Yaml.Decode.succeed YamlOptions
        |> Yaml.Decode.andMap (maybeField "name" Yaml.Decode.string)
        |> Yaml.Decode.andMap (apdkField "block_size" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "boards_per_cluster" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "number_of_boards" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "difficulty" yamlDifficultyDecoder)
        |> Yaml.Decode.andMap (apdkField "progression" yamlProgressionDecoder)
        |> Yaml.Decode.andMap (apdkField "duplicate_progression" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "bundle_size" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "disabled_locations" (Yaml.Decode.list Yaml.Decode.string |> Yaml.Decode.map Set.fromList))
        |> Yaml.Decode.andMap (apdkField "location_scouting" yamlLocationScoutingDecoder)
        |> Yaml.Decode.andMap (apdkField "solve_selected_cell_ratio" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "solve_random_cell_ratio" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "remove_random_candidate_ratio" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "emoji_trap_ratio" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "disco_trap_ratio" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "tunnel_vision_trap_ratio" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "pre_fill_nothings_percent" decodeYamlOptionInt)
        |> Yaml.Decode.andMap (apdkField "progression_balancing" decodeYamlOptionInt)


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
        , ( "tunnelVisionTrap", Encode.bool (model.tunnelVisionTrapTimer > 0) )
        , ( "fireworks", Encode.bool (model.fireworksTimer > 0) )
        , ( "animationsEnabled", Encode.bool model.animationsEnabled )
        , ( "candidateLayout", Encode.int model.candidateLayout )
        , ( "deathLinkTriggers", Encode.int model.deathLinkTriggers )
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
    if cellIsVisible model cell && not (cellIsHiddenByTunnelVision model cell) then
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
                                if Set.isEmpty model.highlightedNumbers then
                                    Encode.bool False

                                else
                                    Set.intersect nums model.highlightedNumbers
                                        |> Set.isEmpty
                                        |> xor (Set.isEmpty model.highlightedNumbers)
                                        |> Encode.bool
                      )
                    , ( "dimmedNumbers"
                      , case model.highlightMode of
                            HighlightNumber ->
                                if Set.isEmpty model.highlightedNumbers then
                                    Encode.list Encode.int []

                                else
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

    else if not (cellIsVisible model cell) then
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

    else
        Encode.object
            [ ( "type", Encode.string "tunnel" )
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


encodeSavedGame : Time.Posix -> Model -> Encode.Value
encodeSavedGame timestamp model =
    let
        cells : PackedBoardCells
        cells =
            packBoardCells model
    in
    Encode.object
        [ ( "blockSize", Encode.int model.blockSize )
        , ( "bundleBlocks", encodeBundleBlocks model.bundleBlocks )
        , ( "bundleSize", Encode.int model.bundleSize )
        , ( "coordinates", Encode.list Encode.int cells.coordinates )
        , ( "current", Encode.list Encode.int cells.current )
        , ( "disabledLocations", Encode.set Encode.string model.disabledLocations )
        , ( "discoTrapTriggers", Encode.int model.discoTrapTriggers )
        , ( "emojiTrapTriggers", Encode.int model.emojiTrapTriggers )
        , ( "gameIsLocal", Encode.bool model.gameIsLocal )
        , ( "locationScouting", Encode.string (locationScoutingToString model.locationScouting) )
        , ( "lockedBlocks", Encode.list (encodeTuple Encode.int Encode.int) model.lockedBlocks )
        , ( "progression", Encode.string (progressionToString model.progression) )
        , ( "puzzleAreas", encodePuzzleAreas model.puzzleAreas )
        , ( "removeRandomCandidateReceived", Encode.int model.removeRandomCandidateReceived )
        , ( "removeRandomCandidateUsed", Encode.int model.removeRandomCandidateUsed )
        , ( "seed", Encode.int model.seedInput )
        , ( "solution", Encode.list Encode.int cells.solution )
        , ( "solveRandomCellReceived", Encode.int model.solveRandomCellReceived )
        , ( "solveRandomCellUsed", Encode.int model.solveRandomCellUsed )
        , ( "solveSelectedCellReceived", Encode.int model.solveSelectedCellReceived )
        , ( "solveSelectedCellUsed", Encode.int model.solveSelectedCellUsed )
        , ( "solvedLocations", Encode.list Encode.int (Set.toList model.solvedLocations) )
        , ( "timestamp", Encode.int (Time.posixToMillis timestamp) )
        , ( "tunnelVisionTrapTriggers", Encode.int model.tunnelVisionTrapTriggers )
        , ( "unlockMap", encodeUnlockMap model.unlockMap )
        , ( "unlockedBlocks", Encode.list (encodeTuple Encode.int Encode.int) (Set.toList model.unlockedBlocks) )
        ]


savedGameDecoder : Decode.Decoder SavedGame
savedGameDecoder =
    Field.require "blockSize" Decode.int <| \blockSize ->
    Field.optional "bundleBlocks" bundleBlocksSaveDecoder <| \bundleBlocks ->
    Field.optional "bundleSize" Decode.int <| \bundleSize ->
    Field.require "coordinates" (Decode.list Decode.int) <| \coordinates ->
    Field.require "current" (Decode.list Decode.int) <| \current ->
    Field.optional "disabledLocations" (Decode.list Decode.string) <| \disabledLocations ->
    Field.require "discoTrapTriggers" Decode.int <| \discoTrapTriggers ->
    Field.require "emojiTrapTriggers" Decode.int <| \emojiTrapTriggers ->
    Field.require "gameIsLocal" Decode.bool <| \gameIsLocal ->
    Field.require "locationScouting" locationScoutingDecoder <| \locationScouting ->
    Field.require "lockedBlocks" (Decode.list (tupleDecoder Decode.int Decode.int)) <| \lockedBlocks ->
    Field.require "progression" progressionDecoder <| \progression ->
    Field.require "puzzleAreas" puzzleAreasDecoder <| \puzzleAreas ->
    Field.require "seed" Decode.int <| \seed ->
    Field.require "removeRandomCandidateReceived" Decode.int <| \removeRandomCandidateReceived ->
    Field.require "removeRandomCandidateUsed" Decode.int <| \removeRandomCandidateUsed ->
    Field.require "solution" (Decode.list Decode.int) <| \solution ->
    Field.require "solveRandomCellReceived" Decode.int <| \solveRandomCellReceived ->
    Field.require "solveRandomCellUsed" Decode.int <| \solveRandomCellUsed ->
    Field.require "solveSelectedCellReceived" Decode.int <| \solveSelectedCellReceived ->
    Field.require "solveSelectedCellUsed" Decode.int <| \solveSelectedCellUsed ->
    Field.require "solvedLocations" (Decode.list Decode.int) <| \solvedLocations ->
    Field.require "timestamp" (Decode.map Time.millisToPosix Decode.int) <| \timestamp ->
    Field.optional "tunnelVisionTrapTriggers" Decode.int <| \tunnelVisionTrapTriggers ->
    Field.require "unlockMap" unlockMapDecoder <| \unlockMap ->
    Field.require "unlockedBlocks" (Decode.list (tupleDecoder Decode.int Decode.int)) <| \unlockedBlocks ->
    Decode.succeed
        { blockSize = blockSize
        , bundleBlocks = Maybe.withDefault Dict.empty bundleBlocks
        , bundleSize = Maybe.withDefault 1 bundleSize
        , coordinates = coordinates
        , current = current
        , disabledLocations = Set.fromList (Maybe.withDefault [] disabledLocations)
        , discoTrapTriggers = discoTrapTriggers
        , emojiTrapTriggers = emojiTrapTriggers
        , gameIsLocal = gameIsLocal
        , locationScouting = locationScouting
        , lockedBlocks = lockedBlocks
        , progression = progression
        , puzzleAreas = puzzleAreas
        , removeRandomCandidateReceived = removeRandomCandidateReceived
        , removeRandomCandidateUsed = removeRandomCandidateUsed
        , seed = seed
        , solution = solution
        , solveRandomCellReceived = solveRandomCellReceived
        , solveRandomCellUsed = solveRandomCellUsed
        , solveSelectedCellReceived = solveSelectedCellReceived
        , solveSelectedCellUsed = solveSelectedCellUsed
        , solvedLocations = Set.fromList solvedLocations
        , timestamp = timestamp
        , tunnelVisionTrapTriggers = Maybe.withDefault 0 tunnelVisionTrapTriggers
        , unlockMap = unlockMap
        , unlockedBlocks = Set.fromList unlockedBlocks
        }


packBoardCells : Model -> PackedBoardCells
packBoardCells model =
    Dict.foldl
        (\( row, col ) solution acc ->
            { acc
                | coordinates = packCoordinate ( row, col ) :: acc.coordinates
                , current = packCellValue (getCellValue model ( row, col )) :: acc.current
                , solution = solution :: acc.solution
            }
        )
        { coordinates = []
        , current = []
        , solution = []
        }
        model.solution


unpackBoardCells : PackedBoardCells -> UnpackedBoardCells
unpackBoardCells packedCells =
    List.foldl
        (\( packedCoordinate, packedCurrent, solution ) acc ->
            let
                coordinate : ( Int, Int )
                coordinate =
                    unpackCoordinate packedCoordinate

                current : Maybe CellValue
                current =
                    unpackCellValue packedCurrent
            in
            case current of
                Just (Given n) ->
                    { current = acc.current
                    , givens = Set.insert coordinate acc.givens
                    , solution = Dict.insert coordinate solution acc.solution
                    }

                Just (Single n) ->
                    { current = Dict.insert coordinate (Single n) acc.current
                    , givens = acc.givens
                    , solution = Dict.insert coordinate solution acc.solution
                    }

                Just (Multiple nums) ->
                    { current = Dict.insert coordinate (Multiple nums) acc.current
                    , givens = acc.givens
                    , solution = Dict.insert coordinate solution acc.solution
                    }

                Nothing ->
                    { current = acc.current
                    , givens = acc.givens
                    , solution = Dict.insert coordinate solution acc.solution
                    }
        )
        { current = Dict.empty
        , givens = Set.empty
        , solution = Dict.empty
        }
        (List.Extra.zip3 packedCells.coordinates packedCells.current packedCells.solution)


maxBoardWidth : Int
maxBoardWidth =
    180


packCoordinate : ( Int, Int ) -> Int
packCoordinate ( row, col ) =
    row * maxBoardWidth + col


unpackCoordinate : Int -> ( Int, Int )
unpackCoordinate packedCoordinate =
    ( packedCoordinate // maxBoardWidth, modBy maxBoardWidth packedCoordinate )


packCellValue : Maybe CellValue -> Int
packCellValue cellValue =
    case cellValue of
        Just (Given n) ->
            Bitwise.or (Bitwise.shiftLeftBy 2 n) 1

        Just (Single n) ->
            Bitwise.or (Bitwise.shiftLeftBy 2 n) 2

        Just (Multiple nums) ->
            let
                mask : Int
                mask =
                    List.foldl
                        (\n -> Bitwise.or (Bitwise.shiftLeftBy (n - 1) 1))
                        0
                        (Set.toList nums)
            in
            Bitwise.or (Bitwise.shiftLeftBy 2 mask) 3

        Nothing ->
            0


unpackCellValue : Int -> Maybe CellValue
unpackCellValue packedValue =
    let
        tag : Int
        tag =
            Bitwise.and packedValue 3

        data : Int
        data =
            Bitwise.shiftRightZfBy 2 packedValue
    in
    case tag of
        1 ->
            Just ( Given data )

        2 ->
            Just ( Single data )

        3 ->
            let
                nums : Set Int
                nums =
                    List.range 1 16
                        |> List.filter (\n -> Bitwise.and data (Bitwise.shiftLeftBy (n - 1) 1) /= 0)
                        |> Set.fromList
            in
            Just ( Multiple nums )

        _ ->
            Nothing


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

        "apdk-candidate-layout" ->
            ( { model
                | candidateLayout = candidateLayoutFromString value
              }
            , Cmd.none
            )

        "apdk-color-scheme" ->
            ( { model | colorScheme = value }
            , Cmd.none
            )

        "apdk-connection-history" ->
            ( { model
                | connectionHistory =
                    Decode.decodeString connectionHistoryDecoder value
                        |> Result.withDefault []
              }
            , Cmd.none
            )

        "apdk-create-hints-when-scouting" ->
            ( { model | createHintsWhenScouting = value == "1" }
            , Cmd.none
            )

        "apdk-emoji-trap-variant" ->
            ( { model | emojiTrapVariant = emojiTrapVariantFromString value }
            , Cmd.none
            )

        "apdk-firework-on-nothing" ->
            ( { model | fireworkOnNothing = value == "1" }
            , Cmd.none
            )

        "apdk-host" ->
            ( { model | host = value }
            , Cmd.none
            )

        "apdk-keybindings" ->
            ( { model
                | keyBindings =
                    keyBindingsFromString value
                        |> Maybe.map mergeSavedKeyBindings
                        |> Maybe.withDefault model.keyBindings
              }
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

        "apdk-show-input-errors" ->
            ( { model | showInputErrors = value == "1" }
            , Cmd.none
            )

        "apdk-show-toast-messages" ->
            ( { model | showToastMessages = value == "1" }
            , Cmd.none
            )

        "apdk-trap-duration" ->
            case String.toInt value of
                Just duration ->
                    if duration > 0 then
                        ( { model | trapDuration = duration }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

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
                        |> List.concatMap .cells
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
                            |> List.concatMap .cells
                            |> Set.fromList
                        , Dict.get model.selectedCell model.cellRows
                            |> Maybe.withDefault []
                            |> List.concatMap .cells
                            |> Set.fromList
                        , Dict.get model.selectedCell model.cellCols
                            |> Maybe.withDefault []
                            |> List.concatMap .cells
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
    if Set.member "inputModifier" model.heldKeys then
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


andThenIf : Bool -> (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThenIf condition fun ( model, cmd ) =
    if condition then
        andThen fun ( model, cmd )

    else
        ( model, cmd )


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


updateState : Bool -> Model -> ( Model, Cmd Msg )
updateState triggerAnimations model =
    case model.gameState of
        Playing ->
            if model.autoRemoveInvalidCandidates then
                applySteps
                    [ updateStateApplyServerChecks
                    , updateStateChanges triggerAnimations
                    , updateStateErrors
                    , updateStateRemoveInvalidCandidates
                    , updateStateDropCandidateErrors
                    , updateStateScoutLocations
                    , updateStateGoal
                    , updateStateHighlight
                    , updateStateSaveGame
                    , updateBoardData
                    ]
                    model

            else
                applySteps
                    [ updateStateApplyServerChecks
                    , updateStateChanges triggerAnimations
                    , updateStateErrors
                    , updateStateScoutLocations
                    , updateStateGoal
                    , updateStateHighlight
                    , updateStateSaveGame
                    , updateBoardData
                    ]
                    model

        _ ->
            ( model, Cmd.none )


pendingServerSolves : Model -> Set Int
pendingServerSolves model =
    model.serverCheckedLocations
        |> Set.filter (\id -> id >= 1000000 && id < 4000000)
        |> (\ids -> Set.diff ids model.solvedLocations)


queueServerSolves : Model -> Model
queueServerSolves model =
    let
        cellsInRange : Int -> Int -> Set ( Int, Int )
        cellsInRange low high =
            pendingServerSolves model
                |> Set.toList
                |> List.filterMap
                    (\id ->
                        if id >= low && id < high then
                            Just (cellFromId id)

                        else
                            Nothing
                    )
                |> Set.fromList
    in
    { model
        | pendingSolvedBlocks = Set.union model.pendingSolvedBlocks (cellsInRange 1000000 2000000)
        , pendingSolvedRows = Set.union model.pendingSolvedRows (cellsInRange 2000000 3000000)
        , pendingSolvedCols = Set.union model.pendingSolvedCols (cellsInRange 3000000 4000000)
    }


updateStateApplyServerChecks : Model -> ( Model, Cmd Msg )
updateStateApplyServerChecks model =
    if model.autoApplyServerChecks then
        ( queueServerSolves model
        , Cmd.none
        )

    else
        ( model
        , Cmd.none
        )


updateStateItems : Bool -> Model -> ( Model, Cmd Msg )
updateStateItems triggerAnimations model =
    ( { model | pendingItems = [] }
    , Cmd.none
    )
        |> applyList (updateStateItem triggerAnimations) model.pendingItems


updateStateChanges : Bool -> Model -> ( Model, Cmd Msg )
updateStateChanges triggerAnimations model =
    updateStateChangesLoop triggerAnimations ( model, Cmd.none )


updateStateChangesLoop : Bool -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateStateChangesLoop triggerAnimations ( model, cmd ) =
    if Set.isEmpty model.pendingCellChanges
        && List.isEmpty model.pendingItems
        && Set.isEmpty model.pendingSolvedBlocks
        && Set.isEmpty model.pendingSolvedBoards
        && Set.isEmpty model.pendingSolvedCols
        && Set.isEmpty model.pendingSolvedRows
    then
        ( model, cmd )

    else
        let
            ( newModel, newCmd ) =
                applySteps
                    [ updateStateItems triggerAnimations
                    , updateStateCellChanges
                    , updateStateSolvedAreas triggerAnimations
                    , updateStateCheckLocations
                    ]
                    model
        in
        updateStateChangesLoop
            triggerAnimations
            ( newModel
            , Cmd.batch [ cmd, newCmd ]
            )


updateStateCellChanges : Model -> ( Model, Cmd Msg )
updateStateCellChanges model =
    ( { model | pendingCellChanges = Set.empty }
    , Cmd.none
    )
        |> applySet updateStateCellChange model.pendingCellChanges


updateStateSolvedAreas : Bool -> Model -> ( Model, Cmd Msg )
updateStateSolvedAreas triggerAnimations model =
    ( { model
        | pendingSolvedBlocks = Set.empty
        , pendingSolvedBoards = Set.empty
        , pendingSolvedCols = Set.empty
        , pendingSolvedRows = Set.empty
      }
    , Cmd.none
    )
        |> applySet (updateStateSolvedArea triggerAnimations model.cellBlocks cellToBlockId) model.pendingSolvedBlocks
        |> applySet (updateStateSolvedArea triggerAnimations model.cellRows cellToRowId) model.pendingSolvedRows
        |> applySet (updateStateSolvedArea triggerAnimations model.cellCols cellToColId) model.pendingSolvedCols
        |> applySet (updateStateSolvedBoard triggerAnimations model.cellBoards) model.pendingSolvedBoards


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


updateStateDropCandidateErrors : Model -> ( Model, Cmd Msg )
updateStateDropCandidateErrors model =
    ( { model
        | errors =
            Dict.filter
                (\_ error ->
                    case error of
                        NumberError _ ->
                            True

                        CandidateErrors _ ->
                            False
                )
                model.errors
      }
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
                scoutLocalLocations model model.pendingScoutLocations

            else
                model.scoutedItems
      }
    , if not model.gameIsLocal && model.locationScouting == ScoutingAuto then
        scoutLocations
            { locations = Set.toList model.pendingScoutLocations
            , createHint = model.createHintsWhenScouting
            }

      else
        Cmd.none
    )


addKnownItems : List Hint -> Model -> Model
addKnownItems hints model =
    { model
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


markHintedLocations : List Hint -> Model -> Model
markHintedLocations hints model =
    { model
        | hintedLocations =
            List.foldl
                (\item acc ->
                    if item.senderName == model.player then
                        Set.insert item.locationId acc

                    else
                        acc
                )
                model.hintedLocations
                hints
    }


scoutLocalLocations : Model -> Set Int -> Dict Int Hint
scoutLocalLocations model ids =
    Set.foldl
        (\id scoutedItems ->
            case Dict.get id model.unlockMap of
                Just item ->
                    Dict.insert id (createHint id item) scoutedItems

                Nothing ->
                    scoutedItems
        )
        model.scoutedItems
        ids


updateStateGoal : Model -> ( Model, Cmd Msg )
updateStateGoal model =
    if List.all (cellIsSolved model) (Dict.keys model.solution) then
        ( { model | fireworksTimer = 60 }
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


updateStateSaveGame : Model -> ( Model, Cmd Msg )
updateStateSaveGame model =
    ( model
    , Task.perform (GotSaveGameTime True) Time.now
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
                    && List.all (cellIsSolved model) block.cells
                    && List.all (cellIsVisible model) block.cells
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
            (if Set.member "blocks" initialModel.disabledLocations then
                []

             else
                Dict.get updatedCell initialModel.cellBlocks
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
                    && List.all (cellIsSolved model) row.cells
                    && List.all (cellIsVisible model) row.cells
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
            (if Set.member "rows" initialModel.disabledLocations then
                []

             else
                Dict.get updatedCell initialModel.cellRows
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
                    && List.all (cellIsSolved model) col.cells
                    && List.all (cellIsVisible model) col.cells
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
            (if Set.member "columns" initialModel.disabledLocations then
                []

             else
                Dict.get updatedCell initialModel.cellCols
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
                    && List.all (cellIsSolved model) board.cells
                    && List.all (cellIsVisible model) board.cells
                then
                    ( { model
                        | pendingCheckLocations =
                            Set.insert boardId model.pendingCheckLocations
                        , pendingSolvedBoards =
                            Set.insert
                                ( board.startRow, board.startCol )
                                model.pendingSolvedBoards
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
            (if Set.member "boards" initialModel.disabledLocations then
                []

             else
                Dict.get updatedCell initialModel.cellBoards
                    |> Maybe.withDefault []
            )


unlockInitialBlocks : Model -> ( Model, Cmd Msg )
unlockInitialBlocks model =
    let
        lockedBlocksSet : Set ( Int, Int )
        lockedBlocksSet =
            Set.fromList model.lockedBlocks
    in
    applyList
        (unlockBlock False)
        (List.filterMap
            (\block ->
                if (block.endRow <= model.blockSize && block.endCol <= model.blockSize)
                    || (not (Set.member ( block.startRow, block.startCol ) lockedBlocksSet))
                then
                    Just ( block.startRow, block.startCol )

                else
                    Nothing
            )
            model.puzzleAreas.blocks
        )
        ( model, Cmd.none )


unlockNextBlock : Model -> ( Model, Cmd Msg )
unlockNextBlock model =
    case List.Extra.find (\block -> not (Set.member block model.unlockedBlocks)) model.lockedBlocks of
        Just block ->
            unlockBlock True block model

        Nothing ->
            ( model
            , Cmd.none
            )


unlockBlock : Bool -> ( Int, Int ) -> Model -> ( Model, Cmd Msg )
unlockBlock triggerAnimations block model =
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
                |> Maybe.map .cells
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
                            area.cells
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
        | pendingCellChanges = Set.union blockCells model.pendingCellChanges
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
        |> addLocalMessage
            model.gameIsLocal
            (String.concat
                [ "Unlocked Block "
                , rowToLabel (Tuple.first block)
                , String.fromInt (Tuple.second block)
                ]
            )
    , if triggerAnimations && model.animationsEnabled then
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
                    area.cells

                placedByValue : Dict Int (Set ( Int, Int ))
                placedByValue =
                    List.foldl
                        (\areaCell dict ->
                            if Set.member areaCell model.visibleCells then
                                case getCellValue model areaCell |> Maybe.andThen cellValueToInt of
                                    Just number ->
                                        Dict.update
                                            number
                                            (Maybe.withDefault Set.empty >> Set.insert areaCell >> Just)
                                            dict

                                    Nothing ->
                                        dict

                            else
                                dict
                        )
                        Dict.empty
                        areaCells

                getConflictingCells : ( Int, Int ) -> Int -> Set ( Int, Int )
                getConflictingCells cell number =
                    Dict.get number placedByValue
                        |> Maybe.withDefault Set.empty
                        |> Set.remove cell
            in
            List.foldl
                (\cell acc ->
                    case getCellValue model cell of
                        Just (Given v) ->
                            acc

                        Just (Single v) ->
                            let
                                conflictingCells : Set ( Int, Int )
                                conflictingCells =
                                    getConflictingCells cell v
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
                                                        getConflictingCells cell number
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


updateStateItem : Bool -> Item -> Model -> ( Model, Cmd Msg )
updateStateItem triggerAnimations item model =
    case item of
        ProgressiveBlock ->
            applyList
                (\_ -> unlockNextBlock)
                (List.range 1 (clamp 1 model.blockSize model.bundleSize))
                ( model, Cmd.none )

        Block block ->
            unlockBlock True block model

        BlockBundle index ->
            applyList
                (unlockBlock True)
                (Dict.get index model.bundleBlocks |> Maybe.withDefault [])
                ( model, Cmd.none )

        SolveSelectedCell ->
            ( { model | solveSelectedCellReceived = model.solveSelectedCellReceived + 1 }
                |> addLocalMessage model.gameIsLocal "Unlocked a Solve Selected Cell."
            , Cmd.none
            )

        SolveRandomCell ->
            ( { model | solveRandomCellReceived = model.solveRandomCellReceived + 1 }
                |> addLocalMessage model.gameIsLocal "Unlocked a Solve Random Cell."
            , Cmd.none
            )

        RemoveRandomCandidate ->
            ( { model | removeRandomCandidateReceived = model.removeRandomCandidateReceived + 1 }
                |> addLocalMessage model.gameIsLocal "Unlocked a Remove Random Candidate."
            , Cmd.none
            )

        DiscoTrap ->
            ( { model | discoTrapReceived = model.discoTrapReceived + 1 }
                |> addLocalMessage model.gameIsLocal "Unlocked a Disco Trap."
            , Cmd.none
            )
                |> andThenIf (model.discoTrapReceived >= model.discoTrapTriggers) triggerDiscoTrap

        EmojiTrap ->
            ( { model | emojiTrapReceived = model.emojiTrapReceived + 1 }
                |> addLocalMessage model.gameIsLocal "Unlocked an Emoji Trap."
            , Cmd.none
            )
                |> andThenIf (model.emojiTrapReceived >= model.emojiTrapTriggers) triggerEmojiTrap

        TunnelVisionTrap ->
            ( { model | tunnelVisionTrapReceived = model.tunnelVisionTrapReceived + 1 }
                |> addLocalMessage model.gameIsLocal "Unlocked a Tunnel Vision Trap."
            , Cmd.none
            )
                |> andThenIf (model.tunnelVisionTrapReceived >= model.tunnelVisionTrapTriggers) triggerTunnelVisionTrap

        NothingItem ->
            ( model
            , if triggerAnimations && model.animationsEnabled && model.fireworkOnNothing then
                triggerFirework ()

              else
                Cmd.none
            )


updateStateSolvedArea :
    Bool
    -> Dict ( Int, Int ) (List Area)
    -> (( Int, Int ) -> Int)
    -> ( Int, Int )
    -> Model
    -> ( Model, Cmd Msg )
updateStateSolvedArea triggerAnimations cellAreas toId ( row, col ) model =
    let
        cells : List ( Int, Int )
        cells =
            Dict.get ( row, col ) cellAreas
                |> Maybe.withDefault []
                |> List.Extra.find (\area -> area.startRow == row && area.startCol == col)
                |> Maybe.map .cells
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
    , if triggerAnimations && model.animationsEnabled then
        triggerAnimation (encodeTriggerAnimation "shine" cells)

      else
        Cmd.none
    )


updateStateSolvedBoard :
    Bool
    -> Dict ( Int, Int ) (List Area)
    -> ( Int, Int )
    -> Model
    -> ( Model, Cmd Msg )
updateStateSolvedBoard triggerAnimations cellAreas ( row, col ) model =
    let
        cells : List ( Int, Int )
        cells =
            Dict.get ( row, col ) cellAreas
                |> Maybe.withDefault []
                |> List.Extra.find (\area -> area.startRow == row && area.startCol == col)
                |> Maybe.map .cells
                |> Maybe.withDefault []
    in
    ( { model | solvedLocations = Set.insert (cellToBoardId ( row, col )) model.solvedLocations }
    , if triggerAnimations && model.animationsEnabled then
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


cellIsHiddenByTunnelVision : Model -> ( Int, Int ) -> Bool
cellIsHiddenByTunnelVision model ( row, col ) =
    if model.tunnelVisionTrapTimer <= 0 then
        False

    else
        let
            rows : Int
            rows =
                abs (Tuple.first model.selectedCell - row)

            cols : Int
            cols =
                abs (Tuple.second model.selectedCell - col)

            ( limitOne, limitCombined ) =
                tunnelVisionLimit model.blockSize
        in
        rows > limitOne || cols > limitOne || (rows + cols) > limitCombined


tunnelVisionLimit : Int -> ( Int, Int )
tunnelVisionLimit blockSize =
    case blockSize of
        4 ->
            ( 1, 2 )

        6 ->
            ( 2, 3 )

        8 ->
            ( 2, 3 )

        9 ->
            ( 2, 3 )

        12 ->
            ( 3, 4 )

        16 ->
            ( 3, 5 )

        _ ->
            ( 2, 3 )


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
                    area.cells
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


blockBundleBaseId : Int
blockBundleBaseId =
    1000


maxBundles : Int
maxBundles =
    450


locationReenableOrder : List String
locationReenableOrder =
    [ "boards", "blocks", "columns", "rows" ]


progressionDensityCap : Progression -> Float
progressionDensityCap progression =
    case progression of
        Fixed ->
            0.8

        Shuffled ->
            0.6


resolveDisabledLocations : Set String -> Dict String Int -> Int -> Float -> Set String
resolveDisabledLocations requested locationCounts progressionItemCount cap =
    let
        enabledLocations : Set String -> Int
        enabledLocations effective =
            Dict.foldl
                (\typ count sum ->
                    if Set.member typ effective then
                        sum

                    else
                        sum + count
                )
                0
                locationCounts

        -- A board is the highest-sphere location (the whole board must be solved to check it), so
        -- boards alone are terrible progression seeds. Require at least one non-board type enabled.
        hasSeedType : Set String -> Bool
        hasSeedType effective =
            Dict.keys locationCounts
                |> List.any (\typ -> typ /= "boards" && not (Set.member typ effective))

        step : String -> Set String -> Set String
        step typ effective =
            if hasSeedType effective && toFloat progressionItemCount <= cap * toFloat (enabledLocations effective) then
                effective

            else
                Set.remove typ effective
    in
    List.foldl
        step
        (Set.intersect requested (Set.fromList (Dict.keys locationCounts)))
        locationReenableOrder


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


addConnectionHistory : Model -> ( Model, Cmd Msg )
addConnectionHistory model =
    let
        newHistory : List ConnectionHistoryEntry
        newHistory =
            { host = model.host
            , password = model.password
            , player = model.player
            }
                :: List.filter
                    (\e -> not (e.host == model.host && e.player == model.player))
                    model.connectionHistory
                |> List.take 5
    in
    ( { model | connectionHistory = newHistory }
    , setLocalStorage
        ( "apdk-connection-history"
        , Encode.encode 0 (encodeConnectionHistory newHistory)
        )
    )


addMessage : Message -> Model -> Model
addMessage message model =
    { model
        | messageCounter = model.messageCounter + 1
        , messages = (message :: model.messages) |> List.take maxMessages
        , toastMessages =
            if showAsToast model.player message then
                { id = model.messageCounter
                , message = message
                , timer = toastDuration
                }
                    :: model.toastMessages
                    |> List.take maxToastMessages

            else
                model.toastMessages
    }


addLocalMessage : Bool -> String -> Model -> Model
addLocalMessage condition text =
    if condition then
        addMessage
            { nodes = [ TextualMessageNode text ]
            , extra = LocalMessage
            }

    else
        identity


showAsToast : String -> Message -> Bool
showAsToast player message =
    case message.extra of
        AdminCommandMessage ->
            False

        ChatMessage _ ->
            True

        CollectedMessage messagePlayer ->
            messagePlayer.name == player

        ConnectedMessage messagePlayer ->
            messagePlayer.name == player

        CountdownMessage ->
            True

        DisconnectedMessage messagePlayer ->
            messagePlayer.name == player

        GoaledMessage messagePlayer ->
            messagePlayer.name == player

        ItemCheatedMessage item ->
            item.sender.name == player || item.receiver.name == player

        ItemHintedMessage item ->
            item.sender.name == player || item.receiver.name == player

        ItemSentMessage item ->
            item.sender.name == player || item.receiver.name == player

        LocalMessage ->
            True

        ReleasedMessage messagePlayer ->
            messagePlayer.name == player

        ServerChatMessage ->
            True

        TagsUpdatedMessage messagePlayer ->
            messagePlayer.name == player

        TutorialMessage ->
            False

        UserCommandMessage ->
            False


maxMessages : Int
maxMessages =
    500


toastDuration : Int
toastDuration =
    5


maxToastMessages : Int
maxToastMessages =
    8


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


buildArea : Int -> Int -> Int -> Int -> Area
buildArea startRow startCol endRow endCol =
    { startRow = startRow
    , startCol = startCol
    , endRow = endRow
    , endCol = endCol
    , cells =
        List.concatMap
            (\row ->
                List.map (Tuple.pair row) (List.range startCol endCol)
            )
            (List.range startRow endRow)
    }


buildCellAreasMap : List Area -> Dict ( Int, Int ) (List Area)
buildCellAreasMap areas =
    List.foldl
        (\area acc ->
            let
                areaCells : List ( Int, Int )
                areaCells =
                    area.cells
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

    else if id > blockBundleBaseId && id <= blockBundleBaseId + maxBundles then
        BlockBundle (id - blockBundleBaseId)

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

    else if id == 403 then
        TunnelVisionTrap

    else
        NothingItem


itemToId : Item -> Int
itemToId item =
    case item of
        Block block ->
            cellToBlockId block

        BlockBundle index ->
            blockBundleBaseId + index

        SolveRandomCell ->
            1

        RemoveRandomCandidate ->
            2

        ProgressiveBlock ->
            101

        SolveSelectedCell ->
            201

        EmojiTrap ->
            401

        DiscoTrap ->
            402

        TunnelVisionTrap ->
            403

        NothingItem ->
            99


fillerItemSortOrder : List Item
fillerItemSortOrder =
    [ SolveSelectedCell
    , SolveRandomCell
    , RemoveRandomCandidate
    , DiscoTrap
    , EmojiTrap
    , TunnelVisionTrap
    , NothingItem
    ]


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
    , locationGameName = ""
    , itemId = itemToId item
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

        BlockBundle index ->
            "Block Bundle " ++ String.fromInt index

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

        TunnelVisionTrap ->
            "Tunnel Vision Trap"

        NothingItem ->
            "Nothing"


itemClassification : Item -> ItemClass
itemClassification item =
    case item of
        ProgressiveBlock ->
            Progression

        Block _ ->
            Progression

        BlockBundle _ ->
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

        TunnelVisionTrap ->
            Trap

        NothingItem ->
            Filler


timers : Model -> List Int
timers model =
    [ model.discoTrapTimer
    , model.emojiTrapTimer
    , model.fireworksTimer
    , model.tunnelVisionTrapTimer
    ]


triggerDiscoTrap : Model -> ( Model, Cmd Msg )
triggerDiscoTrap model =
    ( { model
        | discoTrapOffset = 0
        , discoTrapTimer = model.trapDuration
        , discoTrapTriggers = model.discoTrapTriggers + 1
      }
    , Cmd.none
    )
        |> andThen updateDiscoTrapMap


updateDiscoTrapMap : Model -> ( Model, Cmd Msg )
updateDiscoTrapMap model =
    let
        step : Int
        step =
            if model.blockSize >= 12 then
                2

            else
                1

        newOffset : Int
        newOffset =
            modBy model.blockSize (model.discoTrapOffset + step)

        discoTrapMap : Dict Int Int
        discoTrapMap =
            List.map
                (\number ->
                    ( number, modBy model.blockSize (number + newOffset) + 1 )
                )
                (List.range 1 model.blockSize)
                |> Dict.fromList
    in
    ( { model
        | discoTrapMap = discoTrapMap
        , discoTrapOffset = newOffset
      }
    , Cmd.none
    )
        |> andThen updateBoardData


triggerEmojiTrap : Model -> ( Model, Cmd Msg )
triggerEmojiTrap model =
    ( { model
        | emojiTrapTimer = model.trapDuration
        , emojiTrapTriggers = model.emojiTrapTriggers + 1
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

                EmojiTrapShapes ->
                    Random.uniform shapeEmojis []

                EmojiTrapRandom ->
                    Random.uniform animalEmojis [ fruitEmojis, shapeEmojis ]

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


shapeEmojis : List String
shapeEmojis =
    [ "🟢", "⭕", "🌐", "🔳", "🔶", "💎", "⭐", "♣️", "♠️", "♥️"
    , "♦️", "🔆", "🌙", "💧", "❄️", "⚜️", "🔱", "🧩", "🌀", "🔼"
    ]


emojiTrapVariantToString : EmojiTrapVariant -> String
emojiTrapVariantToString variant =
    case variant of
        EmojiTrapAnimals ->
            "animals"

        EmojiTrapFruits ->
            "fruits"

        EmojiTrapShapes ->
            "shapes"

        EmojiTrapRandom ->
            "random"


emojiTrapVariantFromString : String -> EmojiTrapVariant
emojiTrapVariantFromString str =
    case str of
        "animals" ->
            EmojiTrapAnimals

        "fruits" ->
            EmojiTrapFruits

        "shapes" ->
            EmojiTrapShapes

        _ ->
            EmojiTrapRandom


triggerTunnelVisionTrap : Model -> ( Model, Cmd Msg )
triggerTunnelVisionTrap model =
    ( { model
        | tunnelVisionTrapTimer = model.trapDuration
        , tunnelVisionTrapTriggers = model.tunnelVisionTrapTriggers + 1
      }
    , Cmd.none
    )
        |> andThen updateBoardData


candidateLayoutFromString : String -> Int
candidateLayoutFromString str =
    case str of
        "0" ->
            0

        "1" ->
            1

        _ ->
            0


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


toZeroPaddedString : Int -> String
toZeroPaddedString number =
    if number < 10 then
        "0" ++ String.fromInt number

    else
        String.fromInt number


loadSavedGame : SavedGame -> Model -> Model
loadSavedGame save model =
    let
        cells : UnpackedBoardCells
        cells =
            unpackBoardCells
                { coordinates = save.coordinates
                , current = save.current
                , solution = save.solution
                }

        cellBlocks : Dict ( Int, Int ) (List Area)
        cellBlocks =
            buildCellAreasMap save.puzzleAreas.blocks
    in
    { model
        | cellBlocks = cellBlocks
        , cellBoards = buildCellAreasMap save.puzzleAreas.boards
        , cellCols = buildCellAreasMap save.puzzleAreas.cols
        , cellRows = buildCellAreasMap save.puzzleAreas.rows
        , disabledLocations = save.disabledLocations
        , blockSize = save.blockSize
        , bundleSize = save.bundleSize
        , bundleBlocks = save.bundleBlocks
        , blockBundles = buildBlockBundles save.bundleBlocks
        , current = cells.current
        , discoTrapReceived = 0
        , discoTrapTriggers = save.discoTrapTriggers
        , emojiTrapReceived = 0
        , emojiTrapTriggers = save.emojiTrapTriggers
        , errors = Dict.empty
        , gameIsLocal = save.gameIsLocal
        , gameState = Playing
        , givens = cells.givens
        , locationScouting = save.locationScouting
        , lockedBlocks = save.lockedBlocks
        , progression = save.progression
        , puzzleAreas = save.puzzleAreas
        , removeRandomCandidateReceived = if save.gameIsLocal then save.removeRandomCandidateReceived else 0
        , removeRandomCandidateUsed = save.removeRandomCandidateUsed
        , seedInput = save.seed
        , solution = cells.solution
        , solveRandomCellReceived = if save.gameIsLocal then save.solveRandomCellReceived else 0
        , solveRandomCellUsed = save.solveRandomCellUsed
        , solveSelectedCellReceived = if save.gameIsLocal then save.solveSelectedCellReceived else 0
        , solveSelectedCellUsed = save.solveSelectedCellUsed
        , solvedLocations = save.solvedLocations
        , tunnelVisionTrapTriggers = save.tunnelVisionTrapTriggers
        , unlockMap = save.unlockMap
        , unlockedBlocks = if save.gameIsLocal then save.unlockedBlocks else Set.empty
        , visibleCells =
            Set.foldl
                (\block visibleCells ->
                    Dict.get block cellBlocks
                        |> Maybe.withDefault []
                        |> List.Extra.find
                            (\area ->
                                area.startRow == Tuple.first block
                                    && area.startCol == Tuple.second block
                            )
                        |> Maybe.map .cells
                        |> Maybe.withDefault []
                        |> Set.fromList
                        |> Set.union visibleCells
                )
                Set.empty
                save.unlockedBlocks
    }
        |> restoreScoutedItems


visibleLocations : Model -> Set Int
visibleLocations model =
    [ ( "blocks", model.puzzleAreas.blocks, cellToBlockId )
    , ( "boards", model.puzzleAreas.boards, cellToBoardId )
    , ( "columns", model.puzzleAreas.cols, cellToColId )
    , ( "rows", model.puzzleAreas.rows, cellToRowId )
    ]
        |> List.concatMap
            (\( locationType, areas, toId ) ->
                if Set.member locationType model.disabledLocations then
                    []

                else
                    List.filterMap
                        (\area ->
                            if List.all (cellIsVisible model) area.cells then
                                Just (toId ( area.startRow, area.startCol ))

                            else
                                Nothing
                        )
                        areas
            )
        |> Set.fromList


scoutableLocations : Model -> Set Int
scoutableLocations model =
    if model.locationScouting == ScoutingDisabled then
        Set.empty

    else if not model.gameIsLocal && model.createHintsWhenScouting then
        Set.diff (visibleLocations model) model.hintedLocations

    else
        Set.diff (visibleLocations model) (Set.fromList (Dict.keys model.scoutedItems))


scoutLocationIds : Set Int -> Model -> ( Model, Cmd Msg )
scoutLocationIds ids model =
    if model.gameIsLocal then
        ( { model | scoutedItems = scoutLocalLocations model ids }
        , Cmd.none
        )

    else
        ( model
        , scoutLocations
            { locations = Set.toList ids
            , createHint = model.createHintsWhenScouting
            }
        )


restoreScoutedItems : Model -> Model
restoreScoutedItems model =
    if model.gameIsLocal then
        { model
            | scoutedItems =
                scoutLocalLocations model
                    (if model.locationScouting == ScoutingAuto then
                        Set.union model.solvedLocations (visibleLocations model)

                     else
                        model.solvedLocations
                    )
        }

    else
        model


positionBoards : Int -> Int -> Int -> List ( Int, Int )
positionBoards blockSize boardsPerCluster numberOfBoards =
    let
        fullClusters : Int
        fullClusters =
            numberOfBoards // boardsPerCluster

        remainingBoards : Int
        remainingBoards =
            modBy boardsPerCluster numberOfBoards

        totalClusters : Int
        totalClusters =
            if remainingBoards > 0 then
                fullClusters + 1

            else
                fullClusters

        gridSize : Int
        gridSize =
            totalClusters
                |> toFloat
                |> sqrt
                |> ceiling
                |> max 1

        ( clusterRows, clusterCols ) =
            getClusterDimensions blockSize boardsPerCluster

        clusterPositions : List ( Int, Int )
        clusterPositions =
            positionClusters totalClusters gridSize clusterRows clusterCols

        fullClusterPositions : List ( Int, Int )
        fullClusterPositions =
            List.concatMap
                (\i ->
                    let
                        clusterPosition : ( Int, Int )
                        clusterPosition =
                            List.drop i clusterPositions
                                |> List.head
                                |> Maybe.withDefault ( 0, 0 )

                        clusterBoardPositions : List ( Int, Int )
                        clusterBoardPositions =
                            positionBoardsInCluster blockSize boardsPerCluster
                    in
                    List.map
                        (\( rowOffset, colOffset ) ->
                            ( Tuple.first clusterPosition + rowOffset - 1
                            , Tuple.second clusterPosition + colOffset - 1
                            )
                        )
                        clusterBoardPositions
                )
                (List.range 0 (fullClusters - 1))

        remainingClusterPositions : List ( Int, Int )
        remainingClusterPositions =
            if remainingBoards > 0 then
                let
                    clusterPosition : ( Int, Int )
                    clusterPosition =
                        List.drop fullClusters clusterPositions
                            |> List.head
                            |> Maybe.withDefault ( 0, 0 )

                    clusterBoardPositions : List ( Int, Int )
                    clusterBoardPositions =
                        positionBoardsInCluster blockSize remainingBoards
                in
                List.map
                    (\( rowOffset, colOffset ) ->
                        ( Tuple.first clusterPosition + rowOffset - 1
                        , Tuple.second clusterPosition + colOffset - 1
                        )
                    )
                    clusterBoardPositions

            else
                []
    in
    List.append fullClusterPositions remainingClusterPositions


getClusterDimensions : Int -> Int -> ( Int, Int )
getClusterDimensions blockSize numberOfBoards =
    List.foldl
        (\( row, col ) ( maxRow, maxCol ) ->
            ( max maxRow (row + blockSize - 1)
            , max maxCol (col + blockSize - 1)
            )
        )
        ( 0, 0 )
        (positionBoardsInCluster blockSize numberOfBoards)


positionClusters : Int -> Int -> Int -> Int -> List ( Int, Int )
positionClusters totalClusters gridSize clusterRows clusterCols =
    let
        padding : Int
        padding =
            1
    in
    List.map
        (\i ->
            let
                ring : Int
                ring =
                    floor (sqrt (toFloat i))

                ringStart : Int
                ringStart =
                    ring * ring

                offset : Int
                offset =
                    i - ringStart
            in
            if offset < ring then
                let
                    row : Int
                    row =
                        offset

                    col : Int
                    col =
                        ring
                in
                ( row * (clusterRows + padding) + 1
                , col * (clusterCols + padding) + 1
                )

            else
                let
                    row : Int
                    row =
                        ring

                    col : Int
                    col =
                        i - ringStart - ring
                in
                ( row * (clusterRows + padding) + 1
                , col * (clusterCols + padding) + 1
                )
        )
        (List.range 0 (totalClusters - 1))


positionBoardsInCluster : Int -> Int -> List ( Int, Int )
positionBoardsInCluster blockSize numberOfBoards =
    let
        ( overlapRows, overlapCols ) =
            blockSizeToOverlap blockSize

        spotsInGrid : Int -> Int
        spotsInGrid side =
            ceiling (toFloat (side * side) / 2)

        findSideLength : Int -> Int
        findSideLength side =
            if spotsInGrid side >= numberOfBoards then
                side

            else
                findSideLength (side + 1)

        isCornerOverlap : ( Int, Int ) -> Bool
        isCornerOverlap ( row, col ) =
            modBy 2 (row + col) == 0

        mapToCell : ( Int, Int ) -> ( Int, Int )
        mapToCell ( row, col ) =
            ( row * (blockSize - overlapRows) + 1
            , col * (blockSize - overlapCols) + 1
            )

        gridSideLength : Int
        gridSideLength =
            findSideLength 1
    in
    List.concatMap
        (\r ->
            List.map
                (\c ->
                    ( r, c )
                )
                (List.range 0 (gridSideLength - 1))
        )
        (List.range 0 (gridSideLength - 1))
        |> List.filter isCornerOverlap
        |> List.take numberOfBoards
        |> List.map mapToCell


blockSizeToOverlap : Int -> ( Int, Int )
blockSizeToOverlap blockSize =
    case blockSize of
        4 ->
            ( 1, 1 )

        6 ->
            ( 2, 2 )

        8 ->
            ( 2, 2 )

        9 ->
            ( 3, 3 )

        12 ->
            ( 3, 4 )

        16 ->
            ( 4, 4 )

        _ ->
            ( 0, 0 )


buildPuzzleAreasForBoard : Int -> Int -> Int -> PuzzleAreas
buildPuzzleAreasForBoard blockSize startRow startCol =
    let
        ( blockRows, blockCols ) =
            blockSizeToDimensions blockSize
    in
    { blocks =
        List.concatMap
            (\r ->
                List.map
                    (\c ->
                        buildArea
                            (startRow + r * blockRows)
                            (startCol + c * blockCols)
                            (startRow + (r + 1) * blockRows - 1)
                            (startCol + (c + 1) * blockCols - 1)
                    )
                    (List.range 0 (blockSize // blockRows - 1))
            )
            (List.range 0 (blockSize // blockCols - 1))
    , rows =
        List.map
            (\r ->
                buildArea
                    (startRow + r)
                    startCol
                    (startRow + r)
                    (startCol + blockSize - 1)
            )
            (List.range 0 (blockSize - 1))
    , cols =
        List.map
            (\c ->
                buildArea
                    startRow
                    (startCol + c)
                    (startRow + blockSize - 1)
                    (startCol + c)
            )
            (List.range 0 (blockSize - 1))
    , boards =
        [ buildArea
            startRow
            startCol
            (startRow + blockSize - 1)
            (startCol + blockSize - 1)
        ]
    }


joinPuzzleAreas : List PuzzleAreas -> PuzzleAreas
joinPuzzleAreas puzzleAreasList =
    let
        areaDict : (PuzzleAreas -> List Area) -> Dict ( Int, Int ) Area
        areaDict areaFun =
            List.foldl
                (\puzzleAreas acc ->
                    List.foldl
                        (\area dictAcc ->
                            Dict.insert ( area.startRow, area.startCol ) area dictAcc
                        )
                        acc
                        (areaFun puzzleAreas)
                )
                Dict.empty
                puzzleAreasList
    in
    { boards = Dict.values (areaDict .boards)
    , blocks = Dict.values (areaDict .blocks)
    , rows = Dict.values (areaDict .rows)
    , cols = Dict.values (areaDict .cols)
    }


getFillerCounts : Model -> Int -> Dict Int Int
getFillerCounts model targetTotal =
    let
        ratios : Dict Int Float
        ratios =
            [ ( 2, model.removeRandomCandidateRatio )
            , ( 1, model.solveRandomCellRatio )
            , ( 99, 0 )
            , ( 201, model.solveSelectedCellRatio )
            , ( 401, model.emojiTrapRatio )
            , ( 402, model.discoTrapRatio )
            , ( 403, model.tunnelVisionTrapRatio )
            ]
                |> List.map (Tuple.mapSecond (\r -> toFloat r / 100))
                |> Dict.fromList

        initialCounts : Dict Int Int
        initialCounts =
            Dict.map
                (\id ratio ->
                    ceiling (toFloat model.numberOfBoards * ratio)
                )
                ratios

        initialTotal : Int
        initialTotal =
            Dict.values initialCounts
                |> List.sum

        scale : Float
        scale =
            if initialTotal == 0 then
                1

            else
                toFloat targetTotal / toFloat initialTotal

        scaledCounts : Dict Int Int
        scaledCounts =
            Dict.map
                (\_ count ->
                    floor (toFloat count * scale)
                )
                initialCounts

        scaledTotal : Int
        scaledTotal =
            Dict.values scaledCounts
                |> List.sum

        toAdd : Int
        toAdd =
            targetTotal - scaledTotal

        bestKey : Dict Int Int -> Maybe Int
        bestKey counts =
            List.Extra.maximumWith
                (\a b ->
                    let
                        currentRatio : Int -> Float
                        currentRatio key =
                            (Dict.get key counts
                                |> Maybe.withDefault 0
                                |> toFloat
                             ) / toFloat model.numberOfBoards

                        targetRatio : Int -> Float
                        targetRatio key =
                            Dict.get key ratios
                                |> Maybe.withDefault 0

                        ratio : Int -> Float
                        ratio key =
                            targetRatio key - currentRatio key
                    in
                    compare (ratio a) (ratio b)
                )
                (Dict.keys counts)

        addBestCounts : Dict Int Int -> Dict Int Int
        addBestCounts counts =
            case bestKey counts of
                Just key ->
                    Dict.update key
                        (\maybeCount ->
                            maybeCount
                                |> Maybe.withDefault 0
                                |> (+) 1
                                |> Just
                        )
                        counts

                Nothing ->
                    counts
    in
    if initialTotal == targetTotal then
        initialCounts

    else if initialTotal < targetTotal then
        Dict.update
            99
            (\v ->
                Maybe.withDefault 0 v + (targetTotal - initialTotal)
                    |> Just
            )
            initialCounts

    else
        List.foldl
            (\_ acc ->
                addBestCounts acc
            )
            scaledCounts
            (List.range 1 toAdd)


applyYamlOptions : YamlOptions -> Model -> Model
applyYamlOptions opts model =
    let
        newBlockSize : Int
        newBlockSize =
            opts.blockSize |> Maybe.withDefault model.blockSize

        newNumberOfBoards : Int
        newNumberOfBoards =
            opts.numberOfBoards
                |> Maybe.withDefault model.numberOfBoards
                |> min (maxNumberOfBoards newBlockSize)

        setIntField : (YamlOptions -> Maybe Int) -> (Model -> Int) -> Int
        setIntField optsField modelField =
            optsField opts
                |> Maybe.withDefault (modelField model)

        setStringField : (YamlOptions -> Maybe String) -> (Model -> String) -> String
        setStringField optsField modelField =
            optsField opts
                |> Maybe.withDefault (modelField model)

        setIntAsStringField : (YamlOptions -> Maybe Int) -> (Model -> String) -> String
        setIntAsStringField optsField modelField =
            optsField opts
                |> Maybe.map String.fromInt
                |> Maybe.withDefault (modelField model)
    in
    { model
        | playerNameOption = setStringField .playerName .playerNameOption
        , blockSize = newBlockSize
        , boardsPerCluster = setIntField .boardsPerCluster .boardsPerCluster
        , numberOfBoards = newNumberOfBoards
        , numberOfBoardsInput = String.fromInt newNumberOfBoards
        , difficulty = setIntField .difficulty .difficulty
        , progression = opts.progression |> Maybe.withDefault model.progression
        , duplicateProgression = setIntField .duplicateProgression .duplicateProgression
        , duplicateProgressionInput = setIntAsStringField .duplicateProgression .duplicateProgressionInput
        , bundleSize = setIntField .bundleSize .bundleSize
        , disabledLocationsChecked = Maybe.withDefault model.disabledLocationsChecked opts.disabledLocations
        , bundleSizeInput = setIntAsStringField .bundleSize .bundleSizeInput
        , locationScouting = opts.locationScouting |> Maybe.withDefault model.locationScouting
        , solveSelectedCellRatio = setIntField .solveSelectedCellRatio .solveSelectedCellRatio
        , solveSelectedCellRatioInput = setIntAsStringField .solveSelectedCellRatio .solveSelectedCellRatioInput
        , solveRandomCellRatio = setIntField .solveRandomCellRatio .solveRandomCellRatio
        , solveRandomCellRatioInput = setIntAsStringField .solveRandomCellRatio .solveRandomCellRatioInput
        , removeRandomCandidateRatio = setIntField .removeRandomCandidateRatio .removeRandomCandidateRatio
        , removeRandomCandidateRatioInput = setIntAsStringField .removeRandomCandidateRatio .removeRandomCandidateRatioInput
        , emojiTrapRatio = setIntField .emojiTrapRatio .emojiTrapRatio
        , emojiTrapRatioInput = setIntAsStringField .emojiTrapRatio .emojiTrapRatioInput
        , discoTrapRatio = setIntField .discoTrapRatio .discoTrapRatio
        , discoTrapRatioInput = setIntAsStringField .discoTrapRatio .discoTrapRatioInput
        , tunnelVisionTrapRatio = setIntField .tunnelVisionTrapRatio .tunnelVisionTrapRatio
        , tunnelVisionTrapRatioInput = setIntAsStringField .tunnelVisionTrapRatio .tunnelVisionTrapRatioInput
        , preFillNothingsPercent = setIntField .preFillNothingsPercent .preFillNothingsPercent
        , preFillNothingsPercentInput = setIntAsStringField .preFillNothingsPercent .preFillNothingsPercentInput
        , progressionBalancing = setIntField .progressionBalancing .progressionBalancing
        , progressionBalancingInput = setIntAsStringField .progressionBalancing .progressionBalancingInput
    }


bindableActionData : BindableAction -> BindableActionData
bindableActionData action =
    case action of
        ClearBoard ->
            { defaultCodes = []
            , id = "clearBoard"
            , label = \_ -> "Clear board"
            , msg = ClearBoardPressed
            }

        ClearCell ->
            { defaultCodes = [ "Backspace", "Delete" ]
            , id = "clearCell"
            , label = \_ -> "Clear cell"
            , msg = ClearCellPressed
            }

        EnterNumber n ->
            { defaultCodes = defaultNumberCodes n
            , id = "enterNumber" ++ String.fromInt n
            , label = \model -> "Input " ++ numberToString { model | emojiTrapTimer = 0 } n
            , msg = NumberPressed n
            }

        FillBoardCandidates ->
            { defaultCodes = []
            , id = "fillBoardCandidates"
            , label = \_ -> "Add candidates to board"
            , msg = FillBoardCandidatesPressed
            }

        FillCellCandidates ->
            { defaultCodes = [ "KeyQ" ]
            , id = "fillCellCandidates"
            , label = \_ -> "Fill cell candidates"
            , msg = FillCellCandidatesPressed
            }

        HoldCandidateMode ->
            { defaultCodes = [ "Shift" ]
            , id = "holdCandidateMode"
            , label = \_ -> "Hold input mode"
            , msg = InputModifierHeld
            }

        MoveDown ->
            { defaultCodes = [ "ArrowDown", "KeyJ" ]
            , id = "moveDown"
            , label = \_ -> "Move down"
            , msg = MoveSelectionPressed ( 1, 0 )
            }

        MoveLeft ->
            { defaultCodes = [ "ArrowLeft", "KeyH" ]
            , id = "moveLeft"
            , label = \_ -> "Move left"
            , msg = MoveSelectionPressed ( 0, -1 )
            }

        MoveRight ->
            { defaultCodes = [ "ArrowRight", "KeyL" ]
            , id = "moveRight"
            , label = \_ -> "Move right"
            , msg = MoveSelectionPressed ( 0, 1 )
            }

        MoveUp ->
            { defaultCodes = [ "ArrowUp", "KeyK" ]
            , id = "moveUp"
            , label = \_ -> "Move up"
            , msg = MoveSelectionPressed ( -1, 0 )
            }

        RemoveInvalidCandidates ->
            { defaultCodes = [ "KeyW" ]
            , id = "removeInvalidCandidates"
            , label = \_ -> "Remove invalid candidates"
            , msg = RemoveInvalidCandidatesPressed
            }

        SelectSingleCandidateCell ->
            { defaultCodes = [ "KeyS" ]
            , id = "selectSingleCandidateCell"
            , label = \_ -> "Select single-candidate cell"
            , msg = SelectSingleCandidateCellPressed
            }

        SelectSolvableBoard ->
            { defaultCodes = [ "KeyG" ]
            , id = "selectSolvableBoard"
            , label = \_ -> "Select solvable board"
            , msg = SelectSolvableBoardPressed
            }

        ToggleCandidateMode ->
            { defaultCodes = [ "Space" ]
            , id = "toggleCandidateMode"
            , label = \_ -> "Toggle input mode"
            , msg = ToggleCandidateModePressed
            }

        ToggleHighlightMode ->
            { defaultCodes = [ "Tab" ]
            , id = "toggleHighlightMode"
            , label = \_ -> "Toggle highlight mode"
            , msg = ToggleHighlightModePressed
            }

        Undo ->
            { defaultCodes = [ "KeyZ" ]
            , id = "undo"
            , label = \_ -> "Undo"
            , msg = UndoPressed
            }

        UseRemoveRandomCandidate ->
            { defaultCodes = []
            , id = "removeRandomCandidate"
            , label = \_ -> "Remove random candidate"
            , msg = RemoveRandomCandidatePressed
            }

        UseSolveRandomCell ->
            { defaultCodes = []
            , id = "solveRandomCell"
            , label = \_ -> "Solve random cell"
            , msg = SolveRandomCellPressed
            }

        UseSolveSelectedCell ->
            { defaultCodes = []
            , id = "solveSelectedCell"
            , label = \_ -> "Solve selected cell"
            , msg = SolveSelectedCellPressed
            }

        ZoomIn ->
            { defaultCodes = [ "NumpadAdd" ]
            , id = "zoomIn"
            , label = \_ -> "Zoom in"
            , msg = ZoomInPressed
            }

        ZoomOut ->
            { defaultCodes = [ "NumpadSubtract" ]
            , id = "zoomOut"
            , label = \_ -> "Zoom out"
            , msg = ZoomOutPressed
            }

        ZoomReset ->
            { defaultCodes = []
            , id = "zoomReset"
            , label = \_ -> "Zoom reset"
            , msg = ZoomResetPressed
            }


defaultNumberCodes : Int -> List String
defaultNumberCodes n =
    case n of
        10 ->
            [ "Digit0", "Numpad0" ]

        11 ->
            [ "KeyA" ]

        12 ->
            [ "KeyB" ]

        13 ->
            [ "KeyC" ]

        14 ->
            [ "KeyD" ]

        15 ->
            [ "KeyE" ]

        16 ->
            [ "KeyF" ]

        _ ->
            if n >= 1 && n <= 9 then
                [ "Digit" ++ String.fromInt n, "Numpad" ++ String.fromInt n ]

            else
                []


allBindableActions : List BindableAction
allBindableActions =
    [ MoveUp
    , MoveDown
    , MoveLeft
    , MoveRight
    , ClearCell
    , ZoomIn
    , ZoomOut
    , ZoomReset
    , HoldCandidateMode
    , ToggleCandidateMode
    , ToggleHighlightMode
    , Undo
    , SelectSingleCandidateCell
    , SelectSolvableBoard
    , RemoveInvalidCandidates
    , FillCellCandidates
    , FillBoardCandidates
    , ClearBoard
    , UseSolveSelectedCell
    , UseSolveRandomCell
    , UseRemoveRandomCandidate
    ]
        ++ List.map EnterNumber (List.range 1 16)


defaultKeyBindings : Dict String (List String)
defaultKeyBindings =
    allBindableActions
        |> List.map
            (\action ->
                let
                    data : BindableActionData
                    data =
                        bindableActionData action
                in
                ( data.id, data.defaultCodes )
            )
        |> Dict.fromList


codesForAction : BindableAction -> Dict String (List String) -> List String
codesForAction action bindings =
    Dict.get (bindableActionData action).id bindings
        |> Maybe.withDefault []


normalizeCode : String -> String
normalizeCode code =
    case code of
        "ShiftLeft" ->
            "Shift"

        "ShiftRight" ->
            "Shift"

        "ControlLeft" ->
            "Control"

        "ControlRight" ->
            "Control"

        "AltLeft" ->
            "Alt"

        "AltRight" ->
            "Alt"

        _ ->
            code


codeToChar : Dict String String -> String -> Maybe String
codeToChar layout code =
    Dict.get code layout
        |> Maybe.map String.toUpper


stripCode : String -> String
stripCode code =
    code
        |> String.replace "Key" ""
        |> String.replace "Digit" ""
        |> String.replace "Numpad" ""


menuCode : Model -> String -> String
menuCode model code =
    codeToChar model.keyboardLayout code
        |> Maybe.withDefault code


keyLabel : Model -> BindableAction -> String
keyLabel model action =
    case List.head (codesForAction action model.keyBindings) of
        Just code ->
            "[" ++ (codeToChar model.keyboardLayout code |> Maybe.withDefault (stripCode code)) ++ "]"

        Nothing ->
            ""


keyHint : Model -> BindableAction -> String
keyHint model action =
    let
        label : String
        label =
            keyLabel model action
    in
    if String.isEmpty label then
        ""

    else
        " " ++ label


actionForCode : String -> Dict String (List String) -> Maybe BindableAction
actionForCode code bindings =
    List.Extra.find
        (\action -> List.member code (codesForAction action bindings))
        allBindableActions


bindingSlots : BindableAction -> Dict String (List String) -> List (Maybe String)
bindingSlots action bindings =
    let
        codes : List String
        codes =
            codesForAction action bindings
    in
    [ List.Extra.getAt 0 codes
    , List.Extra.getAt 1 codes
    ]


setBindingSlot : BindableAction -> Int -> String -> Dict String (List String) -> Dict String (List String)
setBindingSlot action slotIndex code bindings =
    let
        newCodes : List String
        newCodes =
            bindingSlots action bindings
                |> List.Extra.setAt slotIndex (Just code)
                |> List.filterMap identity
                |> List.Extra.unique
    in
    bindings
        |> Dict.map (\_ codes -> List.filter ((/=) code) codes)
        |> Dict.insert (bindableActionData action).id newCodes


clearBindingSlot : BindableAction -> Int -> Dict String (List String) -> Dict String (List String)
clearBindingSlot action slotIndex bindings =
    Dict.insert
        (bindableActionData action).id
        (List.Extra.removeAt slotIndex (codesForAction action bindings))
        bindings


mergeSavedKeyBindings : Dict String (List String) -> Dict String (List String)
mergeSavedKeyBindings saved =
    let
        usedCodes : Set String
        usedCodes =
            saved
                |> Dict.values
                |> List.concat
                |> Set.fromList
    in
    Dict.foldl
        (\id defaultCodes acc ->
            if Dict.member id saved then
                acc

            else
                Dict.insert id
                    (List.filter (\code -> not (Set.member code usedCodes)) defaultCodes)
                    acc
        )
        saved
        defaultKeyBindings



---
-- View functions
---


view : Model -> Html Msg
view model =
    case model.gameState of
        MainMenu ->
            Html.div
                [ HA.class "row"
                ]
                [ viewMenu model
                , viewColorScheme model
                ]

        Connecting ->
            Html.div
                [ HA.class "row"
                , HA.style "height" "100vh"
                ]
                [ Html.h2
                    [ HA.style "align-self" "center"
                    , HA.style "margin" "0 auto"
                    , HA.style "padding" "var(--spacing-l)"
                    ]
                    [ Html.text "Connecting..." ]
                , viewColorScheme model
                ]

        Generating ->
            Html.div
                [ HA.class "row"
                , HA.style "height" "100vh"
                ]
                [ Html.div
                    [ HA.class "column center gap-m"
                    , HA.style "align-self" "center"
                    , HA.style "margin" "0 auto"
                    , HA.style "padding" "var(--spacing-l)"

                    ]
                    [ Html.h2
                        []
                        [ Html.text "Generating Puzzle..." ]
                    , Html.div
                        []
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
                , viewColorScheme model
                ]

        Playing ->
            Html.div
                [ HA.class "main-container"
                ]
                [ viewBoard model
                , viewInfoPanel model
                , Html.Extra.viewIf model.showKeybindingsMenu (viewKeybindingsOverlay model)
                , viewColorScheme model
                ]

        Disconnected ->
            Html.div
                [ HA.class "main-container"
                ]
                [ viewBoard model
                , viewInfoPanel model
                , viewDisconnectedOverlay model
                , viewColorScheme model
                ]


viewColorScheme : Model -> Html Msg
viewColorScheme model =
    Html.node "style"
        []
        [ Html.text
            (String.concat
                [ ":root { color-scheme: "
                , model.colorScheme
                , "; }"
                ]
            )
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    Html.div
        [ HA.class "main-menu"
        ]
        [ Html.h1
            []
            [ Html.text "Archipeladoku" ]
        , viewMenuConnect model
        , viewMenuResume model
        , viewMenuOptions model
        , viewMenuAbout
        , viewMenuUtilities
        , Html.div
            [ HA.style "align-self" "center"
            , HA.style "color" "var(--text-color)"
            , HA.style "opacity" "0.8"
            ]
            [ Html.text "Client version: "
            , Html.text (String.left 8 model.version)
            ]
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
                    , HA.type_ "password"
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
        , if List.isEmpty model.connectionHistory then
            Html.text ""

          else
            Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.text "Previous Connections:"
                , Html.div
                    [ HA.class "row gap-s wrap"
                    ]
                    (List.append
                        (List.map
                            (\entry ->
                                Html.button
                                    [ HA.class "button"
                                    , HE.onClick (ConnectionHistoryQuickFillPressed entry)
                                    ]
                                    [ Html.text (entry.player ++ "@" ++ entry.host) ]
                            )
                            model.connectionHistory
                        )
                        [ Html.button
                            [ HA.class "button"
                            , HA.style "margin-left" "auto"
                            , HE.onClick ClearConnectionHistoryPressed
                            ]
                            [ Html.text "Clear History" ]
                        ]
                    )
                ]
        ]


viewMenuResume : Model -> Html Msg
viewMenuResume model =
    case model.localGameSave of
        Just save ->
            Html.div
                [ HA.class "main-menu-panel"
                ]
                [ Html.h2
                    []
                    [ Html.text "Resume Local Game" ]
                , Html.div
                    []
                    [ Html.text
                        (String.concat
                            [ "Last played: "
                            , viewDateTime model save.timestamp
                            ]
                        )
                    ]
                , Html.div
                    []
                    [ Html.text
                        (String.concat
                            [ "Seed: "
                            , String.fromInt save.seed
                            ]
                        )
                    ]
                , Html.div
                    []
                    [ Html.text
                        (String.concat
                            [ "Progress: "
                            , String.fromInt (Set.size save.unlockedBlocks)
                            , " / "
                            , String.fromInt (save.puzzleAreas.blocks |> List.length)
                            , " blocks unlocked, "
                            , String.fromInt (Set.size save.solvedLocations)
                            , " / "
                            , String.fromInt
                                (List.sum
                                    [ List.length save.puzzleAreas.rows
                                    , List.length save.puzzleAreas.cols
                                    , List.length save.puzzleAreas.blocks
                                    , List.length save.puzzleAreas.boards
                                    ]
                                )
                            , " areas solved"
                            ]
                        )
                    ]
                , Html.button
                    [ HA.class "button"
                    , HE.onClick (ResumeLocalGamePressed save)
                    ]
                    [ Html.text "Resume Local Game" ]
                ]

        Nothing ->
            Html.text ""


viewDateTime : Model -> Time.Posix -> String
viewDateTime model posix =
    String.concat
        [ Time.toYear model.timezone posix |> String.fromInt
        , "-"
        , Time.toMonth model.timezone posix |> monthToString
        , "-"
        , Time.toDay model.timezone posix |> toZeroPaddedString
        , " "
        , Time.toHour model.timezone posix |> toZeroPaddedString
        , ":"
        , Time.toMinute model.timezone posix |> toZeroPaddedString
        , ":"
        , Time.toSecond model.timezone posix |> toZeroPaddedString
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
            , HA.style "grid-template-columns" "repeat(auto-fit, minmax(220px, 1fr))"
            , HA.style "gap" "var(--spacing-l)"
            ]
            [ Html.button
                [ HA.class "button"
                , HE.onClick LoadYamlPressed
                ]
                [ Html.text "Load YAML" ]
            , Html.button
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
        , viewMenuOptionsStats model
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
                        "The size of each block, and the width/height of each board. A standard Sudoku is 9. Smaller sizes are easier, larger sizes are more difficult."
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
                            , " - Very Hard: X-Wing, Swordfish, Y-Wing."
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
                    , viewRadioButton 5 model.difficulty "difficulty" DifficultyChanged (\_ -> "Very Hard")
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
                    [ viewRadioButton Fixed model.progression "progression" ProgressionChanged (\_ -> "Fixed")
                    , viewRadioButton Shuffled model.progression "progression" ProgressionChanged (\_ -> "Shuffled")
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
                    [ Html.text "Bundle Size:"
                    , viewOptionHint
                        "bundle-size-hint"
                        (String.join
                            "\n"
                            [ "How many blocks a single progression item unlocks at once."
                            , "- A value of 1 disables bundling."
                            , "- For Fixed progression each Progressive Block is worth this many unlocks."
                            , "- For Shuffled progression each bundle is a Block Bundle item unlocking this many blocks."
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
                        , HA.max (String.fromInt model.blockSize)
                        , HA.value model.bundleSizeInput
                        , HE.onBlur BundleSizeInputBlurred
                        , HE.onInput BundleSizeInputChanged
                        ]
                        []
                    , viewRangeSlider
                        model.bundleSize
                        1
                        model.blockSize
                        BundleSizeChanged
                        Nothing
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
                    [ Html.text "Duplicate Progression Items:"
                    , viewOptionHint
                        "duplicate-progression-hint"
                        (String.join
                            "\n"
                            [  "Percent of progression items that should be duplicated."
                            , "- For Fixed progression higher values may lead to progression being too fast and as such a lower value is recommended."
                            , "- For Shuffled progression this can safely be set to 100% to make progression a bit faster. The blocks to duplicate are chosen randomly if not 100%, though each block can only be duplicated once."
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
                        , HA.min "0"
                        , HA.max "100"
                        , HA.value model.duplicateProgressionInput
                        , HE.onBlur DuplicateProgressionInputBlurred
                        , HE.onInput DuplicateProgressionInputChanged
                        ]
                        []
                    , Html.text "%"
                    , viewRangeSlider
                        model.duplicateProgression
                        0
                        100
                        DuplicateProgressionChanged
                        (Just "duplicate-progression-ticks")
                    , Html.datalist
                        [ HA.id "duplicate-progression-ticks"
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
                    [ Html.text "Disabled Locations:"
                    , viewOptionHint
                        "disabled-locations-hint"
                        (String.join
                            "\n"
                            [ "Location types to remove entirely, reducing the number of locations and filler items."
                            , "If disabling the selected types would leave too few locations for the progression items, some are automatically re-enabled."
                            , "Higher bundle sizes allow disabling more, since they reduce the number of progression items."
                            ]
                        )
                    ]
                , Html.div
                    [ HA.class "row gap-m wrap"
                    ]
                    (List.map
                        (\locationType ->
                            Html.label
                                [ HA.class "row gap-s"
                                , HA.style "align-items" "center"
                                ]
                                [ Html.input
                                    [ HA.type_ "checkbox"
                                    , HA.checked (Set.member locationType model.disabledLocationsChecked)
                                    , HE.onCheck (DisabledLocationChanged locationType)
                                    ]
                                    []
                                , Html.text (String.Extra.toSentenceCase locationType)
                                ]
                        )
                        [ "boards", "blocks", "rows", "columns" ]
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
                    [ Html.text "Location Scouting:"
                    , viewOptionHint
                        "location-scouting-hint"
                        (String.join
                            "\n"
                            [  "How scouting of locations (creating a hint) are handled:"
                            ,  " - Auto: Locations are automatically scouted when fully revealed."
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
                , hint = "Ratio of Disco Trap items to number of boards. When received all cells will shift colors for a time."
                , hintId = "disco-trap-ratio-hint"
                , value = model.discoTrapRatio
                , inputValue = model.discoTrapRatioInput
                , onBlur = DiscoTrapRatioInputBlurred
                , onInput = DiscoTrapRatioInputChanged
                , onRangeChange = DiscoTrapRatioChanged
                }
            , viewRatioInputs
                { label = "Emoji Trap Ratio:"
                , hint = "Ratio of Emoji Trap items to number of boards. When received your numbers will be replaced with emojis for a time."
                , hintId = "emoji-trap-ratio-hint"
                , value = model.emojiTrapRatio
                , inputValue = model.emojiTrapRatioInput
                , onBlur = EmojiTrapRatioInputBlurred
                , onInput = EmojiTrapRatioInputChanged
                , onRangeChange = EmojiTrapRatioChanged
                }
            , viewRatioInputs
                { label = "Tunnel Vision Trap Ratio:"
                , hint = "Ratio of Tunnel Vision Trap items to number of boards. When received you will only be able to see a small area of the board for a time."
                , hintId = "tunnelVision-trap-ratio-hint"
                , value = model.tunnelVisionTrapRatio
                , inputValue = model.tunnelVisionTrapRatioInput
                , onBlur = TunnelVisionTrapRatioInputBlurred
                , onInput = TunnelVisionTrapRatioInputChanged
                , onRangeChange = TunnelVisionTrapRatioChanged
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
                            , "Caution: This reduces the number of filler items in the item pool. Having few fillers can lead to increased generation times or even generation failures. As long as you don't remove other filler items this shouldn't be an issue though, even at 100%."
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
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    , HA.style "justify-content" "space-between"
                    ]
                    [ Html.text "Death Link:"
                    , viewOptionHint
                        "death-link-hint"
                        "Enable Death Link. When a player with death link enabled dies all other players that also enabled it die as well. Archipeladoku can only receive death links, not send them. When a death link is received all non-given numbers will be cleared. This can also be toggled in-game from the Debug menu."
                    ]
                , Html.div
                    [ HA.class "row gap-s"
                    , HA.style "align-items" "center"
                    ]
                    [ viewRadioButton True model.deathLinkInput "death-link" DeathLinkInputChanged (\_ -> "Enabled")
                    , viewRadioButton False model.deathLinkInput "death-link" DeathLinkInputChanged (\_ -> "Disabled")
                    ]
                ]
            ]
        ]


viewMenuOptionsStats : Model -> Html Msg
viewMenuOptionsStats model =
    let
        positions : List ( Int, Int )
        positions =
            positionBoards model.blockSize model.boardsPerCluster model.numberOfBoards

        puzzleAreas : PuzzleAreas
        puzzleAreas =
            positions
                |> List.map
                    (\( startRow, startCol ) ->
                        buildPuzzleAreasForBoard model.blockSize startRow startCol
                    )
                |> joinPuzzleAreas

        cells : Int
        cells =
            List.concatMap .cells puzzleAreas.boards
                |> Set.fromList
                |> Set.size

        locationCounts : Dict String Int
        locationCounts =
            Dict.fromList
                [ ( "blocks", List.length puzzleAreas.blocks )
                , ( "boards", List.length puzzleAreas.boards )
                , ( "rows", List.length puzzleAreas.rows )
                , ( "columns", List.length puzzleAreas.cols )
                ]

        progressionBlocks : Int
        progressionBlocks =
            List.length puzzleAreas.blocks - model.blockSize

        effectiveBundleSize : Int
        effectiveBundleSize =
            clamp 1 model.blockSize model.bundleSize

        rawProgressionItems : Int
        rawProgressionItems =
            if progressionBlocks <= 0 then
                0

            else
                (progressionBlocks + effectiveBundleSize - 1) // effectiveBundleSize

        progressionItems : Int
        progressionItems =
            (toFloat model.duplicateProgression) / 100 + 1
                |> (*) (toFloat rawProgressionItems)
                |> floor

        effectiveDisabledLocations : Set String
        effectiveDisabledLocations =
            resolveDisabledLocations
                model.disabledLocationsChecked
                locationCounts
                progressionItems
                (progressionDensityCap model.progression)

        locations : Int
        locations =
            Dict.foldl
                (\typ count sum ->
                    if Set.member typ effectiveDisabledLocations then
                        sum

                    else
                        sum + count
                )
                0
                locationCounts

        reEnabledLocations : Set String
        reEnabledLocations =
            Set.diff model.disabledLocationsChecked effectiveDisabledLocations

        formatLocationTypes : Set String -> String
        formatLocationTypes types =
            types
                |> Set.toList
                |> List.map String.Extra.toSentenceCase
                |> String.join ", "

        fillerItems : Int
        fillerItems =
            locations - progressionItems

        fillerCounts : Dict Int Int
        fillerCounts =
            getFillerCounts model (locations - progressionItems)

        preFilledNothings : Int
        preFilledNothings =
            Dict.get 99 fillerCounts
                |> Maybe.withDefault 0
                |> toFloat
                |> (*) (toFloat model.preFillNothingsPercent / 100)
                |> floor

        textDiv : String -> Html Msg
        textDiv text =
            Html.div
                []
                [ Html.text text ]

        countWithPercent : Int -> Int -> String
        countWithPercent count total =
            String.concat
                [ String.fromInt count
                , " ("
                , String.fromInt (round (toFloat count / toFloat total * 100))
                , "%)"
                ]
    in
    Html.div
        [ HA.class "option-statistics"
        ]
        [ Html.div
            [ HA.class "option-statistics-grid"
            ]
            [ Html.h3
                []
                [ Html.text "Statistics" ]

            , textDiv "Cells: "
            , textDiv (String.fromInt cells)

            , textDiv "Locations: "
            , textDiv (String.fromInt locations)

            , Html.Extra.viewIf
                (not (Set.isEmpty effectiveDisabledLocations))
                (textDiv "Disabled Locations: ")
            , Html.Extra.viewIf
                (not (Set.isEmpty effectiveDisabledLocations))
                (textDiv (formatLocationTypes effectiveDisabledLocations))

            , Html.Extra.viewIf
                (not (Set.isEmpty reEnabledLocations))
                (textDiv "- Auto Re-enabled: ")
            , Html.Extra.viewIf
                (not (Set.isEmpty reEnabledLocations))
                (textDiv (formatLocationTypes reEnabledLocations))

            , textDiv "Progression Items: "
            , textDiv ""
            , textDiv "- Excl. pre-filled: "
            , textDiv (countWithPercent progressionItems (locations - preFilledNothings))
            , textDiv "- Incl. pre-filled: "
            , textDiv (countWithPercent progressionItems locations)
            , textDiv "- Duplicates: "
            , textDiv <| String.fromInt <| progressionItems - rawProgressionItems

            , textDiv "Filler Items: "
            , textDiv ""
            , textDiv "- Excl. pre-filled: "
            , textDiv (countWithPercent (fillerItems - preFilledNothings) (locations - preFilledNothings))
            , textDiv "- Incl. pre-filled: "
            , textDiv (countWithPercent fillerItems locations)
            ]
        , Html.div
            [ HA.class "option-statistics-grid"
            ]
            (List.append
                [ Html.h3
                    []
                    [ Html.text "Filler Item Counts" ]
                ]
                (List.concatMap
                    (\( item, count ) ->
                        if item == NothingItem then
                            [ textDiv <| itemName item ++ ": "
                            , textDiv ""
                            , textDiv "- Excl. pre-filled: "
                            , textDiv <| String.fromInt (count - preFilledNothings)
                            , textDiv "- Incl. pre-filled: "
                            , textDiv <| String.fromInt count
                            ]

                        else
                            [ textDiv <| itemName item ++ ": "
                            , textDiv <| String.fromInt count
                            ]
                    )
                    (Dict.toList fillerCounts
                        |> List.map (Tuple.mapFirst itemFromId)
                        |> List.sortWith
                            (Order.Extra.byFieldWith
                                (Order.Extra.explicit fillerItemSortOrder)
                                Tuple.first
                            )
                    )
                )
            )
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


viewMenuAbout : Html Msg
viewMenuAbout =
    Html.div
        [ HA.class "main-menu-panel" ]
        [ Html.h2
            []
            [ Html.text "About / FAQ" ]
        , Html.div
            [ HA.class "column gap-l"
            ]
            [ Html.div
                []
                [ Html.text "Archipeladoku is Sudoku for "
                , Html.a
                    [ HA.href "https://archipelago.gg/"
                    , HA.target "_blank"
                    ]
                    [ Html.text "Archipelago" ]
                , Html.text ". You start with one board, and more boards have to be unlocked one block at a time. Each solved row, column, block, or entire board is a location. The blocks you have to unlock are your progression items."
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.h3
                    []
                    [ Html.text "What options should I use? How long does this take?" ]
                , Html.text "Think about how long it'd take you to solve a single board and extrapolate from that. Board clusters reduce solving time as later boards in a cluster will be partially solved when you get to them. Filler items can also reduce solving time. Without duplicate progression you will need every progression item to goal, so consider adding duplicate progression to allow earlier goaling. Fixed progression can be selected to ensure a more consistent progression."
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.h3
                    []
                    [ Html.text "How can I reduce the number of 'Nothing' items?" ]
                , Html.p
                    []
                    [ Html.text "Nothing items fill whatever slots remain after other items have been filled. As such you can increase the amount of other items with duplicate progression or by increasing filler ratios. You can reduce the number of locations by disabling location types (reducing progression items with bundling allows for more disabled locations)."
                    ]
                , Html.p
                    []
                    [ Html.text "In general it's recommended to not worry too much about the number of Nothing items. If you put pre-fill at 100% it keeps them out of the multiworld item pool, letting you treat them as unused locations."
                    ]
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.h3
                    []
                    [ Html.text "Where can I download the APWorld?" ]
                , Html.div
                    []
                    [ Html.text "The latest version can be downloaded "
                    , Html.a
                        [ HA.href "https://github.com/galdiuz/archipeladoku/releases/latest/download/archipeladoku.apworld" ]
                        [ Html.text "here" ]
                    , Html.text "."
                    ]
                ]
            , Html.div
                [ HA.class "column gap-s"
                ]
                [ Html.h3
                    []
                    [ Html.text "Where is the source code?" ]
                , Html.div
                    []
                    [ Html.text "The source code is available on "
                    , Html.a
                        [ HA.href "https://github.com/galdiuz/archipeladoku" ]
                        [ Html.text "GitHub" ]
                    , Html.text "."
                    ]
                ]
            ]
        ]


viewMenuUtilities : Html Msg
viewMenuUtilities =
    Html.div
        [ HA.class "main-menu-panel" ]
        [ Html.h2
            []
            [ Html.text "Utilities" ]
        , Html.div
            [ HA.class "row gap-m"
            ]
            [ Html.button
                [ HA.class "button button-flash"
                , HE.onClick ClearSavedGamesPressed
                ]
                [ Html.text "Clear saved game data" ]
            , Html.button
                [ HA.class "button button-flash"
                , HE.onClick ResetClientSettingsPressed
                ]
                [ Html.text "Reset client settings" ]
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
            , HE.on "keyup" (keyUpDecoder model)
            , HE.on "cellselected" cellSelectedDecoder
            , HA.tabindex 0
            ]
            []
        , viewZoomControls
        , viewTrapTimers model
        , viewToastMessages model
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
    if List.all ((==) 0) (timers model) then
        Html.text ""

    else
        Html.div
            [ HA.class "trap-timer-panel"
            ]
            (List.map
                (\timer ->
                    if timer.timeLeft == 0 then
                        Html.text ""

                    else
                        Html.div
                            [ HA.class "column gap-s"
                            ]
                            [ Html.text
                                (String.concat
                                    [ timer.label
                                    , ": "
                                    , String.fromInt timer.timeLeft
                                    , "s"
                                    ]
                                )
                            , Html.div
                                [ HA.class "trap-timer-track" ]
                                [ Html.div
                                    [ HA.class "trap-timer-fill"
                                    , HA.style "width"
                                        (String.fromInt
                                            (timer.timeLeft * 100 // model.trapDuration)
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
                , { label = "Tunnel vision trap"
                  , timeLeft = model.tunnelVisionTrapTimer
                  }
                , { label = "Fireworks"
                  , timeLeft = model.fireworksTimer
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
            [ Html.div
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
                                        (model.showInputErrors && not (Set.member n validCellCandidates))
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
                        [ HE.onClick ClearCellPressed
                        , HA.class "cell"
                        , HA.style "font-size" "1.5em"
                        , HA.style "width" "1.5em"
                        , HA.style "height" "1.5em"
                        ]
                        [ Html.text "×" ]
                    ]
                )
            , Html.label
                [ HA.class "column gap-s"
                ]
                [ Html.text
                    (String.concat
                        [ "Input mode"
                        , keyHint model ToggleCandidateMode
                        , ","
                        , keyHint model HoldCandidateMode
                        ]
                    )
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
                [ HA.class "col gap-s" ]
                [ Html.text
                    (String.concat
                        [ "Highlight mode"
                        , keyHint model ToggleHighlightMode
                        , ", "
                        , keyLabel model HoldCandidateMode
                        , "+"
                        , keyLabel model ToggleHighlightMode
                        ]
                    )
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
    let
        scoutable : Set Int
        scoutable =
            scoutableLocations model
    in
    Html.details
        [ HA.class "info-panel-details"
        ]
        [ Html.summary
            []
            [ Html.text "Selected Cell / Hints" ]
        , Html.div
            [ HA.class "column gap-m"
            ]
            (List.concat
                [ [ viewCellInfo model model.selectedCell ]
                , if Set.member "blocks" model.disabledLocations then
                    []

                  else
                    List.map
                        (viewBlockInfo model)
                        (Dict.get model.selectedCell model.cellBlocks
                            |> Maybe.withDefault []
                            |> List.sortBy .startRow
                        )

                , if Set.member "rows" model.disabledLocations then
                    []

                  else
                    List.map
                        (viewRowInfo model)
                        (Dict.get model.selectedCell model.cellRows
                            |> Maybe.withDefault []
                            |> List.sortBy .startCol
                        )

                , if Set.member "columns" model.disabledLocations then
                    []

                  else
                    List.map
                        (viewColInfo model)
                        (Dict.get model.selectedCell model.cellCols
                            |> Maybe.withDefault []
                            |> List.sortBy .startRow
                        )

                , if Set.member "boards" model.disabledLocations then
                    []

                  else
                    List.map
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

                , if model.locationScouting == ScoutingDisabled then
                    []

                  else
                    [ Html.div
                        [ HA.class "row gap-m"
                        , HA.style "align-items" "center"
                        ]
                        [ Html.button
                            [ HA.class "button"
                            , HA.disabled (Set.isEmpty scoutable)
                            , HE.onClick ScoutAllPressed
                            ]
                            [ Html.text
                                (String.concat
                                    [ if model.gameIsLocal || not model.createHintsWhenScouting then
                                        "Scout all ("

                                      else
                                        "Scout and hint all ("
                                    , String.fromInt (Set.size scoutable)
                                    , ")"
                                    ]
                                )
                            ]
                        ]
                    ]

                , if model.gameIsLocal || model.locationScouting == ScoutingDisabled then
                    []

                  else
                    [ Html.label
                        [ HA.class "row gap-s"
                        , HA.style "align-items" "center"
                        ]
                        [ Html.input
                            [ HA.type_ "checkbox"
                            , HA.checked model.createHintsWhenScouting
                            , HE.onCheck CreateHintsWhenScoutingChanged
                            ]
                            []
                        , Html.text "Create server hints when scouting"
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
                , Html.text (keyHint model Undo)
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
                , Html.text (keyHint model SelectSingleCandidateCell)
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
                , Html.text (keyHint model SelectSolvableBoard)
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
                , Html.text (keyHint model RemoveInvalidCandidates)
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
                , Html.text (keyHint model FillCellCandidates)
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
                , HA.style "align-items" "center"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HE.onClick FillBoardCandidatesPressed
                    ]
                    [ Html.text "Add candidates to board" ]
                , Html.text (keyHint model FillBoardCandidates)
                ]
            , Html.div
                [ HA.class "row gap-m"
                , HA.style "align-items" "center"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HE.onClick ClearBoardPressed
                    ]
                    [ Html.text "Clear board" ]
                , Html.text (keyHint model ClearBoard)
                ]
            , if model.gameIsLocal then
                Html.text ""

              else
                Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    ]
                    [ Html.button
                        [ HA.class "button"
                        , HA.disabled (Set.isEmpty (pendingServerSolves model))
                        , HE.onClick SyncSolvedFromServerPressed
                        ]
                        [ Html.text
                            (String.concat
                                [ "Sync solved from server ("
                                , String.fromInt (Set.size (pendingServerSolves model))
                                , ")"
                                ]
                            )
                        ]
                    , viewOptionHint
                        "sync-solved-from-server-hint"
                        "Solves areas holding items collected on the server but not yet solved locally. This could be due to playing on another device, or another player collecting their items."
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
            [ Html.div
                [ HA.class "row gap-m"
                , HA.style "align-items" "center"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HAE.attributeIf
                        (model.solveSelectedCellReceived > model.solveSelectedCellUsed)
                        (HE.onClick SolveSelectedCellPressed)
                    , HA.disabled (model.solveSelectedCellReceived <= model.solveSelectedCellUsed)
                    ]
                    [ Html.text
                        (String.concat
                            [ "Solve Selected Cell ("
                            , String.fromInt (model.solveSelectedCellReceived - model.solveSelectedCellUsed)
                            , " uses)"
                            ]
                        )
                    ]
                , Html.text (keyHint model UseSolveSelectedCell)
                ]
            , Html.div
                [ HA.class "row gap-m"
                , HA.style "align-items" "center"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HAE.attributeIf
                        (model.solveRandomCellReceived > model.solveRandomCellUsed)
                        (HE.onClick SolveRandomCellPressed)
                    , HA.disabled (model.solveRandomCellReceived <= model.solveRandomCellUsed)
                    ]
                    [ Html.text
                        (String.concat
                            [ "Solve Random Cell ("
                            , String.fromInt (model.solveRandomCellReceived - model.solveRandomCellUsed)
                            , " uses)"
                            ]
                        )
                    ]
                , Html.text (keyHint model UseSolveRandomCell)
                ]
            , Html.div
                [ HA.class "row gap-m"
                , HA.style "align-items" "center"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HAE.attributeIf
                        (model.removeRandomCandidateReceived > model.removeRandomCandidateUsed)
                        (HE.onClick RemoveRandomCandidatePressed)
                    , HA.disabled (model.removeRandomCandidateReceived <= model.removeRandomCandidateUsed)
                    ]
                    [ Html.text
                        (String.concat
                            [ "Remove Random Candidate ("
                            , String.fromInt (model.removeRandomCandidateReceived - model.removeRandomCandidateUsed)
                            , " uses)"
                            ]
                        )
                    ]
                , Html.text (keyHint model UseRemoveRandomCandidate)
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
            [ Html.text "Client Settings" ]
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
                [ HA.class "row gap-s" ]
                [ Html.text "Candidate layout:"
                , Html.select
                    [ HE.onInput CandidateLayoutChanged
                    ]
                    [ Html.option
                        [ HA.value "0"
                        , HA.selected (model.candidateLayout == 0)
                        ]
                        [ Html.text "Standard (top-to-bottom)" ]
                    , Html.option
                        [ HA.value "1"
                        , HA.selected (model.candidateLayout == 1)
                        ]
                        [ Html.text "Numpad (bottom-to-top)" ]
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
                [ HA.class "row gap-s"
                , HA.style "align-items" "center"
                ]
                [ Html.input
                    [ HA.type_ "checkbox"
                    , HA.checked model.fireworkOnNothing
                    , HE.onCheck FireworkOnNothingChanged
                    ]
                    []
                , Html.text "Launch a firework when receiving a Nothing"
                ]
            , Html.label
                [ HA.class "row gap-s"
                , HA.style "align-items" "center"
                ]
                [ Html.input
                    [ HA.type_ "checkbox"
                    , HA.checked model.showInputErrors
                    , HE.onCheck ShowInputErrorsChanged
                    ]
                    []
                , Html.text "Show input errors"
                ]
            , Html.label
                [ HA.class "row gap-s"
                , HA.style "align-items" "center"
                ]
                [ Html.input
                    [ HA.type_ "checkbox"
                    , HA.checked model.showToastMessages
                    , HE.onCheck ShowToastMessagesChanged
                    ]
                    []
                , Html.text "Show toast messages"
                ]
            , Html.label
                [ HA.class "row gap-s" ]
                [ Html.text "Trap duration:"
                , Html.select
                    [ HE.onInput TrapDurationChanged
                    ]
                    [ Html.option
                        [ HA.value "30"
                        , HA.selected (model.trapDuration == 30)
                        ]
                        [ Html.text "30 seconds" ]
                    , Html.option
                        [ HA.value "60"
                        , HA.selected (model.trapDuration == 60)
                        ]
                        [ Html.text "1 minute" ]
                    , Html.option
                        [ HA.value "120"
                        , HA.selected (model.trapDuration == 120)
                        ]
                        [ Html.text "2 minutes" ]
                    , Html.option
                        [ HA.value "300"
                        , HA.selected (model.trapDuration == 300)
                        ]
                        [ Html.text "5 minutes" ]
                    , Html.option
                        [ HA.value "900"
                        , HA.selected (model.trapDuration == 900)
                        ]
                        [ Html.text "15 minutes" ]
                    ]
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
                        [ HA.value "shapes"
                        , HA.selected (model.emojiTrapVariant == EmojiTrapShapes)
                        ]
                        [ Html.text "Shapes" ]
                    , Html.option
                        [ HA.value "random"
                        , HA.selected (model.emojiTrapVariant == EmojiTrapRandom)
                        ]
                        [ Html.text "Random" ]
                    ]
                ]
            , Html.button
                [ HA.class "button"
                , HE.onClick ToggleKeybindingsMenuPressed
                ]
                [ Html.text "Configure keybindings" ]
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
                , HE.onClick TriggerFireworksPressed
                ]
                [ Html.text "Trigger Fireworks" ]
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
                , HE.onClick TriggerTunnelVisionTrapPressed
                ]
                [ Html.text "Trigger Tunnel Vision Trap" ]
            , Html.button
                [ HA.class "button"
                , HE.onClick CancelTrapsPressed
                ]
                [ Html.text "Cancel Traps" ]
            , Html.button
                [ HA.class "button"
                , HE.onClick (DeathLinkTriggered Encode.null)
                ]
                [ Html.text "Trigger Death Link" ]
            , if model.gameIsLocal then
                Html.text ""

              else
                Html.label
                    [ HA.class "row gap-s"
                    , HA.style "align-items" "center"
                    ]
                    [ Html.input
                        [ HA.type_ "checkbox"
                        , HA.checked model.deathLinkEnabled
                        , HE.onCheck EnableDeathLinkChanged
                        ]
                        []
                    , Html.text "Enable death link"
                    ]
            , if model.gameIsLocal then
                Html.text ""

              else
                Html.button
                    [ HA.class "button"
                    , HE.onClick SyncSolvedToServerPressed
                    ]
                    [ Html.text "Sync solved to server" ]
            ]
        ]


viewInfoPanelMessages : Model -> Html Msg
viewInfoPanelMessages model =
    Html.details
        [ HA.class "info-panel-details"
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
        let
            maybeBundle : Maybe Int
            maybeBundle =
                Dict.get ( block.startRow, block.startCol ) model.blockBundles

            ( unlockItemId, unlockItemName ) =
                case maybeBundle of
                    Just bundle ->
                        ( blockBundleBaseId + bundle
                        , "Block Bundle " ++ String.fromInt bundle
                        )

                    Nothing ->
                        ( cellToBlockId ( block.startRow, block.startCol )
                        , "Block " ++ rowToLabel block.startRow ++ String.fromInt block.startCol
                        )

            bundlePrefix : String
            bundlePrefix =
                case maybeBundle of
                    Just _ ->
                        unlockItemName ++ " — "

                    Nothing ->
                        ""
        in
        case Dict.get unlockItemId model.hints of
            Just item ->
                Html.div
                    []
                    [ Html.text
                        (String.concat
                            [ "Unlock: "
                            , bundlePrefix
                            , item.locationName
                            , " ("
                            , item.senderAlias
                            , ", "
                            , item.locationGameName
                            , ")"
                            ]
                        )
                    ]

            Nothing ->
                Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    ]
                    [ Html.text
                        (case maybeBundle of
                            Just _ ->
                                "Unlock: " ++ unlockItemName

                            Nothing ->
                                "Unlock: ???"
                        )
                    , Html.button
                        [ HA.class "button"
                        , HE.onClick (HintItemPressed unlockItemName)
                        , HA.disabled (model.hintPoints < model.hintCost)
                        ]
                        [ Html.text "Hint" ]
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


bundleContents : Model -> Hint -> List ( Int, Int )
bundleContents model hint =
    if
        (model.gameIsLocal || hint.receiverName == model.player)
            && hint.itemId > blockBundleBaseId
            && hint.itemId <= blockBundleBaseId + maxBundles
    then
        Dict.get (hint.itemId - blockBundleBaseId) model.bundleBlocks
            |> Maybe.withDefault []
            |> List.sort

    else
        []


viewReward : Model -> Int -> Area -> Html Msg
viewReward model id area =
    case Dict.get id model.scoutedItems of
        Just hint ->
            Html.div
                []
                [ Html.div
                    [ HA.class "row gap-m"
                    , HA.style "align-items" "center"
                    ]
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
                    , if
                        model.gameIsLocal
                            || model.locationScouting == ScoutingDisabled
                            || Set.member id model.hintedLocations
                            || Set.member id model.solvedLocations
                      then
                        Html.text ""

                      else
                        Html.button
                            [ HA.class "button"
                            , HE.onClick (HintLocationPressed id)
                            ]
                            [ Html.text "Hint" ]
                    ]
                , case bundleContents model hint of
                    [] ->
                        Html.text ""

                    cells ->
                        Html.div
                            []
                            [ Html.text
                                (String.concat
                                    [ "Unlocks: "
                                    , List.map
                                        (\( row, col ) -> rowToLabel row ++ String.fromInt col)
                                        cells
                                        |> String.join ", "
                                    ]
                                )
                            ]
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
                                area.cells
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


viewToastMessages : Model -> Html Msg
viewToastMessages model =
    if List.isEmpty model.toastMessages || not model.showToastMessages then
        Html.text ""

    else
        Html.Keyed.node "div"
            [ HA.class "toast-container"
            ]
            (List.map
                (\message ->
                    ( String.fromInt message.id
                    , Html.div
                        [ HA.classList
                            [ ( "toast-message", True )
                            , ( "toast-message-fading", message.timer == 1 )
                            ]
                        ]
                        [ viewMessage message.message ]
                    )
                )
                model.toastMessages
            )


viewDisconnectedOverlay : Model -> Html Msg
viewDisconnectedOverlay model =
    Html.div
        [ HA.class "overlay"
        ]
        [ Html.div
            [ HA.class "main-menu-panel"
            ]
            [ Html.h2
                []
                [ Html.text "Disconnected" ]
            , Html.div
                []
                [ Html.text "You have been disconnected from the Archipelago server."
                ]
            , Html.button
                [ HA.class "button"
                , HE.onClick ConnectPressed
                ]
                [ Html.text "Reconnect" ]
            ]
        ]


viewKeybindingsOverlay : Model -> Html Msg
viewKeybindingsOverlay model =
    Html.div
        [ HA.class "overlay"
        , HE.preventDefaultOn "keydown" (keyBindingCaptureDecoder model)
        , HE.onClick ToggleKeybindingsMenuPressed
        ]
        [ Html.div
            [ HA.class "main-menu-panel keybindings-panel"
            , HE.stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
            ]
            [ Html.h2
                []
                [ Html.text "Keybindings" ]
            , Html.p
                [ HA.class "keybindings-instructions"
                ]
                [ Html.text "Click a slot, then press a key to bind it. Click the slot again to clear it, or press Escape to cancel." ]
            , Html.div
                [ HA.class "keybindings-list"
                ]
                (List.concatMap (viewKeybindingRow model) allBindableActions)
            , Html.div
                [ HA.class "row gap-m"
                , HA.style "justify-content" "space-between"
                ]
                [ Html.button
                    [ HA.class "button"
                    , HE.onClick ResetKeybindingsPressed
                    ]
                    [ Html.text "Reset to defaults" ]
                , Html.button
                    [ HA.class "button"
                    , HE.onClick ToggleKeybindingsMenuPressed
                    ]
                    [ Html.text "Close" ]
                ]
            ]
        ]


viewKeybindingRow : Model -> BindableAction -> List (Html Msg)
viewKeybindingRow model action =
    List.append
        [ Html.span
            [ HA.class "keybindings-label"
            ]
            [ Html.text ((bindableActionData action).label model) ]
        ]
        (List.indexedMap
            (viewKeybindingSlot model action)
            (bindingSlots action model.keyBindings)
        )


viewKeybindingSlot : Model -> BindableAction -> Int -> Maybe String -> Html Msg
viewKeybindingSlot model action slotIndex slot =
    let
        isListening : Bool
        isListening =
            model.listeningForBinding == Just ( action, slotIndex )
    in
    Html.button
        [ HA.class "button keybinding-key"
        , HA.classList [ ( "keybinding-key-listening", isListening ) ]
        , HE.onClick (RebindSlotPressed action slotIndex)
        ]
        [ Html.text
            (if isListening then
                "Press a key…"

             else
                slot
                    |> Maybe.map (menuCode model)
                    |> Maybe.withDefault "—"
            )
        ]
