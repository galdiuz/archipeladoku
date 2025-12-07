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
import List.Extra
import Random
import Set exposing (Set)
import Set.Extra


port checkLocation : Int -> Cmd msg
port connect : Encode.Value -> Cmd msg
port generateBoard : Encode.Value -> Cmd msg
port hintForItem : String -> Cmd msg
port log : String -> Cmd msg
port scoutLocations : List Int -> Cmd msg
port sendMessage : String -> Cmd msg

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
    , cellBlocks : Dict ( Int, Int ) (List Engine.Area)
    , cellBoards : Dict ( Int, Int ) (List Engine.Area)
    , current : Dict ( Int, Int ) CellValue
    , errors : Dict ( Int, Int ) (Set Int)
    , generationProgress : ( String, Float )
    , gameIsLocal : Bool
    , gameState : GameState
    , hints : Dict Int Hint
    , hintCost : Int
    , hintPoints : Int
    , host : String
    , lockedBlocks : List ( Int, Int )
    , messageInput : String
    , messages : List Message
    , pendingCellChanges : Set ( Int, Int )
    , pendingItems : List Item
    , pendingScoutLocations : Set Int
    , pendingSolvedBlocks : Set ( Int, Int )
    , player : String
    , puzzleAreas : Engine.PuzzleAreas
    , scoutedItems : Dict Int Hint
    , seed : Random.Seed
    , selectedCell : Maybe ( Int, Int )
    , solution : Dict ( Int, Int ) Int
    , solveRandomCellUses : Int
    , solveSelectedCellUses : Int -- TODO: Need to persist these counts
    , unlockedBlocks : Set ( Int, Int )
    }


type Msg
    = CellSelected ( Int, Int )
    | ConnectPressed
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
    | KeyPressed Bool Int
    | MessageInputChanged String
    | PlayLocalPressed
    | PlayerInputChanged String
    | SendMessagePressed
    | SolveRandomCellPressed
    | SolveSelectedCellPressed


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
    ( { blockSize = 0
      , cellBlocks = Dict.empty
      , cellBoards = Dict.empty
      , current = Dict.empty
      , errors = Dict.empty
      , generationProgress = ( "", 0 )
      , gameIsLocal = False
      , gameState = MainMenu
      , hints = Dict.empty
      , hintCost = 0
      , hintPoints = 0
      , host = "localhost:8123"
      , lockedBlocks = []
      , messageInput = ""
      , messages = []
      , pendingCellChanges = Set.empty
      , pendingItems = []
      , pendingScoutLocations = Set.empty
      , pendingSolvedBlocks = Set.empty
      , player = "Player1"
      , puzzleAreas =
            { blocks = []
            , boards = []
            , rows = []
            , cols = []
            }
      , scoutedItems = Dict.empty
      , seed = Random.initialSeed flags.seed
      , selectedCell = Nothing
      , solution = Dict.empty
      , solveRandomCellUses = 0
      , solveSelectedCellUses = 0
      , unlockedBlocks = Set.empty
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
        , Browser.Events.onKeyPress <| Decode.oneOf [ keyCodeDecoder, keyDecoder ]
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
                                KeyPressed False 10
                                    |> Decode.succeed

                            else if Char.isDigit char then
                                Char.toCode char - Char.toCode '0'
                                    |> KeyPressed False
                                    |> Decode.succeed

                            else
                                Char.toCode (Char.toUpper char) - Char.toCode 'A' + 11
                                    |> KeyPressed False
                                    |> Decode.succeed

                        else
                            Decode.fail key

                    Nothing ->
                        Decode.fail key
            )


keyCodeDecoder : Decode.Decoder Msg
keyCodeDecoder =
    Decode.field "code" Decode.string
        |> Decode.andThen
            (\code ->
                case code of
                    "Digit1" -> Decode.succeed 1
                    "Digit2" -> Decode.succeed 2
                    "Digit3" -> Decode.succeed 3
                    "Digit4" -> Decode.succeed 4
                    "Digit5" -> Decode.succeed 5
                    "Digit6" -> Decode.succeed 6
                    "Digit7" -> Decode.succeed 7
                    "Digit8" -> Decode.succeed 8
                    "Digit9" -> Decode.succeed 9
                    "Digit0" -> Decode.succeed 10
                    "Numpad1" -> Decode.succeed 1
                    "Numpad2" -> Decode.succeed 2
                    "Numpad3" -> Decode.succeed 3
                    "Numpad4" -> Decode.succeed 4
                    "Numpad5" -> Decode.succeed 5
                    "Numpad6" -> Decode.succeed 6
                    "Numpad7" -> Decode.succeed 7
                    "Numpad8" -> Decode.succeed 8
                    "Numpad9" -> Decode.succeed 9
                    "Numpad0" -> Decode.succeed 10
                    "KeyA" -> Decode.succeed 11
                    "KeyB" -> Decode.succeed 12
                    "KeyC" -> Decode.succeed 13
                    "KeyD" -> Decode.succeed 14
                    "KeyE" -> Decode.succeed 15
                    "KeyF" -> Decode.succeed 16
                    _ -> Decode.fail code
            )
        |> Decode.andThen
            (\number ->
                Decode.field "shiftKey" Decode.bool
                    |> Decode.andThen
                        (\shift ->
                            Decode.succeed (KeyPressed shift number)
                        )
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                        (List.range 1 board.unlockCount)
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
                                    if id >= 1000000 then
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

        KeyPressed shift number ->
            case model.selectedCell of
                Just ( row, col ) ->
                    if cellIsVisible model ( row, col ) then
                        let
                            newCurrent : Dict ( Int, Int ) CellValue
                            newCurrent =
                                if number < 1 || number > model.blockSize then
                                    model.current

                                else
                                    if shift then
                                        Dict.update
                                            ( row, col )
                                            (toggleNumber number)
                                            model.current

                                    else
                                        Dict.insert
                                            ( row, col )
                                            (Single number)
                                            model.current
                        in
                        ( { model
                            | current = newCurrent
                            , pendingCellChanges = Set.insert ( row, col ) model.pendingCellChanges
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

        MessageInputChanged value ->
            ( { model | messageInput = value }
            , Cmd.none
            )

        PlayLocalPressed ->
            ( { model
                | gameIsLocal = True
                , gameState = Generating
              }
            , generateBoard
                (Json.encodeGenerateArgs
                    { blockSize = 4
                    , numberOfBoards = 25
                    , seed = 1
                    }
                )
            )

        PlayerInputChanged value ->
            ( { model | player = value }
            , Cmd.none
            )

        SendMessagePressed ->
            ( { model | messageInput = "" }
            , if String.isEmpty model.messageInput || model.gameIsLocal then
                Cmd.none

              else
                sendMessage model.messageInput
            )

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
                                    if cellIsVisible model cell then
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
                                            , " could not be used because the cell is already given."
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
    Set.foldl
        (andThen << updateStateSolvedBlock)
        ( { model | pendingSolvedBlocks = Set.empty }
        , Cmd.none
        )
        model.pendingSolvedBlocks


updateStateErrors : Model -> ( Model, Cmd Msg )
updateStateErrors model =
    ( { model | errors = getBoardErrors model }
    , Cmd.none
    )


updateStateScoutLocations : Model -> ( Model, Cmd Msg )
updateStateScoutLocations model =
    if Set.isEmpty model.pendingScoutLocations || model.gameIsLocal then
        ( model
        , Cmd.none
        )

    else
        ( { model | pendingScoutLocations = Set.empty }
        , scoutLocations (Set.toList model.pendingScoutLocations)
        )


updateStateCellChange : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
updateStateCellChange updatedCell initialModel =
    let
        listFoldlChain : (a -> b -> b) -> List a -> b -> b
        listFoldlChain fun list initial =
            List.foldl fun initial list
    in
    ( initialModel
    , Cmd.none
    )
        |> listFoldlChain
            (\block ->
                andThen
                    (\model ->
                        let
                            blockCells : List ( Int, Int )
                            blockCells =
                                Engine.getAreaCells block
                        in
                        if List.all (cellIsSolved model) blockCells then
                            let
                                newModel : Model
                                newModel =
                                    { model
                                        | current =
                                            List.foldl
                                                (\cell acc ->
                                                    Dict.insert
                                                        cell
                                                        (Given (Dict.get cell model.solution |> Maybe.withDefault 0))
                                                        acc
                                                )
                                                model.current
                                                blockCells
                                    }
                            in
                            if model.gameIsLocal then
                                ( newModel
                                , Cmd.none
                                )
                                    |> andThen unlockNextBlock

                            else
                                ( newModel
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
            (\boardArea ->
                andThen
                    (\model ->
                        if List.all (cellIsSolved model) (Engine.getAreaCells boardArea) then
                            if model.gameIsLocal then
                                ( model
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


unlockNextBlock : Model -> ( Model, Cmd Msg )
unlockNextBlock model =
    case model.lockedBlocks of
        block :: remainingBlocks ->
            let
                unlockedModel : Model
                unlockedModel =
                    { model
                        | lockedBlocks = remainingBlocks
                        , unlockedBlocks = Set.insert block model.unlockedBlocks
                    }
            in
            ( { unlockedModel
                | pendingScoutLocations =
                    unlockedModel.pendingScoutLocations
                        |> Set.insert (cellToBlockId block)
                        |> Set.union
                            (unlockedBoardsAtCell unlockedModel block
                                |> Set.map cellToBoardId
                            )
              }
            , Cmd.none
            )

        [] ->
            ( model
            , Cmd.none
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
                                        |> List.filter (cellIsVisible model)
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
                                        |> List.filter (cellIsVisible model)
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
                                        |> List.filter (cellIsVisible model)
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

        Block cell ->
            let
                unlockedModel : Model
                unlockedModel =
                    { model
                        | lockedBlocks =
                            List.filter ((/=) cell) model.lockedBlocks
                            , unlockedBlocks =
                                Set.insert cell model.unlockedBlocks
                    }
            in
            ( { unlockedModel
                | pendingScoutLocations =
                    unlockedModel.pendingScoutLocations
                        |> Set.insert (cellToBlockId cell)
                        |> Set.union
                            (unlockedBoardsAtCell unlockedModel cell
                                |> Set.map cellToBoardId
                            )
              }
            , Cmd.none
            )

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


updateStateSolvedBlock : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
updateStateSolvedBlock ( row, col ) model =
    let
        cells : List ( Int, Int )
        cells =
            Dict.get ( row, col ) model.cellBlocks
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
      }
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


cellIsVisible : Model -> ( Int, Int ) -> Bool
cellIsVisible model cell =
    Set.intersect
        (Dict.get cell model.cellBlocks
            |> Maybe.withDefault []
            |> List.map (\blockArea -> ( blockArea.startRow, blockArea.startCol ))
            |> Set.fromList
        )
        model.unlockedBlocks
        |> Set.isEmpty
        |> not


unlockedBoardsAtCell : Model -> ( Int, Int ) -> Set ( Int, Int )
unlockedBoardsAtCell model cell =
    Dict.get cell model.cellBoards
        |> Maybe.withDefault []
        |> List.map (\area -> ( area.startRow, area.startCol ))
        |> List.filter
            (\( boardRow, boardCol ) ->
                Engine.buildPuzzleAreas model.blockSize boardRow boardCol
                    |> .blocks
                    |> List.map (\blockArea -> ( blockArea.startRow, blockArea.startCol ))
                    |> Set.fromList
                    |> Set.Extra.isSubsetOf model.unlockedBlocks
            )
        |> Set.fromList


view : Model -> Html Msg
view model =
    case model.gameState of
        MainMenu ->
            viewMenu model

        Connecting ->
            Html.div
                []
                [ Html.text "Connecting..." ]

        Generating ->
            Html.div
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
        []
        [ Html.form
            [ HE.onSubmit ConnectPressed ]
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
        , Html.br [] []
        , Html.button
            [ HE.onClick PlayLocalPressed ]
            [ Html.text "Play Singleplayer" ]
        ]


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
    Html.div
        [ HA.class "board"
        , HA.class <| "block-" ++ String.fromInt model.blockSize
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
            cellIsVisible model ( row, col )

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

        cellIsGiven : Bool
        cellIsGiven =
            Dict.get ( row, col ) model.current
                |> Maybe.map isGiven
                |> Maybe.withDefault False

        cellIsMultiple : Bool
        cellIsMultiple =
            Dict.get ( row, col ) model.current
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
        [ Html.text
            (String.concat
                [ "Cell "
                , rowToLabel row
                , String.fromInt col
                , " (r"
                , String.fromInt row
                , "c"
                , String.fromInt col
                , ")"
                ]
            )
        ]


viewBlockInfo : Model -> Engine.Area -> Html Msg
viewBlockInfo model block =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ Html.text
            (String.concat
                [ "Block "
                , rowToLabel block.startRow
                , String.fromInt block.startCol
                , " (r"
                , String.fromInt block.startRow
                , "c"
                , String.fromInt block.startCol
                , ")"
                ]
            )

        , if model.gameIsLocal then
            Html.text ""

          else
            case Dict.get (cellToBlockId ( block.startRow, block.startCol )) model.scoutedItems of
                Just item ->
                    Html.div
                        []
                        [ Html.text
                            (String.concat
                                [ "Reward: "
                                , item.itemName
                                , " ("
                                , itemClassToString item.itemClass
                                , ", "
                                , item.playerName
                                , ", "
                                , item.gameName
                                , ")"
                                ]
                            )
                        ]

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


viewBoardInfo : Model -> Engine.Area -> Html Msg
viewBoardInfo model board =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ Html.text
            (String.concat
                [ "Board "
                , rowToLabel board.startRow
                , String.fromInt board.startCol
                , " (r"
                , String.fromInt board.startRow
                , "c"
                , String.fromInt board.startCol
                , ")"
                ]
            )

        , if model.gameIsLocal then
            Html.text ""

          else
            case Dict.get (cellToBoardId ( board.startRow, board.startCol )) model.scoutedItems of
                Just item ->
                    Html.div
                        []
                        [ Html.text
                            (String.concat
                                [ "Reward: "
                                , item.itemName
                                , " ("
                                , itemClassToString item.itemClass
                                , ", "
                                , item.playerName
                                , ", "
                                , item.gameName
                                , ")"
                                ]
                            )
                        ]

                Nothing ->
                    Html.div
                        []
                        [ Html.text "Reward: ???" ]

        -- TODO: List unlock conditions
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


cellToBlockId : ( Int, Int ) -> Int
cellToBlockId ( row, col ) =
    1000000 + row * 1000 + col


cellToBoardId : ( Int, Int ) -> Int
cellToBoardId ( row, col ) =
    2000000 + row * 1000 + col


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
