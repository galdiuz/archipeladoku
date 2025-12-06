port module Archipeladoku.Worker exposing (..)

import Archipeladoku.Engine as Engine
import Archipeladoku.Json as Json
import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Task


port sendBoard : Encode.Value -> Cmd msg
port receiveGenerateArgs : (Encode.Value -> msg) -> Sub msg
port receiveGenerateArgs2 : (Encode.Value -> msg) -> Sub msg


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = GotGenerateArgs Encode.Value
    | GotGenerateArgs2 Encode.Value
    | GotGenerationState Engine.BoardGenerationState


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( ()
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveGenerateArgs GotGenerateArgs
        , receiveGenerateArgs2 GotGenerateArgs2
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGenerateArgs value ->
            case Decode.decodeValue Json.generateArgsDecoder value of
                Ok args ->
                    ( model
                    , Engine.generate args
                        |> Task.succeed
                        |> Task.perform GotGenerationState
                    )

                Err err ->
                    ( model, Cmd.none )

        GotGenerateArgs2 value ->
            case Decode.decodeValue Json.decodeGenerateArgs2 value of
                Ok args ->
                    ( model
                    , Engine.generateFromServer args
                        |> Task.succeed
                        |> Task.perform GotGenerationState
                    )

                Err err ->
                    ( model, Cmd.none )

        GotGenerationState state ->
            case state of
                Engine.Completed board ->
                    ( model
                    , sendBoard (Json.encodeBoard board)
                    )

                Engine.Failed err ->
                    let
                        _ = Debug.log "Generation failed with error:" err
                    in
                    ( model, Cmd.none )

                Engine.PlacingNumbers genState ->
                    let
                        _ = Debug.log "Placing numbers, remaining clusters:" genState.remainingClusters
                    in
                    ( model
                    , Cmd.batch
                        [ Engine.continueGeneration state
                            |> Task.succeed
                            |> Task.perform GotGenerationState
                        -- TODO: Send progress to UI
                        ]
                    )

                Engine.RemovingGivens genState ->
                    let
                        _ = Debug.log "Removing givens, remaining clusters:" genState.remainingClusters
                    in
                    ( model
                    , Cmd.batch
                        [ Engine.continueGeneration state
                            |> Task.succeed
                            |> Task.perform GotGenerationState
                        -- TODO: Send progress to UI
                        ]
                    )
