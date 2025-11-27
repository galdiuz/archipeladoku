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


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = GotGenerateArgs Encode.Value
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
                    let
                        _ = Debug.log "Error decoding generate args:" err
                    in
                    ( model, Cmd.none )

        GotGenerationState state ->
            case state of
                Engine.Completed board ->
                    ( model
                    , sendBoard (Json.encodeBoard board)
                    )

                Engine.Failed err ->
                    let
                        _ = Debug.log "Error generating board:" err
                    in
                    ( model, Cmd.none )

                Engine.Generating genState ->
                    let
                        _ = Debug.log "Still generating, remaining clusters:" genState.remainingClusters
                    in
                    ( model
                    , Cmd.batch
                        [ Engine.continueGeneration state
                            |> Task.succeed
                            |> Task.perform GotGenerationState
                        -- TODO: Send progress to UI
                        ]
                    )
