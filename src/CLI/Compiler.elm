port module CLI.Compiler exposing (main)


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Stage.Parse as Cherry
import Cherry.Stage.Emit as Cherry
import Json.Decode
import Result.Extra
import Parser.Extra


-- MAIN ------------------------------------------------------------------------


{-| -}
main : Program Json.Decode.Value Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL -----------------------------------------------------------------------


{-| -}
type alias Model =
    { debug : Bool
    }

{-| -}
init : Json.Decode.Value -> ( Model, Cmd Msg )
init json =
    let
        dummyModel =
            { debug = False
            }
    in
    case Json.Decode.decodeValue modelDecoder json of
        Ok model ->
            ( model
            , Cmd.none 
            )

        Err _ ->
            ( dummyModel
            , fromError "Couldn't decode flags, exiting compiler." 
            )

{-| -}
modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    Json.Decode.map Model
        (Json.Decode.field "debug" Json.Decode.bool)


-- UPDATE ----------------------------------------------------------------------


{-| -}
type Msg
    = CompileFile File

{-| -}
type alias File =
    { source : String
    , name : String
    , path : String
    }

{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompileFile { source, name, path } ->
            if model.debug then
                Cherry.parseModule source
                    |> Result.map Cherry.emitJSON
                    |> Result.map (\s -> File s (name ++ ".json") path)
                    |> Result.map fromEmitter
                    |> Result.mapError (Parser.Extra.deadEndsToString)
                    |> Result.mapError fromError
                    |> Result.Extra.unwrap
                    |> Tuple.pair model
            
            else
                Cherry.parseModule source
                    |> Result.map Cherry.emitJavaScript
                    |> Result.map (\s -> File s (name ++ ".js") path)
                    |> Result.map fromEmitter
                    |> Result.mapError (Parser.Extra.deadEndsToString)
                    |> Result.mapError fromError
                    |> Result.Extra.unwrap
                    |> Tuple.pair model

{-| -}
port fromEmitter : File -> Cmd msg

{-| -}
port fromError : String -> Cmd msg


-- SUBSCRIPTIONS ---------------------------------------------------------------


{-| -}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ toParser CompileFile
        ]

{-| -}
port toParser : (File -> msg) -> Sub msg
