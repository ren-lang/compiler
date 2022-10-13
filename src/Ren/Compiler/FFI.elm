module Ren.Compiler.FFI exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Http
import Json.Decode
import Json.Encode
import Task



-- TYPES -----------------------------------------------------------------------


type alias Error =
    String


type alias Task a =
    Task.Task Error a



--


do : Task a -> (a -> Task b) -> Task b
do task f =
    Task.andThen f task



--


get : String -> String -> Json.Decode.Decoder response -> Task response
get mod prop decoder =
    Http.task
        { method = "FFI"
        , headers = []
        , url = mod ++ ":" ++ prop
        , body = Http.jsonBody Json.Encode.null
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.GoodStatus_ _ body ->
                            Json.Decode.decodeString decoder body
                                |> Result.mapError Json.Decode.errorToString

                        _ ->
                            Err <| "Error accessing `" ++ mod ++ ":" ++ prop ++ "`"
                )
        , timeout = Nothing
        }


call : String -> String -> List Json.Encode.Value -> Json.Decode.Decoder response -> Task response
call mod fn args decoder =
    Http.task
        { method = "FFI"
        , headers = []
        , url = mod ++ ":" ++ fn
        , body = Http.jsonBody <| Json.Encode.list Basics.identity args
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.GoodStatus_ _ body ->
                            Json.Decode.decodeString decoder body
                                |> Result.mapError Json.Decode.errorToString

                        _ ->
                            ("Error calling `" ++ mod ++ ":" ++ fn ++ "` with ")
                                ++ (Json.Encode.encode 0 <| Json.Encode.list Basics.identity args)
                                |> Err
                )
        , timeout = Nothing
        }
