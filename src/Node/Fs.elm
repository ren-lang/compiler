module Node.Fs exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Util.FFI



-- TYPES -----------------------------------------------------------------------


{-| -}
type Proxy
    = Proxy Json.Decode.Value


{-| -}
type alias Fs =
    { copyFile : String -> String -> ()
    , exists : String -> Bool
    , isDir : String -> Bool
    , isFile : String -> Bool
    , makeDir : Bool -> String -> ()
    , readDir : String -> List String
    , readFile : String -> String -> Maybe String
    , writeFile : String -> String -> ()

    -- The proxy object partially applied to all the functions above. You need
    -- this if you want to call any of the functions in this module directly
    -- instead of using the `Fs` record.
    , this : Proxy
    }



-- CONSTRUCTORS ----------------------------------------------------------------


from : Json.Decode.Value -> Maybe Fs
from =
    Json.Decode.decodeValue decoder >> Result.toMaybe


{-| -}
decoder : Json.Decode.Decoder Fs
decoder =
    Json.Decode.map2 Tuple.pair (Json.Decode.map Proxy Json.Decode.value) (Json.Decode.field "FFI.Fs" Json.Decode.bool)
        |> Json.Decode.andThen
            (\( proxy, isFsProxy ) ->
                if isFsProxy then
                    Json.Decode.succeed
                        { copyFile = proxy |> copyFile
                        , exists = proxy |> exists
                        , isDir = proxy |> isDir
                        , isFile = proxy |> isFile
                        , makeDir = proxy |> makeDir
                        , readDir = proxy |> readDir
                        , readFile = proxy |> readFile
                        , writeFile = proxy |> writeFile

                        --
                        , this = proxy
                        }

                else
                    Json.Decode.fail <|
                        "Uh oh, it looks like there was an internal error in "
                            ++ "the `FFI.Fs.elm` module. Please open an issue at "
                            ++ "https://github.com/ren-lang/compiler."
            )



--


{-| Synchronously copies src to dest. By default, dest is overwritten if it already
exists. Node.js makes no guarantees about the atomicity of the copy operation.
If an error occurs after the destination file has been opened for writing,
Node.js will attempt to remove the destination.
-}
copyFile : Proxy -> String -> String -> ()
copyFile (Proxy proxy) src dest =
    Maybe.withDefault () <|
        Util.FFI.call proxy
            "copyFileSync"
            (List.map Json.Encode.string [ src, dest ])
            (Json.Decode.succeed ())


exists : Proxy -> String -> Bool
exists (Proxy proxy) path =
    Maybe.withDefault False <|
        Util.FFI.call proxy
            "existsSync"
            [ Json.Encode.string path ]
            Json.Decode.bool


isDir : Proxy -> String -> Bool
isDir (Proxy proxy) path =
    Maybe.withDefault False <|
        Util.FFI.call proxy
            "isDirectory"
            [ Json.Encode.string path ]
            Json.Decode.bool


isFile : Proxy -> String -> Bool
isFile (Proxy proxy) path =
    Maybe.withDefault False <|
        Util.FFI.call proxy
            "isFile"
            [ Json.Encode.string path ]
            Json.Decode.bool


makeDir : Proxy -> Bool -> String -> ()
makeDir (Proxy proxy) recursive path =
    Maybe.withDefault () <|
        Util.FFI.call proxy
            "mkdirSync"
            [ Json.Encode.string path, Json.Encode.object [ ( "recursive", Json.Encode.bool recursive ) ] ]
            (Json.Decode.succeed ())


readDir : Proxy -> String -> List String
readDir (Proxy proxy) path =
    Maybe.withDefault [] <|
        Util.FFI.call proxy
            "readdirSync"
            [ Json.Encode.string path, Json.Encode.object [ ( "encoding", Json.Encode.string "utf8" ) ] ]
            (Json.Decode.list Json.Decode.string)


readFile : Proxy -> String -> String -> Maybe String
readFile (Proxy proxy) path encoding =
    Util.FFI.call proxy
        "readFileSync"
        [ Json.Encode.string path, Json.Encode.object [ ( "encoding", Json.Encode.string encoding ) ] ]
        Json.Decode.string


writeFile : Proxy -> String -> String -> ()
writeFile (Proxy proxy) path data =
    Maybe.withDefault () <|
        Util.FFI.call proxy
            "writeFileSync"
            [ Json.Encode.string path, Json.Encode.string data, Json.Encode.object [ ( "encoding", Json.Encode.string "utf8" ) ] ]
            (Json.Decode.succeed ())
