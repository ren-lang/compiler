module Node.Chalk exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Util.FFI



-- TYPES -----------------------------------------------------------------------


{-| -}
type Proxy
    = Proxy Json.Decode.Value


{-| -}
type alias Chalk =
    { black : String -> String
    , red : String -> String
    , green : String -> String
    , yellow : String -> String
    , blue : String -> String
    , magenta : String -> String
    , cyan : String -> String
    , white : String -> String
    , gray : String -> String
    , grey : String -> String

    -- The proxy object partially applied to all the functions above. You need
    -- this if you want to call any of the functions in this module directly
    -- instead of using the `Chalk` record.
    , this : Proxy
    }



--


{-| -}
decoder : Json.Decode.Decoder Chalk
decoder =
    Json.Decode.map2 Tuple.pair (Json.Decode.map Proxy Json.Decode.value) (Json.Decode.field "FFI.Chalk" Json.Decode.bool)
        |> Json.Decode.andThen
            (\( proxy, isChalkProxy ) ->
                if isChalkProxy then
                    Json.Decode.succeed
                        { black = proxy |> black
                        , red = proxy |> red
                        , green = proxy |> green
                        , yellow = proxy |> yellow
                        , blue = proxy |> blue
                        , magenta = proxy |> magenta
                        , cyan = proxy |> cyan
                        , white = proxy |> white
                        , gray = proxy |> gray
                        , grey = proxy |> grey

                        --
                        , this = proxy
                        }

                else
                    Json.Decode.fail <|
                        "Uh oh, it looks like there was an internal error in "
                            ++ "the `FFI.Chalk.elm` module. Please open an issue at "
                            ++ "https://github.com/ren-lang/compiler."
            )



--


black : Proxy -> String -> String
black (Proxy proxy) input =
    Maybe.withDefault input <|
        Util.FFI.call proxy
            "black"
            [ Json.Encode.string input ]
            Json.Decode.string


red : Proxy -> String -> String
red (Proxy proxy) input =
    Maybe.withDefault input <|
        Util.FFI.call proxy
            "red"
            [ Json.Encode.string input ]
            Json.Decode.string


green : Proxy -> String -> String
green (Proxy proxy) input =
    Maybe.withDefault input <|
        Util.FFI.call proxy
            "green"
            [ Json.Encode.string input ]
            Json.Decode.string


yellow : Proxy -> String -> String
yellow (Proxy proxy) input =
    Maybe.withDefault input <|
        Util.FFI.call proxy
            "yellow"
            [ Json.Encode.string input ]
            Json.Decode.string


blue : Proxy -> String -> String
blue (Proxy proxy) input =
    Maybe.withDefault input <|
        Util.FFI.call proxy
            "blue"
            [ Json.Encode.string input ]
            Json.Decode.string


magenta : Proxy -> String -> String
magenta (Proxy proxy) input =
    Maybe.withDefault input <|
        Util.FFI.call proxy
            "magenta"
            [ Json.Encode.string input ]
            Json.Decode.string


cyan : Proxy -> String -> String
cyan (Proxy proxy) input =
    Maybe.withDefault input <|
        Util.FFI.call proxy
            "cyan"
            [ Json.Encode.string input ]
            Json.Decode.string


white : Proxy -> String -> String
white (Proxy proxy) input =
    Maybe.withDefault input <|
        Util.FFI.call proxy
            "white"
            [ Json.Encode.string input ]
            Json.Decode.string


grey : Proxy -> String -> String
grey =
    gray


gray : Proxy -> String -> String
gray (Proxy proxy) input =
    Maybe.withDefault input <|
        Util.FFI.call proxy
            "gray"
            [ Json.Encode.string input ]
            Json.Decode.string
