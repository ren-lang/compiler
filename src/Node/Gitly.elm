module Node.Gitly exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Util.FFI



-- TYPES -----------------------------------------------------------------------


{-| -}
type Proxy
    = Proxy Json.Decode.Value


{-| -}
type alias Gitly =
    { fetch : String -> String -> String -> ()

    -- The proxy object partially applied to all the functions above. You need
    -- this if you want to call any of the functions in this module directly
    -- instead of using the `Gitly` record.
    , this : Proxy
    }



--


{-| -}
decoder : Json.Decode.Decoder Gitly
decoder =
    Json.Decode.map2 Tuple.pair (Json.Decode.map Proxy Json.Decode.value) (Json.Decode.field "FFI.Gitly" Json.Decode.bool)
        |> Json.Decode.andThen
            (\( proxy, isGitlyProxy ) ->
                if isGitlyProxy then
                    Json.Decode.succeed
                        { fetch = proxy |> fetch

                        --
                        , this = proxy
                        }

                else
                    Json.Decode.fail <|
                        "Uh oh, it looks like there was an internal error in "
                            ++ "the `FFI.Gitly.elm` module. Please open an issue at "
                            ++ "https://github.com/ren-lang/compiler."
            )



--


fetch : Proxy -> String -> String -> String -> ()
fetch (Proxy proxy) author repo dest =
    Maybe.withDefault () <|
        Util.FFI.call proxy
            "fetch"
            (List.map Json.Encode.string [ author, repo, dest ])
            (Json.Decode.succeed ())
