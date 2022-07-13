module Node.Path exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Util.FFI



-- TYPES -----------------------------------------------------------------------


{-| -}
type Proxy
    = Proxy Json.Decode.Value


{-| -}
type alias Path =
    { basename : String -> Maybe String -> String
    , dirname : String -> String
    , extname : String -> String
    , isAbsolute : String -> Bool
    , join : List String -> String
    , normalise : String -> String
    , relative : String -> String -> String
    , resolve : List String -> String
    , sep : String

    -- The proxy object partially applied to all the functions above. You need
    -- this if you want to call any of the functions in this module directly
    -- instead of using the `Path` record.
    , this : Proxy
    }



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
decoder : Json.Decode.Decoder Path
decoder =
    Json.Decode.map2 Tuple.pair (Json.Decode.map Proxy Json.Decode.value) (Json.Decode.field "FFI.Path" Json.Decode.bool)
        |> Json.Decode.andThen
            (\( proxy, isPathProxy ) ->
                if isPathProxy then
                    Json.Decode.succeed
                        { basename = proxy |> basename
                        , dirname = proxy |> dirname
                        , extname = proxy |> extname
                        , isAbsolute = proxy |> isAbsolute
                        , join = proxy |> join
                        , normalise = proxy |> normalise
                        , relative = proxy |> relative
                        , resolve = proxy |> resolve
                        , sep = proxy |> sep

                        --
                        , this = proxy
                        }

                else
                    Json.Decode.fail <|
                        "Uh oh, it looks like there was an internal error in "
                            ++ "the `Util.FFI.Path.elm` module. Please open an issue at "
                            ++ "https://github.com/ren-lang/compiler."
            )



--


{-| Returns the last portion of a path, similar to the Unix basename command.
Trailing directory separators are ignored. Separators are platform-dependent and
can be checked by calling `sep`.
An optional file extension may be provided that will be stripped from the returned
string if present.
basename proxy "/foo/bar/baz/asdf/quux.html" Nothing
-- "quux.html"
basename proxy "/foo/bar/baz/asdf/quux.html" (Just ".html")
-- "quux"
-}
basename : Proxy -> String -> Maybe String -> String
basename (Proxy proxy) path maybeExt =
    case maybeExt of
        Just ext ->
            Util.FFI.call proxy "basename" [ Json.Encode.string path, Json.Encode.string ext ] Json.Decode.string
                |> Maybe.withDefault ""

        Nothing ->
            Util.FFI.call proxy "basename" [ Json.Encode.string path ] Json.Decode.string
                |> Maybe.withDefault ""


{-| Returns the directory name of a path, similar to the Unix `dirname` command.
Trailing directory separators are ignored. Separators are platform-dependent and
can be checked by calling `sep`.
dirname proxy "/foo/bar/baz/asdf/quux.html"
-- "/foo/bar/baz/asdf"
-}
dirname : Proxy -> String -> String
dirname (Proxy proxy) path =
    Util.FFI.call proxy "dirname" [ Json.Encode.string path ] Json.Decode.string
        |> Maybe.withDefault ""


{-| Returns the extension of the path, from the last occurrence of the `.` (period)
character to end of string in the last portion of the path. If there is no `.` in
the last portion of the path, or if there are no `.` characters other than the first
character of the basename of path, an empty string is returned.
extname proxy "index.html"
-- ".html"
extname proxy "index.coffee.md"
-- ".md"
extname proxy "index."
-- "."
extname proxy "index"
-- ""
extname proxy ".index"
-- ""
extname proxy ".index.md"
-- ".md"
-}
extname : Proxy -> String -> String
extname (Proxy proxy) path =
    Maybe.withDefault "" <|
        Util.FFI.call proxy
            "extname"
            [ Json.Encode.string path ]
            Json.Decode.string


{-| Determines if path is an absolute path. If the given path is a zero-length
string, `False` will be returned.
isAbsolute proxy "/foo/bar"
-- True
isAbsolute proxy "/baz/.."
-- True
isAbsolute proxy "qux/"
-- False
isAbsolute proxy "."
-- False
-}
isAbsolute : Proxy -> String -> Bool
isAbsolute (Proxy proxy) path =
    Util.FFI.call proxy "isAbsolute" [ Json.Encode.string path ] Json.Decode.bool
        |> Maybe.withDefault False


{-| Joins all given path segments together using the platform-specific separator
as a delimiter, then normalizes the resulting path. Zero-length path segments are
ignored. If the joined path string is a zero-length string then `"."` will be
returned, representing the current working directory.
join proxy [ "/foo", "bar", "baz/asdf", "quux", ".." ]
-- "/foo/bar/baz/asdf"
-}
join : Proxy -> List String -> String
join (Proxy proxy) paths =
    Util.FFI.call proxy "join" (List.map Json.Encode.string paths) Json.Decode.string
        |> Maybe.withDefault ""


{-| Normalises the given path, resolving `".."` and `"."` segments. When multiple,
sequential path segment separation characters are found (e.g. `/` on POSIX and
either `\` or `/` on Windows), they are replaced by a single instance of the
platform-specific path segment separator (`/` on POSIX and `\` on Windows). Trailing
separators are preserved.
If the path is a zero-length string, `"."` is returned, representing the current
working directory.
-}
normalise : Proxy -> String -> String
normalise (Proxy proxy) path =
    Util.FFI.call proxy "normalize" [ Json.Encode.string path ] Json.Decode.string
        |> Maybe.withDefault ""


{-| Returns the relative path from from to to based on the current working directory.
If from and to each resolve to the same path (after calling `resolve` on each),
a zero-length string is returned.
If a zero-length string is passed as from or to, the current working directory will
be used instead of the zero-length strings.
relative proxy "/data/orandea/test/aaa" "/data/orandea/impl/bbb"
-- "../../impl/bbb"
-}
relative : Proxy -> String -> String -> String
relative (Proxy proxy) from to =
    Util.FFI.call proxy "relative" [ Json.Encode.string from, Json.Encode.string to ] Json.Decode.string
        |> Maybe.withDefault ""


resolve : Proxy -> List String -> String
resolve (Proxy proxy) paths =
    Util.FFI.call proxy "resolve" (List.map Json.Encode.string paths) Json.Decode.string
        |> Maybe.withDefault ""


sep : Proxy -> String
sep (Proxy proxy) =
    Util.FFI.get proxy "sep" Json.Decode.string
        |> Maybe.withDefault "/"
