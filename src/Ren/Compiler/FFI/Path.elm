module Ren.Compiler.FFI.Path exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Compiler.FFI exposing (Task)



--


{-| Returns the last portion of a path, similar to the Unix basename command.
Trailing directory separators are ignored. Separators are platform-dependent and
can be checked by calling `sep`.
An optional file extension may be provided that will be stripped from the returned
string if present.
basename "Path" "/foo/bar/baz/asdf/quux.html" Nothing
-- "quux.html"
basename "Path" "/foo/bar/baz/asdf/quux.html" (Just ".html")
-- "quux"
-}
basename : String -> Maybe String -> Task String
basename path maybeExt =
    case maybeExt of
        Just ext ->
            Ren.Compiler.FFI.call "Path" "basename" [ Json.Encode.string path, Json.Encode.string ext ] Json.Decode.string

        Nothing ->
            Ren.Compiler.FFI.call "Path" "basename" [ Json.Encode.string path ] Json.Decode.string


{-| Returns the directory name of a path, similar to the Unix `dirname` command.
Trailing directory separators are ignored. Separators are platform-dependent and
can be checked by calling `sep`.
dirname "Path" "/foo/bar/baz/asdf/quux.html"
-- "/foo/bar/baz/asdf"
-}
dirname : String -> Task String
dirname path =
    Ren.Compiler.FFI.call "Path" "dirname" [ Json.Encode.string path ] Json.Decode.string


{-| Returns the extension of the path, from the last occurrence of the `.` (period)
character to end of string in the last portion of the path. If there is no `.` in
the last portion of the path, or if there are no `.` characters other than the first
character of the basename of path, an empty string is returned.

    extname "Path" "index.html"
    -- ".html"
    extname "Path" "index.coffee.md"
    -- ".md"
    extname "Path" "index."
    -- "."
    extname "Path" "index"
    -- ""
    extname "Path" ".index"
    -- ""
    extname "Path" ".index.md"
    -- ".md"

-}
extname : String -> Task String
extname path =
    Ren.Compiler.FFI.call "Path" "extname" [ Json.Encode.string path ] Json.Decode.string


{-| Determines if path is an absolute path. If the given path is a zero-length
string, `False` will be returned.

    isAbsolute "Path" "/foo/bar"
    -- True
    isAbsolute "Path" "/baz/.."
    -- True
    isAbsolute "Path" "qux/"
    -- False
    isAbsolute "Path" "."
    -- False

-}
isAbsolute : String -> Task Bool
isAbsolute path =
    Ren.Compiler.FFI.call "Path" "isAbsolute" [ Json.Encode.string path ] Json.Decode.bool


{-| Joins all given path segments together using the platform-specific separator
as a delimiter, then normalizes the resulting path. Zero-length path segments are
ignored. If the joined path string is a zero-length string then `"."` will be
returned, representing the current working directory.
join "Path" [ "/foo", "bar", "baz/asdf", "quux", ".." ]
-- "/foo/bar/baz/asdf"
-}
join : List String -> Task String
join paths =
    Ren.Compiler.FFI.call "Path" "join" (List.map Json.Encode.string paths) Json.Decode.string


{-| Normalises the given path, resolving `".."` and `"."` segments. When multiple,
sequential path segment separation characters are found (e.g. `/` on POSIX and
either `\` or `/` on Windows), they are replaced by a single instance of the
platform-specific path segment separator (`/` on POSIX and `\` on Windows). Trailing
separators are preserved.
If the path is a zero-length string, `"."` is returned, representing the current
working directory.
-}
normalise : String -> Task String
normalise path =
    Ren.Compiler.FFI.call "Path" "normalize" [ Json.Encode.string path ] Json.Decode.string


{-| Returns the relative path from from to to based on the current working directory.
If from and to each resolve to the same path (after calling `resolve` on each),
a zero-length string is returned.
If a zero-length string is passed as from or to, the current working directory will
be used instead of the zero-length strings.
relative "Path" "/data/orandea/test/aaa" "/data/orandea/impl/bbb"
-- "../../impl/bbb"
-}
relative : String -> String -> Task String
relative from to =
    Ren.Compiler.FFI.call "Path" "relative" [ Json.Encode.string from, Json.Encode.string to ] Json.Decode.string


resolve : List String -> Task String
resolve paths =
    Ren.Compiler.FFI.call "Path" "resolve" (List.map Json.Encode.string paths) Json.Decode.string
