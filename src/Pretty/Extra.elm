module Pretty.Extra exposing (..)

import Pretty


spaces : List (Pretty.Doc t) -> Pretty.Doc t
spaces =
    Pretty.join Pretty.space


singleQuotes : String -> Pretty.Doc t
singleQuotes s =
    Pretty.string s
        |> Pretty.surround (Pretty.char '\'') (Pretty.char '\'')


doubleLine : Pretty.Doc t
doubleLine =
    Pretty.line |> Pretty.a Pretty.line
