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


when : Bool -> Pretty.Doc t -> Pretty.Doc t
when condition doc =
    if condition then
        doc
    else
        Pretty.empty


mapNonEmptyList : List a -> (List a -> Pretty.Doc t) -> Pretty.Doc t
mapNonEmptyList list mapper =
    if List.isEmpty list then
        Pretty.empty
    else
        mapper list
