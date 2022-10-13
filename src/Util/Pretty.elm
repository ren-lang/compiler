module Util.Pretty exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Pretty



--


doubleline : Pretty.Doc ()
doubleline =
    Pretty.line |> Pretty.a Pretty.line


when : Bool -> Pretty.Doc () -> Pretty.Doc ()
when true doc =
    if true then
        doc

    else
        Pretty.empty


maybe : Maybe a -> (a -> Pretty.Doc ()) -> Pretty.Doc ()
maybe just doc =
    Maybe.map doc just |> Maybe.withDefault Pretty.empty


concat : List (Pretty.Doc ()) -> Pretty.Doc ()
concat =
    List.foldl Pretty.a Pretty.empty
