module Ren.Compiler.Emit.Util exposing (..)

import Data.Tuple2
import Pretty



--


{-| -}
array : List (Pretty.Doc t) -> Pretty.Doc t
array elements =
    elements
        |> Pretty.join (Pretty.string ", ")
        |> Pretty.surround (Pretty.string "[ ") (Pretty.string " ]")


{-| -}
object : List ( String, Pretty.Doc t ) -> Pretty.Doc t
object entries =
    entries
        |> List.map (Tuple.mapFirst Pretty.string >> Data.Tuple2.asList (Pretty.join (Pretty.string ": ")))
        |> Pretty.join (Pretty.string ", ")
        |> Pretty.surround (Pretty.string "[ ") (Pretty.string " ]")


{-| -}
string : Pretty.Doc t -> Pretty.Doc t
string text =
    Pretty.surround (Pretty.char '"') (Pretty.char '"') text


{-| -}
binop : String -> Pretty.Doc t -> Pretty.Doc t -> Pretty.Doc t
binop op lhs rhs =
    Pretty.join (Pretty.string <| " " ++ op ++ " ")
        [ lhs, rhs ]


splitBinop : String -> Pretty.Doc t -> Pretty.Doc t -> Pretty.Doc t
splitBinop op lhs rhs =
    lhs
        |> Pretty.a Pretty.line
        |> Pretty.a (Pretty.string <| op ++ " ")
        |> Pretty.a rhs
        |> Pretty.nest 4
        |> Pretty.align


{-| -}
doubleline : Pretty.Doc t
doubleline =
    Pretty.append Pretty.line Pretty.line


{-| -}
when : Bool -> Pretty.Doc t -> Pretty.Doc t
when condition doc =
    if condition then
        doc

    else
        Pretty.empty


{-| -}
fromList : (a -> Pretty.Doc t) -> List a -> Pretty.Doc t
fromList f list =
    List.map f list |> Pretty.join Pretty.empty
