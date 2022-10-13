module Ren.Data.Span exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode



-- TYPES -----------------------------------------------------------------------


{-| A span represents a range or selection of text in a document.
-}
type alias Span =
    { start : Pos
    , end : Pos
    }


type alias Pos =
    { line : Int
    , column : Int
    }



-- CONSTANTS -------------------------------------------------------------------


empty : Span
empty =
    { start =
        { line = 0
        , column = 0
        }
    , end =
        { line = 0
        , column = 0
        }
    }



-- CONSTRUCTORS ----------------------------------------------------------------


from : ( Int, Int ) -> ( Int, Int ) -> Span
from ( x, y ) ( a, b ) =
    { start = Pos x y, end = Pos a b }



-- QUERIES ---------------------------------------------------------------------


compare : Span -> Span -> Basics.Order
compare a b =
    let
        compA =
            ( toComparable a.start, toComparable a.end )

        compB =
            ( toComparable b.start, toComparable b.end )
    in
    if compA < compB then
        Basics.LT

    else if compB < compA then
        Basics.GT

    else
        Basics.EQ



-- MANIPULATIONS ---------------------------------------------------------------


merge : Span -> Span -> Span
merge a b =
    { start =
        if toComparable a.start < toComparable b.start then
            a.start

        else
            b.start
    , end =
        if toComparable a.end < toComparable b.end then
            b.end

        else
            a.end
    }



-- CONVERSIONS -----------------------------------------------------------------


toComparable : Pos -> ( Int, Int )
toComparable { line, column } =
    ( line, column )


toJson : Span -> String
toJson =
    encode >> Json.Encode.encode 4



-- JSON ------------------------------------------------------------------------


encode : Span -> Json.Encode.Value
encode { start, end } =
    Json.Encode.list Basics.identity
        [ Json.Encode.list Json.Encode.int
            [ start.line
            , start.column
            ]
        , Json.Encode.list Json.Encode.int
            [ end.line
            , end.column
            ]
        ]


decoder : Json.Decode.Decoder Span
decoder =
    Json.Decode.map2 Span
        (Json.Decode.index 0 <|
            Json.Decode.map2 Pos
                (Json.Decode.index 0 Json.Decode.int)
                (Json.Decode.index 1 Json.Decode.int)
        )
        (Json.Decode.index 1 <|
            Json.Decode.map2 Pos
                (Json.Decode.index 0 Json.Decode.int)
                (Json.Decode.index 1 Json.Decode.int)
        )



-- UTILS -----------------------------------------------------------------------
