module Ren.Data.Span exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode



-- TYPES -----------------------------------------------------------------------


type alias Span =
    { start : Pos
    , end : Pos
    }


type alias Pos =
    { line : Int
    , column : Int
    }



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


from : ( Int, Int ) -> ( Int, Int ) -> Span
from ( x, y ) ( a, b ) =
    { start = Pos x y, end = Pos a b }



-- QUERIES ---------------------------------------------------------------------
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
