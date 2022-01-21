module Ren.Data.Span exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Data.Tuple2
import Parser.Advanced as Parser exposing ((|=), Parser)



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Span =
    { start : Pos
    , end : Pos
    }


{-| -}
type alias Pos =
    { line : Int
    , column : Int
    }



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
fromTuples : ( Int, Int ) -> ( Int, Int ) -> Span
fromTuples start end =
    Span (Data.Tuple2.apply Pos start) (Data.Tuple2.apply Pos end)


{-| -}
merge : Span -> Span -> Span
merge a b =
    { start =
        if toComparable a.start < toComparable b.start then
            b.start

        else
            a.start
    , end =
        if toComparable a.end < toComparable b.end then
            b.end

        else
            a.end
    }


{-| -}
parser : (Span -> a -> b) -> Parser c e a -> Parser c e b
parser f p =
    Parser.succeed (\start a end -> f (fromTuples start end) a)
        |= Parser.getPosition
        |= p
        |= Parser.getPosition



-- CONVERSIONS -----------------------------------------------------------------


{-| -}
toComparable : Pos -> ( Int, Int )
toComparable { line, column } =
    ( line, column )
