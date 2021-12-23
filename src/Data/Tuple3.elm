module Data.Tuple3 exposing (..)


mapFirst : (a -> d) -> ( a, b, c ) -> ( d, b, c )
mapFirst f ( a, b, c ) =
    ( f a, b, c )


mapSecond : (b -> d) -> ( a, b, c ) -> ( a, d, c )
mapSecond f ( a, b, c ) =
    ( a, f b, c )


mapThird : (c -> d) -> ( a, b, c ) -> ( a, b, d )
mapThird f ( a, b, c ) =
    ( a, b, f c )


mapAll : (a -> d) -> (b -> e) -> (c -> f) -> ( a, b, c ) -> ( d, e, f )
mapAll f g h ( a, b, c ) =
    ( f a, g b, h c )
