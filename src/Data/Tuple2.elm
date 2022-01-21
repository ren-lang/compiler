module Data.Tuple2 exposing (..)


from : a -> ( a, a )
from a =
    ( a, a )


fromBy : (a -> b) -> (a -> c) -> a -> ( b, c )
fromBy f g a =
    ( f a, g a )


apply : (a -> b -> c) -> ( a, b ) -> c
apply f ( a, b ) =
    f a b


asList : (List a -> b) -> ( a, a ) -> b
asList f ( a, b ) =
    f [ a, b ]
