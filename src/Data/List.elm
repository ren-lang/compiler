module Data.List exposing (..)


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 Tuple.pair
