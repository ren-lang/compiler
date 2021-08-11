module Basics.Extra exposing (..)


toInt : Float -> Maybe Int
toInt x =
    let
        y =
            Basics.floor x
    in
    if Basics.toFloat y == x then
        Just y

    else
        Nothing
