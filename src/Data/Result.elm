module Data.Result exposing (..)


or : Result e a -> Result e a -> Result e a
or b a =
    case a of
        Ok _ ->
            a

        Err _ ->
            b


either : Result e a -> Result e a -> Result e a
either a b =
    case a of
        Ok _ ->
            a

        Err _ ->
            b
