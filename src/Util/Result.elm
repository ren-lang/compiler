module Util.Result exposing (..)

-- CONVERSIONS -----------------------------------------------------------------


extract : (a -> b) -> (e -> b) -> Result e a -> b
extract f g result =
    case result of
        Ok a ->
            f a

        Err a ->
            g a


unwrap : Result a a -> a
unwrap result =
    case result of
        Ok a ->
            a

        Err a ->
            a
