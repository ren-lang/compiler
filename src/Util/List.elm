module Util.List exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------
-- QUERIES ---------------------------------------------------------------------


at : Int -> List a -> Maybe a
at idx items =
    let
        go i xs =
            case xs of
                [] ->
                    Nothing

                x :: rest ->
                    if i == idx then
                        Just x

                    else
                        go (i + 1) rest
    in
    go 0 items


indexOf : a -> List a -> Maybe Int
indexOf a items =
    let
        go i xs =
            case xs of
                [] ->
                    Nothing

                x :: rest ->
                    if x == a then
                        Just i

                    else
                        go (i + 1) rest
    in
    go 0 items



-- MANIPULATIONS ---------------------------------------------------------------
-- CONVERSIONS -----------------------------------------------------------------
-- UTILS -----------------------------------------------------------------------
