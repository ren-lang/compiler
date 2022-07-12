module Util.List exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Set



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


find : (a -> Bool) -> List a -> Maybe a
find f items =
    List.filter f items
        |> List.head



-- MANIPULATIONS ---------------------------------------------------------------


uniques : List comparable -> List comparable
uniques =
    Set.fromList >> Set.toList


updateBy : (a -> Bool) -> (a -> a) -> List a -> List a
updateBy p f items =
    List.map
        (\x ->
            if p x then
                f x

            else
                x
        )
        items


partitionWhile : (a -> Bool) -> List a -> ( List a, List a )
partitionWhile p items =
    let
        go xs =
            case xs of
                [] ->
                    ( [], [] )

                x :: rest ->
                    if p x then
                        Tuple.mapFirst ((::) x) (go rest)

                    else
                        ( [], x :: rest )
    in
    go items



-- CONVERSIONS -----------------------------------------------------------------


uncons : List a -> Maybe ( a, List a )
uncons items =
    case items of
        [] ->
            Nothing

        x :: rest ->
            Just ( x, rest )



-- UTILS -----------------------------------------------------------------------
