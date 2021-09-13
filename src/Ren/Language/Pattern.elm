module Ren.Language.Pattern exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Ren.Language exposing (..)



-- QUERIES ---------------------------------------------------------------------


{-| -}
names : Pattern -> List String
names pattern =
    case pattern of
        ArrayDestructure elements ->
            List.concatMap names elements

        Name name ->
            [ name ]

        ObjectDestructure entries ->
            List.concatMap
                (\( k, p ) ->
                    Maybe.map names p
                        |> Maybe.withDefault [ k ]
                )
                entries

        Value _ ->
            []

        VariantDestructure tag patterns ->
            tag :: List.concatMap names patterns

        Typeof _ name ->
            [ name ]

        Wildcard _ ->
            []
