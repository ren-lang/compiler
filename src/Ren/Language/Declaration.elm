module Ren.Language.Declaration exposing (references, referencesNamespace, referencesQualified)

{-|

@docs references, referencesNamespace, referencesQualified

-}

-- IMPORTS ---------------------------------------------------------------------

import Ren.Language exposing (..)
import Ren.Language.Expression



-- QUERIES ---------------------------------------------------------------------


{-| -}
references : String -> Declaration -> Bool
references name declaration =
    case declaration of
        Function _ _ expr ->
            Ren.Language.Expression.references name expr

        Variable _ expr ->
            Ren.Language.Expression.references name expr

        Enum _ _ ->
            False


{-| -}
referencesNamespace : List String -> Declaration -> Bool
referencesNamespace namespace declaration =
    case declaration of
        Function _ _ expr ->
            Ren.Language.Expression.referencesNamespace namespace expr

        Variable _ expr ->
            Ren.Language.Expression.referencesNamespace namespace expr

        Enum _ _ ->
            False


{-| -}
referencesQualified : List String -> String -> Declaration -> Bool
referencesQualified namespace name declaration =
    case declaration of
        Function _ _ expr ->
            Ren.Language.Expression.referencesQualified namespace name expr

        Variable _ expr ->
            Ren.Language.Expression.referencesQualified namespace name expr

        Enum _ _ ->
            False
