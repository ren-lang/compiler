module Ren.Language.Expression exposing
    ( insertBinding
    , coerceToNumber, coerceToInteger, coerceToString
    , references, referencesNamespace, referencesQualified
    )

{-|

@docs insertBinding
@docs coerceToNumber, coerceToInteger, coerceToString
@docs references, referencesNamespace, referencesQualified

-}

-- IMPORTS ---------------------------------------------------------------------

import Basics.Extra
import Dict
import Ren.Language exposing (..)



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
insertBinding : Declaration -> Expression -> Expression
insertBinding binding expression =
    case expression of
        Block bindings body ->
            Block (bindings ++ [ binding ]) body

        _ ->
            Block [ binding ] expression


coerceToNumber : Literal -> Maybe Float
coerceToNumber literal =
    case literal of
        Array _ ->
            Nothing

        Boolean True ->
            Just 1

        Boolean False ->
            Just 0

        Number n ->
            Just n

        Object _ ->
            Nothing

        String s ->
            String.toFloat s

        Template _ ->
            Nothing

        Undefined ->
            Just 0


coerceToInteger : Literal -> Maybe Int
coerceToInteger literal =
    case literal of
        Array _ ->
            Nothing

        Boolean True ->
            Just 1

        Boolean False ->
            Just 0

        Number n ->
            Basics.Extra.toInt n

        Object _ ->
            Nothing

        String s ->
            String.toInt s

        Template _ ->
            Nothing

        Undefined ->
            Just 0


coerceToString : Literal -> Maybe String
coerceToString literal =
    case literal of
        Array _ ->
            Nothing

        Boolean True ->
            Just "true"

        Boolean False ->
            Just "false"

        Number n ->
            Just (String.fromFloat n)

        Object _ ->
            Nothing

        String s ->
            Just s

        Template segments ->
            List.foldr
                (\segment s ->
                    case segment of
                        Expr (Literal lit) ->
                            Maybe.map2 (++)
                                (coerceToString lit)
                                s

                        Expr _ ->
                            Nothing

                        Text text ->
                            Maybe.map ((++) text) s
                )
                (Just "")
                segments

        Undefined ->
            Nothing



-- QUERIES ---------------------------------------------------------------------


{-| -}
references : String -> Expression -> Bool
references name expression =
    case expression of
        Access expr accessors ->
            references name expr
                || List.any (referencesInAccessor name) accessors

        Application expr args ->
            references name expr
                || List.any (references name) args

        Block bindings expr ->
            references name expr
                || List.any (referencesInBinding name) bindings

        Conditional condition true false ->
            references name condition
                || references name true
                || references name false

        Identifier (Local n) ->
            name == n

        Identifier (Constructor n) ->
            name == n

        Identifier _ ->
            False

        Infix _ lhs rhs ->
            references name lhs
                || references name rhs

        Lambda _ expr ->
            references name expr

        Literal (Array elements) ->
            List.any (references name) elements

        Literal (Object entries) ->
            Dict.values entries
                |> List.any (references name)

        Literal (Template segments) ->
            List.any (referencesInTemplate name) segments

        Literal _ ->
            False

        Match expr cases ->
            references name expr
                || List.any (referencesInCase name) cases

        SubExpression expr ->
            references name expr


referencesInAccessor : String -> Accessor -> Bool
referencesInAccessor name accessor =
    case accessor of
        Computed expr ->
            references name expr

        Fixed _ ->
            False


referencesInBinding : String -> Declaration -> Bool
referencesInBinding name binding =
    case binding of
        Function _ _ expr ->
            references name expr

        Variable _ expr ->
            references name expr

        Enum _ _ ->
            False


referencesInTemplate : String -> TemplateSegment -> Bool
referencesInTemplate name segment =
    case segment of
        Text _ ->
            False

        Expr expr ->
            references name expr


referencesInCase : String -> ( Pattern, Maybe Expression, Expression ) -> Bool
referencesInCase name ( _, guard, expr ) =
    references name expr
        || (Maybe.map (references name) guard
                |> Maybe.withDefault False
           )


{-| -}
referencesNamespace : List String -> Expression -> Bool
referencesNamespace namespace expression =
    case expression of
        Access expr accessors ->
            referencesNamespace namespace expr
                || List.any (referencesNamespaceInAccessor namespace) accessors

        Application expr args ->
            referencesNamespace namespace expr
                || List.any (referencesNamespace namespace) args

        Block bindings expr ->
            referencesNamespace namespace expr
                || List.any (referencesNamespaceInBinding namespace) bindings

        Conditional condition true false ->
            referencesNamespace namespace condition
                || referencesNamespace namespace true
                || referencesNamespace namespace false

        Identifier (Scoped ns _) ->
            namespace == ns

        Identifier _ ->
            False

        Infix _ lhs rhs ->
            referencesNamespace namespace lhs
                || referencesNamespace namespace rhs

        Lambda _ expr ->
            referencesNamespace namespace expr

        Literal (Array elements) ->
            List.any (referencesNamespace namespace) elements

        Literal (Object entries) ->
            Dict.values entries
                |> List.any (referencesNamespace namespace)

        Literal (Template segments) ->
            List.any (referencesNamespaceInTemplate namespace) segments

        Literal _ ->
            False

        Match expr cases ->
            referencesNamespace namespace expr
                || List.any (referencesNamespaceInCase namespace) cases

        SubExpression expr ->
            referencesNamespace namespace expr


referencesNamespaceInAccessor : List String -> Accessor -> Bool
referencesNamespaceInAccessor namespace accessor =
    case accessor of
        Computed expr ->
            referencesNamespace namespace expr

        Fixed _ ->
            False


referencesNamespaceInBinding : List String -> Declaration -> Bool
referencesNamespaceInBinding namespace binding =
    case binding of
        Function _ _ expr ->
            referencesNamespace namespace expr

        Variable _ expr ->
            referencesNamespace namespace expr

        Enum _ _ ->
            False


referencesNamespaceInTemplate : List String -> TemplateSegment -> Bool
referencesNamespaceInTemplate namespace segment =
    case segment of
        Text _ ->
            False

        Expr expr ->
            referencesNamespace namespace expr


referencesNamespaceInCase : List String -> ( Pattern, Maybe Expression, Expression ) -> Bool
referencesNamespaceInCase namespace ( _, guard, expr ) =
    referencesNamespace namespace expr
        || (Maybe.map (referencesNamespace namespace) guard
                |> Maybe.withDefault False
           )


{-| -}
referencesQualified : List String -> String -> Expression -> Bool
referencesQualified namespace name expression =
    case expression of
        Access expr accessors ->
            referencesQualified namespace name expr
                || List.any (referencesQualifiedInAccessor namespace name) accessors

        Application expr args ->
            referencesQualified namespace name expr
                || List.any (referencesQualified namespace name) args

        Block bindings expr ->
            referencesQualified namespace name expr
                || List.any (referencesQualifiedInBinding namespace name) bindings

        Conditional condition true false ->
            referencesQualified namespace name condition
                || referencesQualified namespace name true
                || referencesQualified namespace name false

        Identifier (Scoped ns n) ->
            namespace == ns && references name (Identifier n)

        Identifier _ ->
            False

        Infix _ lhs rhs ->
            referencesQualified namespace name lhs
                || referencesQualified namespace name rhs

        Lambda _ expr ->
            referencesQualified namespace name expr

        Literal (Array elements) ->
            List.any (referencesQualified namespace name) elements

        Literal (Object entries) ->
            Dict.values entries
                |> List.any (referencesQualified namespace name)

        Literal (Template segments) ->
            List.any (referencesQualifiedInTemplate namespace name) segments

        Literal _ ->
            False

        Match expr cases ->
            referencesQualified namespace name expr
                || List.any (referencesQualifiedInCase namespace name) cases

        SubExpression expr ->
            referencesQualified namespace name expr


referencesQualifiedInAccessor : List String -> String -> Accessor -> Bool
referencesQualifiedInAccessor namespace name accessor =
    case accessor of
        Computed expr ->
            referencesQualified namespace name expr

        Fixed _ ->
            False


referencesQualifiedInBinding : List String -> String -> Declaration -> Bool
referencesQualifiedInBinding namespace name binding =
    case binding of
        Function _ _ expr ->
            referencesQualified namespace name expr

        Variable _ expr ->
            referencesQualified namespace name expr

        Enum _ _ ->
            False


referencesQualifiedInTemplate : List String -> String -> TemplateSegment -> Bool
referencesQualifiedInTemplate namespace name segment =
    case segment of
        Text _ ->
            False

        Expr expr ->
            referencesQualified namespace name expr


referencesQualifiedInCase : List String -> String -> ( Pattern, Maybe Expression, Expression ) -> Bool
referencesQualifiedInCase namespace name ( _, guard, expr ) =
    referencesQualified namespace name expr
        || (Maybe.map (referencesQualified namespace name) guard
                |> Maybe.withDefault False
           )
