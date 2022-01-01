module Ren.Compiler.Optimise exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Ren.AST.Expr as Expr
    exposing
        ( Expr(..)
        , ExprF(..)
        , Literal(..)
        , Operator(..)
        )
import Ren.AST.Module as Module



-- RUNNING OPTIMISATIONS -------------------------------------------------------


{-| -}
run : List (Optimisation meta) -> Module.Declaration meta -> Module.Declaration meta
run optimisations declaration =
    let
        -- This continuously applies optimisations until they no longer change
        -- the AST. It's possible to blow the stack up if one optimisation undoes
        -- the work of another, so be careful!
        apply meta expr =
            let
                result =
                    List.foldl (\f e -> f meta e) expr optimisations
            in
            if result == expr then
                expr

            else
                apply meta result
    in
    { declaration
        | expr =
            Expr.cata
                (\meta expression ->
                    apply meta expression
                        -- After all the optimisations have been applied we wrap the
                        -- expression back up in our `Expr` wrapper with the original
                        -- metadata attached.
                        |> Expr meta
                )
                declaration.expr
    }



-- TYPES -----------------------------------------------------------------------


{-| An optimisation is any function that takes both the metadata and expression
node in the AST and returns a new AST node; usually a simplified version.

This is actually the same type as a `Transformation`, but I find it useful to
disambiguate between functions that desugar or modify the AST and ones that
attempt to simplify or eliminate nodes.

-}
type alias Optimisation meta =
    meta -> ExprF (Expr meta) -> ExprF (Expr meta)



-- OPTIMISATIONS ---------------------------------------------------------------


{-| This optimisation attempts to evaluate operators at compile-time. This mostly
just optimises away operations on literals, like computing the expression `1 + 1`.
-}
operators : Optimisation meta
operators _ expr =
    case expr of
        Infix Pipe lhs rhs ->
            Application rhs [ lhs ]

        Infix Add (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (+) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Number >> Literal)
                |> Maybe.withDefault expr

        Infix Sub (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (-) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Number >> Literal)
                |> Maybe.withDefault expr

        Infix Mul (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (*) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Number >> Literal)
                |> Maybe.withDefault expr

        Infix Div (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (/) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Number >> Literal)
                |> Maybe.withDefault expr

        Infix Pow (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (^) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Number >> Literal)
                |> Maybe.withDefault expr

        -- Elm's `Basics.modBy` function only works on integers, but numbers in
        -- Ren are always floats. We could attempt to see if both operands are
        -- integers and perform the optimisation in that case, but I think it's
        -- easier to just let this happen at run time.
        Infix Mod _ _ ->
            expr

        Infix Lt (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (<) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Boolean >> Literal)
                |> Maybe.withDefault expr

        Infix Lte (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (<=) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Boolean >> Literal)
                |> Maybe.withDefault expr

        Infix Gt (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (>) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Boolean >> Literal)
                |> Maybe.withDefault expr

        Infix Gte (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (>=) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Boolean >> Literal)
                |> Maybe.withDefault expr

        Infix And (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (&&) (Expr.coerceToBoolean lhs) (Expr.coerceToBoolean rhs)
                |> Maybe.map (Boolean >> Literal)
                |> Maybe.withDefault expr

        Infix Or (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (||) (Expr.coerceToBoolean lhs) (Expr.coerceToBoolean rhs)
                |> Maybe.map (Boolean >> Literal)
                |> Maybe.withDefault expr

        Infix Cons lhs (Expr _ (Literal (Array elements))) ->
            Literal (Array <| lhs :: elements)

        Infix Join (Expr _ (Literal (Array xs))) (Expr _ (Literal (Array ys))) ->
            Literal (Array <| xs ++ ys)

        _ ->
            expr
