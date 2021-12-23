module Ren.Compiler.Optimise exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Ren.AST.Expr as Expr exposing (Expr(..), ExprF(..))


{-| -}
runAll : Expr meta -> Expr meta
runAll =
    run [ operators ]


run : List (Optimisation meta) -> Expr meta -> Expr meta
run optimisations =
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
    Expr.cata <|
        \meta expression ->
            apply meta expression
                -- After all the transformations have been applied we wrap the
                -- expression back up in our `Expr` wrapper with the original
                -- metadata attached.
                |> Expr meta



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


{-| This optimisation attempts to evaluate operators at compile-time. The usual
term for this is _constant folding_.
-}
operators : Optimisation meta
operators _ expr =
    case expr of
        Infix Expr.Add (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (+) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Expr.Number >> Expr.Literal)
                |> Maybe.withDefault expr

        Infix Expr.Sub (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (-) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Expr.Number >> Expr.Literal)
                |> Maybe.withDefault expr

        Infix Expr.Mul (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (*) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Expr.Number >> Expr.Literal)
                |> Maybe.withDefault expr

        Infix Expr.Div (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (/) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Expr.Number >> Expr.Literal)
                |> Maybe.withDefault expr

        Infix Expr.Pow (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (^) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Expr.Number >> Expr.Literal)
                |> Maybe.withDefault expr

        -- Elm's `Basics.modBy` function only works on integers, but numbers in
        -- Ren are always floats. We could attempt to see if both operands are
        -- integers and perform the optimisation in that case, but I think it's
        -- easier to just let this happen at run time.
        Infix Expr.Mod _ _ ->
            expr

        Infix Expr.Lt (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (<) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Expr.Boolean >> Expr.Literal)
                |> Maybe.withDefault expr

        Infix Expr.Lte (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (<=) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Expr.Boolean >> Expr.Literal)
                |> Maybe.withDefault expr

        Infix Expr.Gt (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (>) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Expr.Boolean >> Expr.Literal)
                |> Maybe.withDefault expr

        Infix Expr.Gte (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (>=) (Expr.coerceToNumber lhs) (Expr.coerceToNumber rhs)
                |> Maybe.map (Expr.Boolean >> Expr.Literal)
                |> Maybe.withDefault expr

        Infix Expr.And (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (&&) (Expr.coerceToBoolean lhs) (Expr.coerceToBoolean rhs)
                |> Maybe.map (Expr.Boolean >> Expr.Literal)
                |> Maybe.withDefault expr

        Infix Expr.Or (Expr _ lhs) (Expr _ rhs) ->
            Maybe.map2 (||) (Expr.coerceToBoolean lhs) (Expr.coerceToBoolean rhs)
                |> Maybe.map (Expr.Boolean >> Expr.Literal)
                |> Maybe.withDefault expr

        Infix Expr.Cons lhs (Expr _ (Expr.Literal (Expr.Array elements))) ->
            Expr.Literal (Expr.Array <| lhs :: elements)

        Infix Expr.Join (Expr _ (Expr.Literal (Expr.Array xs))) (Expr _ (Expr.Literal (Expr.Array ys))) ->
            Expr.Literal (Expr.Array <| xs ++ ys)

        _ ->
            expr
