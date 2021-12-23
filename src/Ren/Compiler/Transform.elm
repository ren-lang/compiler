module Ren.Compiler.Transform exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Ren.AST.Expr as Expr
    exposing
        ( Expr(..)
        , ExprF(..)
        , Identifier(..)
        , Pattern(..)
        )



-- RUNNING TRANSFORMATIONS -----------------------------------------------------


{-| -}
runAll : Expr meta -> Expr meta
runAll =
    run [ placeholders ]


{-| -}
run : List (Transformation meta) -> Expr meta -> Expr meta
run transformations =
    Expr.cata <|
        \meta expression ->
            transformations
                |> List.foldl (\f e -> f meta e) expression
                -- After all the transformations have been applied we wrap the
                -- expression back up in our `Expr` wrapper with the original
                -- metadata attached.
                |> Expr meta



-- TYPES -----------------------------------------------------------------------


{-| A transformation is any function that takes both the metadata and expression
node in the AST and returns a new AST node.
-}
type alias Transformation meta =
    meta -> ExprF (Expr meta) -> ExprF (Expr meta)



-- TRANSFORMATIONS -------------------------------------------------------------


{-| Ren allows certain expressions to include placeholder bindings: `_`. These
bindings get transformed into lambdas and allow us a simple way to achieve
positional piping, partial operator application, and more.
-}
placeholders : Transformation meta
placeholders meta expr =
    let
        -- Generate a bunch of id numbers up front so we can create variable names
        -- like `$0`... This is easier than threading some state to increment the
        -- number as we go, even if it is a bit sloppier.
        --
        -- I'd seriously question anyone's sanity if they're using more than 10
        -- placeholders at any given "level" of the AST. Because it's impossible
        -- to refer to placeholders it doesn't matter if we shadow an previously
        -- generated name, so 10 ids should be enough.
        ids =
            List.range 0 10

        isPlaceholder (Expr _ e) =
            case e of
                Identifier (Placeholder _) ->
                    True

                _ ->
                    False

        -- Takes a list of expressions and gives back a list of names to be used
        -- as the arguments for a lambda.
        args exprs =
            List.filterMap Basics.identity <|
                List.map2
                    (\id (Expr _ e) ->
                        case e of
                            Identifier (Placeholder _) ->
                                Just <| Name ("$" ++ String.fromInt id)

                            _ ->
                                Nothing
                    )
                    ids
                    exprs

        -- Replace a placeholder identifier with a local identifier like `$1`
        -- based on some id number.
        replace id (Expr m e) =
            case e of
                Identifier (Placeholder _) ->
                    Expr m (Identifier (Local <| "$" ++ String.fromInt id))

                _ ->
                    Expr m e
    in
    case expr of
        Access ((Expr _ (Identifier (Placeholder _))) as e) accessors ->
            Lambda [ Name "$0" ] <|
                Expr meta (Access (replace 0 e) accessors)

        Application e es ->
            if List.any isPlaceholder (e :: es) then
                Lambda (args (e :: es)) <|
                    Expr meta (Application (replace 0 e) (List.map2 replace (List.drop 1 ids) es))

            else
                expr

        Conditional c t f ->
            if List.any isPlaceholder [ c, t, f ] then
                Lambda (args [ c, t, f ]) <|
                    Expr meta (Conditional (replace 0 c) (replace 1 t) (replace 2 f))

            else
                expr

        Infix op lhs rhs ->
            if List.any isPlaceholder [ lhs, rhs ] then
                Lambda (args [ lhs, rhs ]) <|
                    Expr meta (Infix op (replace 0 lhs) (replace 1 rhs))

            else
                expr

        Match ((Expr _ (Identifier (Placeholder _))) as e) cases ->
            Lambda [ Name "$0" ] <|
                Expr meta (Match (replace 0 e) cases)

        _ ->
            expr
