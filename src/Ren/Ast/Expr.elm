module Ren.Ast.Expr exposing
    ( Expr(..), Operator(..)
    , operators, operatorNames, operatorSymbols
    , raise
    , operatorFromName, operatorFromSymbol
    , transform
    , lower
    )

{-|


## Types

@docs Expr, Operator


## Constants

@docs operators, operatorNames, operatorSymbols


## Constructors

@docs raise
@docs operatorFromName, operatorFromSymbol


## Queries


## Manipulations

@docs transform


## Conversions

@docs lower


## Utils

-}

-- IMPORTS ---------------------------------------------------------------------

import Ren.Ast.Core
import Util.List as List
import Util.Triple as Triple



-- TYPES -----------------------------------------------------------------------


{-| -}
type Expr
    = Access Expr String
    | Binop Operator Expr Expr
    | Call Expr (List Expr)
    | If Expr Expr Expr
    | Lambda (List String) Expr
    | Let Ren.Ast.Core.Pattern Expr Expr
    | Literal (Ren.Ast.Core.Literal Expr)
    | Placeholder
    | Scoped (List String) String
    | Switch Expr (List ( Ren.Ast.Core.Pattern, Maybe Expr, Expr ))
    | Var String


{-| -}
type Operator
    = Add --    +
    | And --    and
    | Concat -- ++
    | Cons --   ::
    | Div --    /
    | Eq --     ==
    | Gte --    >=
    | Gt --     >
    | Lte --    <=
    | Lt --     <
    | Mod --    %
    | Mul --    *
    | Neq --    !=
    | Or --     or
    | Pipe --   |>
    | Sub --    -



-- CONSTANTS -------------------------------------------------------------------


{-| Elm (frustratingly) only allows `Set`s of `comparable` values, which does not
include custom types. This is a list of every operator Ren has as the next-best
thing.

This is guaranteed to be in the same order as `operatorNames` and `operatorSymbols`

-}
operators : List Operator
operators =
    List.map Triple.first operatorsNamesAndSymbols


{-| -}
operatorNames : List String
operatorNames =
    List.map Triple.second operatorsNamesAndSymbols


{-| -}
operatorSymbols : List String
operatorSymbols =
    List.map Triple.third operatorsNamesAndSymbols


{-| -}
operatorsNamesAndSymbols : List ( Operator, String, String )
operatorsNamesAndSymbols =
    [ ( Add, "add", "+" )
    , ( And, "and", "and" )
    , ( Concat, "concat", "++" )
    , ( Cons, "cons", "::" )
    , ( Div, "div", "/" )
    , ( Eq, "eq", "==" )
    , ( Gte, "gte", ">=" )
    , ( Gt, "gt", ">" )
    , ( Lte, "lte", "<=" )
    , ( Lt, "lt", "<" )
    , ( Mod, "mod", "%" )
    , ( Mul, "mul", "*" )
    , ( Neq, "neq", "!=" )
    , ( Or, "or", "or" )
    , ( Pipe, "pipe", "|>" )
    , ( Sub, "sub", "-" )
    ]



-- CONSTRUCTORS ----------------------------------------------------------------


{-| Take an expression from our core 𝝺-calculus representation and raise it up
to the higher-level `Expr` type. This will take some of the magical variables
used and expand them into more useful forms. For example, binary operators are
represented in the core as:

    Expr
        (EApp
            (Expr
                (EApp
                    (Expr
                        (EApp
                            (Expr (EVar "<binop>"))
                            (Expr (ELit (LStr "add")))
                        )
                    )
                    (Expr (EVar "x"))
                )
            )
            (Expr (EVar "y"))
        )

-}
raise : Ren.Ast.Core.Expr -> Expr
raise =
    let
        go exprF =
            case exprF of
                Ren.Ast.Core.EApp (Call (Var "<access>") [ Literal (Ren.Ast.Core.LStr key) ]) expr ->
                    Access expr key

                Ren.Ast.Core.EApp (Call (Var "<binop>") [ (Literal (Ren.Ast.Core.LStr s)) as expr, lhs ]) rhs ->
                    case operatorFromName s of
                        Just op ->
                            Binop op expr lhs

                        Nothing ->
                            Call expr [ lhs, rhs ]

                Ren.Ast.Core.EApp (Call (Var "<if>") [ cond, then_ ]) else_ ->
                    If cond then_ else_

                Ren.Ast.Core.EApp (Call fun args) arg ->
                    Call fun (args ++ [ arg ])

                Ren.Ast.Core.EApp fun arg ->
                    Call fun [ arg ]

                Ren.Ast.Core.ELam arg (Lambda args body) ->
                    Lambda (arg :: args) body

                Ren.Ast.Core.ELam arg body ->
                    Lambda [ arg ] body

                Ren.Ast.Core.ELet name expr body ->
                    Let (Ren.Ast.Core.PVar name) expr body

                Ren.Ast.Core.ELit lit ->
                    Literal lit

                Ren.Ast.Core.EVar "<placeholder>" ->
                    Placeholder

                Ren.Ast.Core.EVar var ->
                    case List.reverse <| String.split "$" var of
                        [] ->
                            Placeholder

                        name :: [] ->
                            Var name

                        name :: scope ->
                            Scoped (List.reverse scope) name

                Ren.Ast.Core.EPat expr cases ->
                    Switch expr cases
    in
    Ren.Ast.Core.fold go


{-| -}
operatorFromName : String -> Maybe Operator
operatorFromName name =
    List.indexOf name operatorNames
        |> Maybe.andThen (\i -> List.at i operators)


{-| -}
operatorFromSymbol : String -> Maybe Operator
operatorFromSymbol symbol =
    List.indexOf symbol operatorSymbols
        |> Maybe.andThen (\i -> List.at i operators)



-- QUERIES ---------------------------------------------------------------------


{-| -}
operatorName : Operator -> String
operatorName op =
    List.find (Triple.first >> (==) op) operatorsNamesAndSymbols
        |> Maybe.map Triple.second
        -- This default should never be hit, we hardcode the list of operators
        -- and their names/symbols.
        |> Maybe.withDefault ""


{-| -}
operatorSymbol : Operator -> String
operatorSymbol op =
    List.find (Triple.first >> (==) op) operatorsNamesAndSymbols
        |> Maybe.map Triple.third
        -- This default should never be hit, we hardcode the list of operators
        -- and their names/symbols.
        |> Maybe.withDefault ""



-- MANIPULATIONS ---------------------------------------------------------------


replacePlaceholders : Expr -> Expr
replacePlaceholders expr =
    let
        -- Creates a valid JavaScript variable name from a placholder.
        name i =
            "$temp" ++ String.fromInt i

        -- Given a list of expressions, creates a list of valid JavaScript variable
        -- names for each placeholder. Rather than incrementing sequentially,
        -- the variable's name is based on the index of the placeholder:
        --
        --     [ Placeholder, Var "x", Placeholder ]
        --
        -- becomes:
        --
        --     [ "$temp0", "$temp2" ]
        --
        names exprs =
            exprs
                |> List.indexedMap
                    (\i e ->
                        if e == Placeholder then
                            Just <| name i

                        else
                            Nothing
                    )
                |> List.filterMap Basics.identity

        -- Replace a placeholder a variable. If the given expression is not an
        -- expression this is a no-op.
        replace i e =
            if e == Placeholder then
                Var <| name i

            else
                e

        -- Replace all placeholders in an expression. An offset is used when
        -- generating the variable names:
        --
        --     replaceWhen (fun :: args) <|
        --         Lambda (names (fun :: args)) <|
        --             -- Note that the names generated for `args` is offset by
        --             -- 1 because we may have already replaced a placeholder in
        --             -- `fun`.
        --             Call (replace 0 fun) (replaceMany 1 args)
        --
        replaceMany offset exprs =
            exprs
                |> List.indexedMap (\i e -> replace (i + offset) e)

        -- If the list of replacement names is not empty, create a lambda using
        -- those names as the arguments and the supplied body, otherwise return
        -- the initial expression unchanged.
        replaceWhen args body =
            if List.isEmpty args then
                expr

            else
                Lambda args body
    in
    case expr of
        Access rec key ->
            replaceWhen (names [ rec ]) <|
                Access (replace 0 rec) key

        Binop op lhs rhs ->
            replaceWhen (names [ lhs, rhs ]) <|
                Binop op (replace 0 lhs) (replace 1 rhs)

        Call fun args ->
            replaceWhen (names (fun :: args)) <|
                Call (replace 0 fun) (replaceMany 1 args)

        If cond then_ else_ ->
            replaceWhen (names [ cond, then_, else_ ]) <|
                If (replace 0 cond)
                    (replace 1 then_)
                    (replace 2 else_)

        Lambda _ _ ->
            expr

        Let _ _ _ ->
            expr

        Literal _ ->
            expr

        Placeholder ->
            expr

        Scoped _ _ ->
            expr

        Switch expr_ cases ->
            replaceWhen (names [ expr_ ]) <|
                Switch (replace 0 expr_) cases

        Var _ ->
            expr


transform : (Expr -> Expr) -> Expr -> Expr
transform f =
    let
        applyTransformation =
            Ren.Ast.Core.map lower >> Ren.Ast.Core.Expr >> raise >> f
    in
    lower >> Ren.Ast.Core.fold applyTransformation



-- CONVERSIONS -----------------------------------------------------------------


{-| Lower a Ren expression to a core representation based on the 𝝺-calculus. Some
constructs are represented using special internal variables, for example:

    Access (Var "foo") "bar"

is represented as:

    Expr
        (EApp
            (Expr
                (EApp
                    (Expr (EVar "<access>"))
                    (Expr (ELit (LStr "bar")))
                )
            )
            (Expr (EVar "foo"))
        )

This is the reverse of `raise`. In fact, you if you call `raise (lower expr)` you
should get back exactly the same expression.

-}
lower : Expr -> Ren.Ast.Core.Expr
lower expr_ =
    case expr_ of
        Access expr key ->
            Ren.Ast.Core.app (Ren.Ast.Core.var "<access>")
                [ Ren.Ast.Core.str key
                , lower expr
                ]

        Binop op lhs rhs ->
            Ren.Ast.Core.app (Ren.Ast.Core.var "<binop>")
                [ Ren.Ast.Core.str <| operatorName op
                , lower lhs
                , lower rhs
                ]

        Call fun args ->
            Ren.Ast.Core.app (lower fun) <|
                List.map lower args

        If cond then_ else_ ->
            Ren.Ast.Core.app (Ren.Ast.Core.var "<if>")
                [ lower cond
                , lower then_
                , lower else_
                ]

        Lambda args body ->
            Ren.Ast.Core.lam args <|
                lower body

        Let (Ren.Ast.Core.PVar name) expr body ->
            Ren.Ast.Core.let_ [ ( name, lower expr ) ] <|
                lower body

        Let pattern expr body ->
            Ren.Ast.Core.pat (lower expr)
                [ ( pattern
                  , Nothing
                  , lower body
                  )
                ]

        Literal (Ren.Ast.Core.LArr elements) ->
            Ren.Ast.Core.arr <|
                List.map lower elements

        Literal (Ren.Ast.Core.LBool b) ->
            Ren.Ast.Core.bool b

        Literal (Ren.Ast.Core.LCon tag args) ->
            Ren.Ast.Core.con tag <|
                List.map lower args

        Literal (Ren.Ast.Core.LNum n) ->
            Ren.Ast.Core.num n

        Literal (Ren.Ast.Core.LRec fields) ->
            Ren.Ast.Core.rec <|
                List.map (Tuple.mapSecond lower) fields

        Literal (Ren.Ast.Core.LStr s) ->
            Ren.Ast.Core.str s

        Literal Ren.Ast.Core.LUnit ->
            Ren.Ast.Core.unit

        Placeholder ->
            Ren.Ast.Core.unit

        Scoped scope name ->
            Ren.Ast.Core.var <|
                String.join "$" scope
                    ++ "$"
                    ++ name

        Switch expr cases ->
            Ren.Ast.Core.pat (lower expr) <|
                List.map
                    (\( pattern, guard, body ) ->
                        ( pattern
                        , Maybe.map lower guard
                        , lower body
                        )
                    )
                    cases

        Var name ->
            Ren.Ast.Core.var name



-- UTILS -----------------------------------------------------------------------
