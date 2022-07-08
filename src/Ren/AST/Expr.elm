module Ren.Ast.Expr exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Ren.Ast.Core
import Util.List as List



-- TYPES -----------------------------------------------------------------------


type Expr
    = Access Expr String
    | Binop Expr Operator Expr
    | Call Expr (List Expr)
    | If Expr Expr Expr
    | Lambda (List String) Expr
    | Let String Expr Expr
    | Literal (Ren.Ast.Core.Literal Expr)
    | Placeholder
    | Scoped (List String) String
    | Switch Expr (List ( Ren.Ast.Core.Pattern, Maybe Expr, Expr ))
    | Var String


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
    | Sub --    -



-- CONSTANTS -------------------------------------------------------------------


operators : List Operator
operators =
    [ Add, And, Concat, Cons, Div, Eq, Gte, Gt, Lte, Lt, Mod, Mul, Neq, Or, Sub ]


operatorNames : List String
operatorNames =
    [ "add", "and", "concat", "cons", "div", "eq", "gte", "gt", "lte", "lt", "mod", "mul", "neq", "or", "sub" ]


operatorSymbols : List String
operatorSymbols =
    [ "+", "and", "++", "::", "/", "==", ">=", ">", "<=", "<", "%", "*", "!=", "or", "-" ]



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
                Ren.Ast.Core.EAbs arg (Lambda args body) ->
                    Lambda (arg :: args) body

                Ren.Ast.Core.EAbs arg body ->
                    Lambda [ arg ] body

                Ren.Ast.Core.EApp (Call (Var "<access>") [ Literal (Ren.Ast.Core.LStr key) ]) expr ->
                    Access expr key

                Ren.Ast.Core.EApp (Call (Var "<binop>") [ (Literal (Ren.Ast.Core.LStr s)) as expr, lhs ]) rhs ->
                    case operatorFromName s of
                        Just op ->
                            Binop expr op lhs

                        Nothing ->
                            Call expr [ lhs, rhs ]

                Ren.Ast.Core.EApp (Call (Var "<if>") [ cond, then_ ]) else_ ->
                    If cond then_ else_

                Ren.Ast.Core.EApp (Call fun args) arg ->
                    Call fun (args ++ [ arg ])

                Ren.Ast.Core.EApp fun arg ->
                    Call fun [ arg ]

                Ren.Ast.Core.ELet pattern expr body ->
                    Let pattern expr body

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


operatorFromName : String -> Maybe Operator
operatorFromName name =
    List.indexOf name operatorNames
        |> Maybe.andThen (\i -> List.at i operators)



-- QUERIES ---------------------------------------------------------------------


operatorName : Operator -> String
operatorName op =
    List.indexOf op operators
        |> Maybe.andThen (\i -> List.at i operatorNames)
        -- This default should never be hit, we hardcode the list of operators
        -- and operatorNames.
        |> Maybe.withDefault ""



-- MANIPULATIONS ---------------------------------------------------------------
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

        Binop lhs op rhs ->
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
            Ren.Ast.Core.abs args <|
                lower body

        Let pattern expr body ->
            Ren.Ast.Core.let_ [ ( pattern, lower expr ) ] <|
                lower body

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
            Ren.Ast.Core.var "<placeholder>"

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
