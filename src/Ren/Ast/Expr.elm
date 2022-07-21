module Ren.Ast.Expr exposing
    ( Expr(..), Operator(..)
    , operators, operatorNames, operatorSymbols
    , raise
    , operatorFromName, operatorFromSymbol
    , operatorName
    , transform, desugar
    , lower, debug
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

@docs operatorName


## Manipulations

@docs transform, desugar


## Conversions

@docs lower, debug


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
    | Lambda (List Ren.Ast.Core.Pattern) Expr
    | Let Ren.Ast.Core.Pattern Expr Expr
    | Literal (Ren.Ast.Core.Literal Expr)
    | Placeholder
    | Scoped (List String) String
    | Where Expr (List ( Ren.Ast.Core.Pattern, Maybe Expr, Expr ))
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
                            Binop op lhs rhs

                        Nothing ->
                            Call expr [ lhs, rhs ]

                Ren.Ast.Core.EApp (Call (Var "<if>") [ cond, then_ ]) else_ ->
                    If cond then_ else_

                Ren.Ast.Core.EApp (Call fun args) arg ->
                    Call fun (args ++ [ arg ])

                Ren.Ast.Core.EApp fun arg ->
                    Call fun [ arg ]

                Ren.Ast.Core.ELam arg (Lambda args body) ->
                    Lambda (Ren.Ast.Core.PVar arg :: args) body

                Ren.Ast.Core.ELam arg body ->
                    Lambda [ Ren.Ast.Core.PVar arg ] body

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
                    Where expr cases
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
                Lambda (List.map Ren.Ast.Core.PVar args) body
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

        Literal (Ren.Ast.Core.LArr elements) ->
            replaceWhen (names elements) <|
                Literal (Ren.Ast.Core.LArr <| replaceMany 0 elements)

        Literal _ ->
            expr

        Placeholder ->
            expr

        Scoped _ _ ->
            expr

        Where expr_ cases ->
            replaceWhen (names [ expr_ ]) <|
                Where (replace 0 expr_) cases

        Var _ ->
            expr


desugar : Expr -> Expr
desugar expr =
    let
        transformations =
            [ replacePlaceholders
            ]

        applyN old =
            let
                new =
                    List.foldl (<|) old transformations
            in
            if old == new then
                old

            else
                applyN new
    in
    transform applyN expr


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
            let
                name i =
                    "$temp" ++ String.fromInt i

                names patterns =
                    List.map name <| List.range 0 <| List.length patterns - 1
            in
            case List.partitionWhile Ren.Ast.Core.isPVar args of
                ( [], patterns ) ->
                    Ren.Ast.Core.lam (names patterns) <|
                        Ren.Ast.Core.pat (Ren.Ast.Core.arr <| List.map Ren.Ast.Core.var <| names patterns)
                            [ ( Ren.Ast.Core.PLit (Ren.Ast.Core.LArr patterns)
                              , Nothing
                              , lower body
                              )
                            ]

                ( pvars, [] ) ->
                    Ren.Ast.Core.lam (List.concatMap Ren.Ast.Core.bindings pvars) <|
                        lower body

                ( pvars, patterns ) ->
                    Ren.Ast.Core.lam (List.concatMap Ren.Ast.Core.bindings pvars) <|
                        Ren.Ast.Core.lam (names patterns) <|
                            Ren.Ast.Core.pat (Ren.Ast.Core.arr <| List.map Ren.Ast.Core.var <| names patterns)
                                [ ( Ren.Ast.Core.PLit (Ren.Ast.Core.LArr patterns)
                                  , Nothing
                                  , lower body
                                  )
                                ]

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

        Placeholder ->
            Ren.Ast.Core.var "<placeholder>"

        Scoped scope name ->
            Ren.Ast.Core.var <|
                String.join "$" scope
                    ++ "$"
                    ++ name

        Where expr cases ->
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


debug : Expr -> String
debug expr =
    let
        indent n s =
            String.split "\n" s
                |> List.map ((++) (String.repeat n " "))
                |> String.join "\n"

        debugPattern pat =
            case pat of
                Ren.Ast.Core.PAny ->
                    "<pattern any>"

                Ren.Ast.Core.PLit (Ren.Ast.Core.LArr patterns) ->
                    String.join "\n" <|
                        List.concat
                            [ [ "<pattern array>" ]
                            , List.map (indent 4 << debugPattern) patterns
                            ]

                Ren.Ast.Core.PLit (Ren.Ast.Core.LCon tag args) ->
                    String.join "\n" <|
                        List.concat
                            [ [ "<pattern constructor> " ++ tag ]
                            , List.map (indent 4 << debugPattern) args
                            ]

                Ren.Ast.Core.PLit (Ren.Ast.Core.LNum n) ->
                    "<pattern number> " ++ String.fromFloat n

                Ren.Ast.Core.PLit (Ren.Ast.Core.LStr s) ->
                    "<pattern string> '" ++ s ++ "'"

                Ren.Ast.Core.PLit (Ren.Ast.Core.LRec fields) ->
                    String.join "\n" <|
                        List.concat
                            [ [ "<pattern record>" ]
                            , List.map (indent 4 << debugFieldPattern) fields
                            ]

                Ren.Ast.Core.PTyp typ pattern ->
                    String.join "\n" <|
                        [ "<pattern type> @" ++ typ
                        , debugPattern pattern
                        ]

                Ren.Ast.Core.PVar name ->
                    "<pattern var> " ++ name

        debugField ( key, val ) =
            key ++ " " ++ debug val

        debugFieldPattern ( key, pat ) =
            key ++ " " ++ debugPattern pat

        debugCase ( pattern, guard, body ) =
            String.join "\n" <|
                List.concat
                    [ [ "<case>"
                      , indent 4 <| debugPattern pattern
                      ]
                    , Maybe.withDefault [] <| Maybe.map (List.singleton << indent 4 << (++) "<guard> " << debug) guard
                    , [ indent 4 <| debug body ]
                    ]
    in
    String.join "\n" <|
        case expr of
            Access expr_ key ->
                [ "<access> " ++ key
                , indent 4 <| debug expr_
                ]

            Binop op lhs rhs ->
                [ "<binop> " ++ operatorName op
                , indent 4 <| debug lhs
                , indent 4 <| debug rhs
                ]

            Call fun args ->
                List.concat
                    [ [ "<call>"
                      , indent 4 <| debug fun
                      ]
                    , List.map (indent 4 << debug) args
                    ]

            If cond then_ else_ ->
                [ "<if>"
                , indent 4 <| debug cond
                , indent 4 <| debug then_
                , indent 4 <| debug else_
                ]

            Lambda args body ->
                List.concat
                    [ [ "<lambda>" ]
                    , List.map (indent 4 << debugPattern) args
                    , [ indent 4 <| debug body ]
                    ]

            Let name expr_ body ->
                [ "<let> " ++ debugPattern name
                , indent 4 <| debug expr_
                , indent 4 <| debug body
                ]

            Literal (Ren.Ast.Core.LArr elements) ->
                List.concat
                    [ [ "<literal array>" ]
                    , List.map (indent 4 << debug) elements
                    ]

            Literal (Ren.Ast.Core.LCon tag args) ->
                List.concat
                    [ [ "<literal constructor> #" ++ tag ]
                    , List.map (indent 4 << debug) args
                    ]

            Literal (Ren.Ast.Core.LNum n) ->
                [ "<literal number> " ++ String.fromFloat n ]

            Literal (Ren.Ast.Core.LRec fields) ->
                List.concat
                    [ [ "<literal record>" ]
                    , List.map (indent 4 << debugField) fields
                    ]

            Literal (Ren.Ast.Core.LStr s) ->
                [ "<literal string> '" ++ s ++ "'" ]

            Placeholder ->
                [ "<placeholder>" ]

            Scoped namespace name ->
                [ "<scoped> " ++ String.join "." namespace ++ "." ++ name ]

            Where expr_ cases ->
                List.concat
                    [ [ "<where>"
                      , indent 4 <| debug expr_
                      ]
                    , List.map (indent 4 << debugCase) cases
                    ]

            Var name ->
                [ "<var> " ++ name ]



-- UTILS -----------------------------------------------------------------------
