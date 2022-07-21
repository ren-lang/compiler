module Ren.Ast.Expr exposing
    ( Expr(..), Operator(..)
    , operators, operatorNames, operatorSymbols
    , raise
    , operatorFromName, operatorFromSymbol
    , operatorName
    , transform, desugar
    , lower
    , decoder, encode
    )

{-|


## Types

@docs Expr, Operator


## Constants

@docs operators, operatorNames, operatorSymbols


## Constructors

@docs raise, fromJson, fromJsonString
@docs operatorFromName, operatorFromSymbol


## Queries

@docs operatorName


## Manipulations

@docs transform, desugar


## Conversions

@docs lower, toJson


## JSON


## Utils

-}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Core
import Ren.Data.Metadata as Metadata
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

        -- When the last argument to a function call is a placeholder, we don't
        -- need to generate a lambda for it because functions are curried anyway!
        Call fun args ->
            case List.reverse args of
                [] ->
                    fun

                Placeholder :: [] ->
                    fun

                -- Just keep dropping placeholders while they're the last argument.
                -- If function application is all placeholders, eg something like:
                --
                --     f _ _ _
                --
                -- This will ultimately desugar to simply:
                --
                --     f
                --
                Placeholder :: _ ->
                    replacePlaceholders <| Call fun (List.drop 1 args)

                _ ->
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

        Literal (Ren.Ast.Core.LCon tag args) ->
            replaceWhen (names args) <|
                Literal (Ren.Ast.Core.LCon tag <| replaceMany 0 args)

        Literal (Ren.Ast.Core.LRec fields) ->
            let
                ( keys, vals ) =
                    List.unzip fields
            in
            replaceWhen (names vals) <|
                Literal (Ren.Ast.Core.LRec <| List.map2 Tuple.pair keys <| replaceMany 0 vals)

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
transform f expr =
    let
        transformCase ( pattern, guard, body ) =
            ( pattern, Maybe.map f guard, f body )
    in
    f <|
        case expr of
            Access rec key ->
                Access (transform f rec) key

            Binop op lhs rhs ->
                Binop op (transform f lhs) (transform f rhs)

            Call fun args ->
                Call (transform f fun) (List.map (transform f) args)

            If cond then_ else_ ->
                If (transform f cond) (transform f then_) (transform f else_)

            Lambda args body ->
                Lambda args (transform f body)

            Let name expr_ body ->
                Let name (transform f expr_) (transform f body)

            Literal (Ren.Ast.Core.LArr elements) ->
                Literal (Ren.Ast.Core.LArr <| List.map (transform f) elements)

            Literal (Ren.Ast.Core.LCon tag args) ->
                Literal (Ren.Ast.Core.LCon tag (List.map (transform f) args))

            Literal (Ren.Ast.Core.LRec fields) ->
                Literal (Ren.Ast.Core.LRec <| List.map (Tuple.mapSecond (transform f)) fields)

            Literal lit ->
                Literal lit

            Placeholder ->
                Placeholder

            Scoped scope name ->
                Scoped scope name

            Where expr_ cases ->
                Where (transform f expr_) (List.map transformCase cases)

            Var name ->
                Var name



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



-- JSON ------------------------------------------------------------------------


encode : Expr -> Json.Encode.Value
encode expr =
    let
        encodeCase ( pattern, guard, body ) =
            Json.Encode.list Basics.identity
                [ Metadata.encode "Case" {}
                , Ren.Ast.Core.encodePattern pattern
                , Maybe.withDefault Json.Encode.null <| Maybe.map encode guard
                , encode body
                ]
    in
    Json.Encode.list Basics.identity <|
        case expr of
            Access expr_ key ->
                [ Metadata.encode "Access" {}
                , encode expr_
                , Json.Encode.string key
                ]

            Binop op lhs rhs ->
                [ Metadata.encode "Binop" {}
                , Json.Encode.string <| operatorName op
                , encode lhs
                , encode rhs
                ]

            Call fun args ->
                [ Metadata.encode "Call" {}
                , encode fun
                , Json.Encode.list encode args
                ]

            If cond then_ else_ ->
                [ Metadata.encode "If" {}
                , encode cond
                , encode then_
                , encode else_
                ]

            Lambda args body ->
                [ Metadata.encode "Lambda" {}
                , Json.Encode.list Ren.Ast.Core.encodePattern args
                , encode body
                ]

            Let name expr_ body ->
                [ Metadata.encode "Let" {}
                , Ren.Ast.Core.encodePattern name
                , encode expr_
                , encode body
                ]

            Literal literal ->
                [ Metadata.encode "Literal" {}
                , Ren.Ast.Core.encodeLiteral encode literal
                ]

            Placeholder ->
                [ Metadata.encode "Placeholder" {}
                ]

            Scoped scope name ->
                [ Metadata.encode "Scoped" {}
                , Json.Encode.list Json.Encode.string scope
                , Json.Encode.string name
                ]

            Where expr_ cases ->
                [ Metadata.encode "Where" {}
                , encode expr_
                , Json.Encode.list encodeCase cases
                ]

            Var name ->
                [ Metadata.encode "Var" {}
                , Json.Encode.string name
                ]


decoder : Json.Decode.Decoder Expr
decoder =
    let
        lazyDecoder =
            Json.Decode.lazy <| \_ -> decoder

        operatorDecoder =
            Json.Decode.string
                |> Json.Decode.andThen
                    (\name ->
                        case operatorFromName name of
                            Just operator ->
                                Json.Decode.succeed operator

                            Nothing ->
                                Json.Decode.fail <| "Unknown operator: " ++ name
                    )

        caseDecoder =
            Json.Decode.map3 Triple.from
                (Json.Decode.index 1 <| Ren.Ast.Core.patternDecoder)
                (Json.Decode.index 2 <| Json.Decode.nullable <| lazyDecoder)
                (Json.Decode.index 3 <| lazyDecoder)
    in
    Metadata.decoder
        |> Json.Decode.andThen
            (\( key, _ ) ->
                case key of
                    "Access" ->
                        Json.Decode.map2 Access
                            (Json.Decode.index 1 <| lazyDecoder)
                            (Json.Decode.index 2 <| Json.Decode.string)

                    "Binop" ->
                        Json.Decode.map3 Binop
                            (Json.Decode.index 1 <| operatorDecoder)
                            (Json.Decode.index 2 <| lazyDecoder)
                            (Json.Decode.index 3 <| lazyDecoder)

                    "Call" ->
                        Json.Decode.map2 Call
                            (Json.Decode.index 1 <| lazyDecoder)
                            (Json.Decode.index 2 <| Json.Decode.list lazyDecoder)

                    "If" ->
                        Json.Decode.map3 If
                            (Json.Decode.index 1 <| lazyDecoder)
                            (Json.Decode.index 2 <| lazyDecoder)
                            (Json.Decode.index 3 <| lazyDecoder)

                    "Lambda" ->
                        Json.Decode.map2 Lambda
                            (Json.Decode.index 1 <| Json.Decode.list Ren.Ast.Core.patternDecoder)
                            (Json.Decode.index 2 <| lazyDecoder)

                    "Let" ->
                        Json.Decode.map3 Let
                            (Json.Decode.index 1 <| Ren.Ast.Core.patternDecoder)
                            (Json.Decode.index 2 <| lazyDecoder)
                            (Json.Decode.index 3 <| lazyDecoder)

                    "Literal" ->
                        Json.Decode.map Literal
                            (Json.Decode.index 1 <| Ren.Ast.Core.literalDecoder lazyDecoder)

                    "Placeholder" ->
                        Json.Decode.succeed Placeholder

                    "Scoped" ->
                        Json.Decode.map2 Scoped
                            (Json.Decode.index 1 <| Json.Decode.list Json.Decode.string)
                            (Json.Decode.index 2 <| Json.Decode.string)

                    "Where" ->
                        Json.Decode.map2 Where
                            (Json.Decode.index 1 <| lazyDecoder)
                            (Json.Decode.index 2 <| Json.Decode.list caseDecoder)

                    "Var" ->
                        Json.Decode.map Var
                            (Json.Decode.index 1 <| Json.Decode.string)

                    _ ->
                        Json.Decode.fail <| "Unknown expression type: " ++ key
            )



-- UTILS -----------------------------------------------------------------------
