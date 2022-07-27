module Ren.Ast.Expr exposing
    ( Expr(..)
    , fold, foldWith, transform, desugar
    , decoder, encode
    )

{-|


## Types

@docs Expr


## Constants

@docs operators, operatorNames, operatorSymbols


## Constructors

@docs fromJson, fromJsonString
@docs operatorFromName, operatorFromSymbol


## Queries

@docs operatorName


## Manipulations

@docs fold, foldWith, transform, desugar


## Conversions

@docs toJson


## JSON


## Utils

-}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Expr.Lit as Lit exposing (Lit)
import Ren.Ast.Expr.Meta as Meta exposing (Meta)
import Ren.Ast.Expr.Op as Operator exposing (Operator)
import Ren.Ast.Expr.Pat as Pattern exposing (Pattern)
import Ren.Ast.Type as Type exposing (Type)
import Util.Json as Json
import Util.List as List
import Util.Triple as Triple



-- TYPES -----------------------------------------------------------------------


{-| -}
type Expr
    = Access Meta Expr String
    | Annotated Meta Expr Type
    | Binop Meta Operator Expr Expr
    | Call Meta Expr (List Expr)
    | If Meta Expr Expr Expr
    | Lambda Meta (List Pattern) Expr
    | Let Meta Pattern Expr Expr
    | Lit Meta (Lit Expr)
    | Placeholder Meta
    | Scoped Meta (List String) String
    | Where Meta Expr (List ( Pattern, Maybe Expr, Expr ))
    | Var Meta String



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------
-- QUERIES ---------------------------------------------------------------------


meta : Expr -> Meta
meta expr =
    case expr of
        Access metadata _ _ ->
            metadata

        Annotated metadata _ _ ->
            metadata

        Binop metadata _ _ _ ->
            metadata

        Call metadata _ _ ->
            metadata

        If metadata _ _ _ ->
            metadata

        Lambda metadata _ _ ->
            metadata

        Let metadata _ _ _ ->
            metadata

        Lit metadata _ ->
            metadata

        Placeholder metadata ->
            metadata

        Scoped metadata _ _ ->
            metadata

        Where metadata _ _ ->
            metadata

        Var metadata _ ->
            metadata



-- MANIPULATIONS ---------------------------------------------------------------


foldWith :
    { access : Expr -> a -> String -> a
    , annotated : Expr -> a -> Type -> a
    , binop : Expr -> Operator -> a -> a -> a
    , call : Expr -> a -> List a -> a
    , if_ : Expr -> a -> a -> a -> a
    , lambda : Expr -> List Pattern -> a -> a
    , let_ : Expr -> Pattern -> a -> a -> a
    , literal : Expr -> Lit a -> a
    , placeholder : Expr -> a
    , scoped : Expr -> List String -> String -> a
    , where_ : Expr -> a -> List ( Pattern, Maybe a, a ) -> a
    , var : Expr -> String -> a
    }
    -> Expr
    -> a
foldWith f expr =
    let
        foldCase ( pattern, guard, body ) =
            ( pattern, Maybe.map (foldWith f) guard, foldWith f body )
    in
    case expr of
        Access _ rec key ->
            f.access expr (foldWith f rec) key

        Annotated _ expr_ type_ ->
            f.annotated expr (foldWith f expr_) type_

        Binop _ op lhs rhs ->
            f.binop expr op (foldWith f lhs) (foldWith f rhs)

        Call _ fun args ->
            f.call expr (foldWith f fun) (List.map (foldWith f) args)

        If _ cond then_ else_ ->
            f.if_ expr (foldWith f cond) (foldWith f then_) (foldWith f else_)

        Lambda _ args body ->
            f.lambda expr args (foldWith f body)

        Let _ name expr_ body ->
            f.let_ expr name (foldWith f expr_) (foldWith f body)

        Lit _ (Lit.Array elements) ->
            f.literal expr (Lit.Array <| List.map (foldWith f) elements)

        Lit _ (Lit.Enum tag args) ->
            f.literal expr (Lit.Enum tag (List.map (foldWith f) args))

        Lit _ (Lit.Number n) ->
            f.literal expr (Lit.Number n)

        Lit _ (Lit.Record fields) ->
            f.literal expr (Lit.Record <| List.map (Tuple.mapSecond (foldWith f)) fields)

        Lit _ (Lit.String s) ->
            f.literal expr (Lit.String s)

        Placeholder _ ->
            f.placeholder expr

        Scoped _ scope name ->
            f.scoped expr scope name

        Where _ expr_ cases ->
            f.where_ expr (foldWith f expr_) (List.map foldCase cases)

        Var _ name ->
            f.var expr name


fold :
    { access : a -> String -> a
    , annotated : a -> Type -> a
    , binop : Operator -> a -> a -> a
    , call : a -> List a -> a
    , if_ : a -> a -> a -> a
    , lambda : List Pattern -> a -> a
    , let_ : Pattern -> a -> a -> a
    , literal : Lit a -> a
    , placeholder : a
    , scoped : List String -> String -> a
    , where_ : a -> List ( Pattern, Maybe a, a ) -> a
    , var : String -> a
    }
    -> Expr
    -> a
fold f expr =
    foldWith
        { access = Basics.always f.access
        , annotated = Basics.always f.annotated
        , binop = Basics.always f.binop
        , call = Basics.always f.call
        , if_ = Basics.always f.if_
        , lambda = Basics.always f.lambda
        , let_ = Basics.always f.let_
        , literal = Basics.always f.literal
        , placeholder = Basics.always f.placeholder
        , scoped = Basics.always f.scoped
        , where_ = Basics.always f.where_
        , var = Basics.always f.var
        }
        expr


transform : (Expr -> Expr) -> Expr -> Expr
transform f =
    foldWith
        { access = \e expr key -> f <| Access (meta e) expr key
        , annotated = \e expr type_ -> f <| Annotated (meta e) expr type_
        , binop = \e op lhs rhs -> f <| Binop (meta e) op lhs rhs
        , call = \e fun args -> f <| Call (meta e) fun args
        , if_ = \e cond then_ else_ -> f <| If (meta e) cond then_ else_
        , lambda = \e args body -> f <| Lambda (meta e) args body
        , let_ = \e name expr body -> f <| Let (meta e) name expr body
        , literal = \e lit -> f <| Lit (meta e) lit
        , placeholder = \e -> f <| Placeholder (meta e)
        , scoped = \e scope name -> f <| Scoped (meta e) scope name
        , where_ = \e expr cases -> f <| Where (meta e) expr cases
        , var = \e name -> f <| Var (meta e) name
        }


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
                        case e of
                            Placeholder _ ->
                                Just <| name i

                            _ ->
                                Nothing
                    )
                |> List.filterMap Basics.identity

        -- Replace a placeholder a variable. If the given expression is not an
        -- expression this is a no-op.
        replace i e =
            case e of
                Placeholder metadata ->
                    Var metadata <| name i

                _ ->
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
        replaceWhen metadata args body =
            if List.isEmpty args then
                expr

            else
                Lambda metadata (List.map Pattern.Var args) body
    in
    case expr of
        Access metadata rec key ->
            replaceWhen metadata (names [ rec ]) <|
                Access metadata (replace 0 rec) key

        Annotated metadata expr_ type_ ->
            replaceWhen metadata (names [ expr_ ]) <|
                Annotated metadata (replace 0 expr_) type_

        Binop metadata op lhs rhs ->
            replaceWhen metadata (names [ lhs, rhs ]) <|
                Binop metadata op (replace 0 lhs) (replace 1 rhs)

        -- When the last argument to a function call is a placeholder, we don't
        -- need to generate a lambda for it because functions are curried anyway!
        Call metadata fun args ->
            case List.reverse args of
                [] ->
                    fun

                (Placeholder _) :: [] ->
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
                (Placeholder _) :: _ ->
                    replacePlaceholders <| Call metadata fun (List.drop 1 args)

                _ ->
                    replaceWhen metadata (names (fun :: args)) <|
                        Call metadata (replace 0 fun) (replaceMany 1 args)

        If metadata cond then_ else_ ->
            replaceWhen metadata (names [ cond, then_, else_ ]) <|
                If metadata
                    (replace 0 cond)
                    (replace 1 then_)
                    (replace 2 else_)

        Lambda _ _ _ ->
            expr

        Let _ _ _ _ ->
            expr

        Lit metadata (Lit.Array elements) ->
            replaceWhen metadata (names elements) <|
                Lit metadata (Lit.Array <| replaceMany 0 elements)

        Lit metadata (Lit.Enum tag args) ->
            replaceWhen metadata (names args) <|
                Lit metadata (Lit.Enum tag <| replaceMany 0 args)

        Lit metadata (Lit.Record fields) ->
            let
                ( keys, vals ) =
                    List.unzip fields
            in
            replaceWhen metadata (names vals) <|
                Lit metadata (Lit.Record <| List.map2 Tuple.pair keys <| replaceMany 0 vals)

        Lit _ _ ->
            expr

        Placeholder _ ->
            expr

        Scoped _ _ _ ->
            expr

        Where metadata expr_ cases ->
            replaceWhen metadata (names [ expr_ ]) <|
                Where metadata (replace 0 expr_) cases

        Var _ _ ->
            expr



-- CONVERSIONS -----------------------------------------------------------------
-- JSON ------------------------------------------------------------------------


encode : Expr -> Json.Encode.Value
encode expr =
    let
        encodeCase ( pattern, guard, body ) =
            Json.taggedEncoder "Case"
                []
                [ Pattern.encode pattern
                , Maybe.withDefault Json.Encode.null <| Maybe.map encode guard
                , encode body
                ]
    in
    case expr of
        Access metadata expr_ key ->
            Json.taggedEncoder "Access"
                (Meta.encode metadata)
                [ encode expr_
                , Json.Encode.string key
                ]

        Annotated metadata expr_ type_ ->
            Json.taggedEncoder "Annotated"
                (Meta.encode metadata)
                [ encode expr_
                , Type.encode type_
                ]

        Binop metadata op lhs rhs ->
            Json.taggedEncoder "Binop"
                (Meta.encode metadata)
                [ Operator.encode op
                , encode lhs
                , encode rhs
                ]

        Call metadata fun args ->
            Json.taggedEncoder "Call"
                (Meta.encode metadata)
                [ encode fun
                , Json.Encode.list encode args
                ]

        If metadata cond then_ else_ ->
            Json.taggedEncoder "If"
                (Meta.encode metadata)
                [ encode cond
                , encode then_
                , encode else_
                ]

        Lambda metadata args body ->
            Json.taggedEncoder "Lambda"
                (Meta.encode metadata)
                [ Json.Encode.list Pattern.encode args
                , encode body
                ]

        Let metadata name expr_ body ->
            Json.taggedEncoder "Let"
                (Meta.encode metadata)
                [ Pattern.encode name
                , encode expr_
                , encode body
                ]

        Lit metadata literal ->
            Json.taggedEncoder "Lit"
                (Meta.encode metadata)
                [ Lit.encode encode literal
                ]

        Placeholder metadata ->
            Json.taggedEncoder "Placeholder"
                (Meta.encode metadata)
                []

        Scoped metadata scope name ->
            Json.taggedEncoder "Scoped"
                (Meta.encode metadata)
                [ Json.Encode.list Json.Encode.string scope
                , Json.Encode.string name
                ]

        Where metadata expr_ cases ->
            Json.taggedEncoder "Where"
                (Meta.encode metadata)
                [ encode expr_
                , Json.Encode.list encodeCase cases
                ]

        Var metadata name ->
            Json.taggedEncoder "Var"
                (Meta.encode metadata)
                [ Json.Encode.string name
                ]


decoder : Json.Decode.Decoder Expr
decoder =
    let
        lazyDecoder =
            Json.Decode.lazy <| \_ -> decoder

        caseDecoder =
            Json.Decode.map3 Triple.from
                (Json.Decode.index 1 <| Pattern.decoder)
                (Json.Decode.index 2 <| Json.Decode.nullable <| lazyDecoder)
                (Json.Decode.index 3 <| lazyDecoder)
    in
    Json.taggedDecoder
        (\key ->
            case key of
                "Access" ->
                    Json.Decode.map3 Access
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| Json.Decode.string)

                "Annotated" ->
                    Json.Decode.map3 Annotated
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| Type.decoder)

                "Binop" ->
                    Json.Decode.map4 Binop
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| Operator.decoder)
                        (Json.Decode.index 2 <| lazyDecoder)
                        (Json.Decode.index 3 <| lazyDecoder)

                "Call" ->
                    Json.Decode.map3 Call
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| Json.Decode.list lazyDecoder)

                "If" ->
                    Json.Decode.map4 If
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| lazyDecoder)
                        (Json.Decode.index 3 <| lazyDecoder)

                "Lambda" ->
                    Json.Decode.map3 Lambda
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| Json.Decode.list Pattern.decoder)
                        (Json.Decode.index 2 <| lazyDecoder)

                "Let" ->
                    Json.Decode.map4 Let
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| Pattern.decoder)
                        (Json.Decode.index 2 <| lazyDecoder)
                        (Json.Decode.index 3 <| lazyDecoder)

                "Lit" ->
                    Json.Decode.map2 Lit
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| Lit.decoder lazyDecoder)

                "Placeholder" ->
                    Json.Decode.map Placeholder
                        (Json.Decode.index 0 <| Meta.decoder)

                "Scoped" ->
                    Json.Decode.map3 Scoped
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| Json.Decode.list Json.Decode.string)
                        (Json.Decode.index 2 <| Json.Decode.string)

                "Where" ->
                    Json.Decode.map3 Where
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| lazyDecoder)
                        (Json.Decode.index 2 <| Json.Decode.list caseDecoder)

                "Var" ->
                    Json.Decode.map2 Var
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| Json.Decode.string)

                _ ->
                    Json.Decode.fail <| "Unknown expression type: " ++ key
        )



-- UTILS -----------------------------------------------------------------------
