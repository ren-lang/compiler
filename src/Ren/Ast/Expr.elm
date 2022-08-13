module Ren.Ast.Expr exposing
    ( Expr(..), Meta
    , meta
    , references, shadows
    , transform, transformMeta, desugar
    , fold, foldWith, foldWithMeta
    , toJson
    , ParseContext, parser
    , encode, decoder
    )

{-|


## Types

@docs Expr, Meta


## Constants


## Constructors


## Queries

@docs meta
@docs references, shadows


## Manipulations

@docs transform, transformMeta, desugar
@docs fold, foldWith, foldWithMeta


## Conversions

@docs toJson


## Parsing

@docs ParseContext, parser


## JSON

@docs encode, decoder


## Utils

-}

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Ren.Ast.Expr.Lit as Lit exposing (Lit)
import Ren.Ast.Expr.Meta as Meta
import Ren.Ast.Expr.Op as Op exposing (Op)
import Ren.Ast.Expr.Pat as Pat exposing (Pat)
import Ren.Ast.Type as Type exposing (Type)
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Control.Parser.Pratt as Pratt
import Ren.Data.Span as Span
import Ren.Data.Token as Token
import Set exposing (Set)
import Util.Dict as Dict
import Util.Json as Json
import Util.List as List
import Util.Triple as Triple



-- TYPES -----------------------------------------------------------------------


{-| -}
type Expr
    = Access Meta Expr String
    | Annotated Meta Expr Type
    | Binop Meta Op Expr Expr
    | Call Meta Expr (List Expr)
    | If Meta Expr Expr Expr
    | Lambda Meta (List Pat) Expr
    | Let Meta Pat Expr Expr
    | Lit Meta (Lit Expr)
    | Placeholder Meta
    | Scoped Meta (List String) String
    | Where Meta Expr (List ( Pat, Maybe Expr, Expr ))
    | Var Meta String


type alias Meta =
    Meta.Meta


type alias ParseContext =
    { inArgPosition : Bool
    }



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------
-- QUERIES ---------------------------------------------------------------------


{-| -}
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


references : Expr -> Dict ( List String, String ) (List Meta)
references =
    let
        removeCaseBindings ( pat, guardDict, bodyDict ) =
            Maybe.withDefault Dict.empty guardDict
                |> Dict.combine (++) bodyDict
                |> removePatBindings [ pat ]

        removePatBindings pats dict =
            List.map Pat.bindings pats
                |> List.foldl Set.union Set.empty
                |> Set.map (Tuple.pair [])
                |> Set.foldl (\k d -> Dict.remove k d) dict
    in
    foldWithMeta
        { access = \_ accessDict _ -> accessDict
        , annotated = \_ annotatedDict _ -> annotatedDict
        , binop = \_ _ lhsDict rhsDict -> Dict.combine (++) lhsDict rhsDict
        , call = \_ funDict argDicts -> List.foldl (Dict.combine (++)) funDict argDicts
        , if_ = \_ condDict thenDict elseDict -> List.foldl (Dict.combine (++)) condDict [ thenDict, elseDict ]
        , lambda = \_ pats bodyDict -> removePatBindings pats bodyDict
        , let_ = \_ pat exprDict bodyDict -> Dict.combine (++) exprDict (removePatBindings [ pat ] bodyDict)
        , literal =
            \_ lit ->
                case lit of
                    Lit.Array array ->
                        List.foldl (Dict.combine (++)) Dict.empty array

                    Lit.Enum _ args ->
                        List.foldl (Dict.combine (++)) Dict.empty args

                    Lit.Record record ->
                        List.foldl (Tuple.second >> Dict.combine (++)) Dict.empty record

                    _ ->
                        Dict.empty
        , placeholder = \_ -> Dict.empty
        , scoped = \metadata scope name -> Dict.singleton ( scope, name ) [ metadata ]
        , where_ = \_ exprDict caseDicts -> List.foldl (Dict.combine (++) << removeCaseBindings) exprDict caseDicts
        , var = \metadata name -> Dict.singleton ( [], name ) [ metadata ]
        }


shadows : Set String -> Expr -> Dict String (List Meta)
shadows names =
    let
        insertCaseBindings metadata ( pat, guardDict, bodyDict ) dict =
            Maybe.withDefault Dict.empty guardDict
                |> Dict.combine (++) bodyDict
                |> Dict.combine (++) dict
                |> insertPatBindings metadata [ pat ]

        insertPatBindings metadata pats dict =
            List.map Pat.bindings pats
                |> List.foldl Set.union Set.empty
                |> Set.filter (\name -> Set.member name names)
                |> Set.foldl (\name d -> Dict.upsert name [ metadata ] ((::) metadata) d) dict
    in
    foldWithMeta
        { access = \_ accessDict _ -> accessDict
        , annotated = \_ annotatedDict _ -> annotatedDict
        , binop = \_ _ lhsDict rhsDict -> Dict.combine (++) lhsDict rhsDict
        , call = \_ funDict argDicts -> List.foldl (Dict.combine (++)) funDict argDicts
        , if_ = \_ condDict thenDict elseDict -> List.foldl (Dict.combine (++)) condDict [ thenDict, elseDict ]
        , lambda = \metadata pats bodyDict -> insertPatBindings metadata pats bodyDict
        , let_ = \metadata pat exprDict bodyDict -> insertPatBindings metadata [ pat ] <| Dict.combine (++) exprDict bodyDict
        , literal =
            \_ lit ->
                case lit of
                    Lit.Array array ->
                        List.foldl (Dict.combine (++)) Dict.empty array

                    Lit.Enum _ args ->
                        List.foldl (Dict.combine (++)) Dict.empty args

                    Lit.Record record ->
                        List.foldl (Tuple.second >> Dict.combine (++)) Dict.empty record

                    _ ->
                        Dict.empty
        , placeholder = \_ -> Dict.empty
        , scoped = \_ _ _ -> Dict.empty
        , where_ = \metadata exprDict cases -> List.foldl (insertCaseBindings metadata) exprDict cases
        , var = \_ _ -> Dict.empty
        }



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
transformMeta : (Meta -> Meta) -> Expr -> Expr
transformMeta f expr =
    case expr of
        Access metadata expr_ name ->
            Access (f metadata) expr_ name

        Annotated metadata expr_ type_ ->
            Annotated (f metadata) expr_ type_

        Binop metadata op lhs rhs ->
            Binop (f metadata) op lhs rhs

        Call metadata fun args ->
            Call (f metadata) fun args

        If metadata cond then_ else_ ->
            If (f metadata) cond then_ else_

        Lambda metadata args body ->
            Lambda (f metadata) args body

        Let metadata pat expr_ body ->
            Let (f metadata) pat expr_ body

        Lit metadata lit ->
            Lit (f metadata) lit

        Placeholder metadata ->
            Placeholder (f metadata)

        Scoped metadata expr_ name ->
            Scoped (f metadata) expr_ name

        Where metadata expr_ cases ->
            Where (f metadata) expr_ cases

        Var metadata name ->
            Var (f metadata) name


{-| Reduce an expression to a single value, walking the tree bottom-up and
collecting the results as we go. You can think of this like Pat matching on
each `Expr` variant, but any recursive `Expr` parameters are replaced with the
result of calling `fold` instead.
-}
fold :
    { access : a -> String -> a
    , annotated : a -> Type -> a
    , binop : Op -> a -> a -> a
    , call : a -> List a -> a
    , if_ : a -> a -> a -> a
    , lambda : List Pat -> a -> a
    , let_ : Pat -> a -> a -> a
    , literal : Lit a -> a
    , placeholder : a
    , scoped : List String -> String -> a
    , where_ : a -> List ( Pat, Maybe a, a ) -> a
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


{-| Works the same as [`fold]`(#fold) but also gives access to the current
expression being folded.
-}
foldWith :
    { access : Expr -> a -> String -> a
    , annotated : Expr -> a -> Type -> a
    , binop : Expr -> Op -> a -> a -> a
    , call : Expr -> a -> List a -> a
    , if_ : Expr -> a -> a -> a -> a
    , lambda : Expr -> List Pat -> a -> a
    , let_ : Expr -> Pat -> a -> a -> a
    , literal : Expr -> Lit a -> a
    , placeholder : Expr -> a
    , scoped : Expr -> List String -> String -> a
    , where_ : Expr -> a -> List ( Pat, Maybe a, a ) -> a
    , var : Expr -> String -> a
    }
    -> Expr
    -> a
foldWith f expr =
    let
        foldCase ( pat, guard, body ) =
            ( pat, Maybe.map (foldWith f) guard, foldWith f body )
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


{-| -}
foldWithMeta :
    { access : Meta -> a -> String -> a
    , annotated : Meta -> a -> Type -> a
    , binop : Meta -> Op -> a -> a -> a
    , call : Meta -> a -> List a -> a
    , if_ : Meta -> a -> a -> a -> a
    , lambda : Meta -> List Pat -> a -> a
    , let_ : Meta -> Pat -> a -> a -> a
    , literal : Meta -> Lit a -> a
    , placeholder : Meta -> a
    , scoped : Meta -> List String -> String -> a
    , where_ : Meta -> a -> List ( Pat, Maybe a, a ) -> a
    , var : Meta -> String -> a
    }
    -> Expr
    -> a
foldWithMeta f expr =
    foldWith
        { access = meta >> f.access
        , annotated = meta >> f.annotated
        , binop = meta >> f.binop
        , call = meta >> f.call
        , if_ = meta >> f.if_
        , lambda = meta >> f.lambda
        , let_ = meta >> f.let_
        , literal = meta >> f.literal
        , placeholder = meta >> f.placeholder
        , scoped = meta >> f.scoped
        , where_ = meta >> f.where_
        , var = meta >> f.var
        }
        expr


{-| -}
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


{-| -}
desugar : Expr -> Expr
desugar expr =
    let
        transformations =
            [ replacePlaceholders
            , uncallEnums
            ]

        --
        applyMany old =
            let
                new =
                    List.foldl (<|) old transformations
            in
            if old == new then
                old

            else
                applyMany new
    in
    transform applyMany expr


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
                Lambda metadata (List.map Pat.Var args) body
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


uncallEnums : Expr -> Expr
uncallEnums expr =
    case expr of
        Call callMeta (Lit enumMeta (Lit.Enum tag args)) moreArgs ->
            Lit { enumMeta | span = Span.merge callMeta.span enumMeta.span, tipe = Type.Hole } <|
                Lit.Enum tag (args ++ moreArgs)

        _ ->
            expr



-- CONVERSIONS -----------------------------------------------------------------


toJson : Expr -> String
toJson =
    encode >> Json.Encode.encode 4



-- PARSING ---------------------------------------------------------------------


{-| -}
parser : ParseContext -> Parser () String Expr
parser context =
    Pratt.expression
        { oneOf =
            List.concat
                [ callableParsers context
                , if context.inArgPosition then
                    []

                  else
                    [ ifParser, lambdaParser, letParser, whereParser ]
                ]
        , andThenOneOf =
            if context.inArgPosition then
                []

            else
                operatorParsers
        , spaces = Parser.succeed ()
        }


operatorParsers : List (Pratt.Operator () String Expr)
operatorParsers =
    let
        operatorParser ( assoc, precedence, sym ) =
            assoc precedence
                (Parser.operator "" sym)
                (\lhs rhs -> Binop (Meta.new (mergeSpan lhs rhs)) sym lhs rhs)

        mergeSpan lhs rhs =
            Span.merge (meta lhs).span (meta rhs).span
    in
    List.map operatorParser
        [ -- Left associativity
          ( Pratt.infixLeft, 2, Op.Pipe )
        , ( Pratt.infixLeft, 4, Op.Eq )
        , ( Pratt.infixLeft, 4, Op.Gt )
        , ( Pratt.infixLeft, 4, Op.Gte )
        , ( Pratt.infixLeft, 4, Op.Lt )
        , ( Pratt.infixLeft, 4, Op.Lte )
        , ( Pratt.infixLeft, 4, Op.Neq )
        , ( Pratt.infixLeft, 6, Op.Add )
        , ( Pratt.infixLeft, 6, Op.Sub )
        , ( Pratt.infixLeft, 7, Op.Div )
        , ( Pratt.infixLeft, 7, Op.Mul )

        -- Right associativity
        , ( Pratt.infixRight, 1, Op.Seq )
        , ( Pratt.infixRight, 2, Op.Or )
        , ( Pratt.infixRight, 3, Op.And )
        , ( Pratt.infixRight, 5, Op.Concat )
        , ( Pratt.infixRight, 5, Op.Cons )
        ]


callableParsers : ParseContext -> List (Pratt.Parsers () String Expr -> Parser () String Expr)
callableParsers context =
    List.map
        (Parser.andThen accessParser
            >> Parser.andThen (callParser context)
            >> Pratt.literal
        )
        [ parenthesisedParser
        , placeholderParser
        , scopedParser
        , varParser
        , Lit.parser { inArgPosition = context.inArgPosition }
            { fromString = Var Meta.default
            , itemParser = Parser.lazy <| \_ -> parser { inArgPosition = False }
            , wrapParser = parenthesisedParser
            }
            |> Parser.map (Lit Meta.default)
            |> withParseMetadata
        ]


parenthesisedParser : Parser () String Expr
parenthesisedParser =
    Parser.succeed Basics.identity
        |> Parser.drop (Parser.symbol "" <| Token.Paren Token.Left)
        |> Parser.keep (Parser.lazy <| \_ -> parser { inArgPosition = False })
        |> Parser.drop (Parser.symbol "" <| Token.Paren Token.Right)
        |> withParseMetadata


accessParser : Expr -> Parser () String Expr
accessParser expr_ =
    let
        chainAccessors key keys =
            List.foldl (\k e -> Access Meta.default e k) (Access Meta.default expr_ key) keys
    in
    Parser.oneOf
        [ Parser.succeed chainAccessors
            |> Parser.drop (Parser.symbol "" <| Token.Period)
            |> Parser.keep (Parser.identifier "" Token.Lower)
            |> Parser.keep
                (Parser.many
                    (\ks ->
                        [ Parser.succeed (\k -> k :: ks)
                            |> Parser.drop (Parser.symbol "" Token.Period)
                            |> Parser.keep (Parser.identifier "" Token.Lower)
                            |> Parser.map Parser.Continue
                        , Parser.succeed ()
                            |> Parser.map (\_ -> List.reverse ks)
                            |> Parser.map Parser.Break
                        ]
                    )
                )
        , Parser.succeed expr_
        ]
        |> withParseMetadata


callParser : ParseContext -> Expr -> Parser () String Expr
callParser { inArgPosition } expr =
    let
        argParser =
            Parser.lazy <| \_ -> parser { inArgPosition = True }
    in
    if inArgPosition then
        Parser.succeed expr

    else
        Parser.oneOf
            [ Parser.succeed (\arg args -> Call Meta.default expr (arg :: args))
                |> Parser.keep argParser
                |> Parser.keep
                    (Parser.many
                        (\args ->
                            [ Parser.succeed (\arg -> arg :: args)
                                |> Parser.keep argParser
                                |> Parser.map Parser.Continue
                            , Parser.succeed ()
                                |> Parser.map (\_ -> List.reverse args)
                                |> Parser.map Parser.Break
                            ]
                        )
                    )
            , Parser.succeed expr
            ]
            |> withParseMetadata


ifParser : Pratt.Parsers () String Expr -> Parser () String Expr
ifParser parsers =
    Parser.succeed (If Meta.default)
        |> Parser.drop (Parser.keyword "" Token.If)
        |> Parser.keep (Pratt.subExpression 0 parsers)
        |> Parser.drop (Parser.keyword "" Token.Then)
        |> Parser.keep (Pratt.subExpression 0 parsers)
        |> Parser.drop (Parser.keyword "" Token.Else)
        |> Parser.keep (Pratt.subExpression 0 parsers)
        |> withParseMetadata


lambdaParser : Pratt.Parsers () String Expr -> Parser () String Expr
lambdaParser parsers =
    Parser.succeed (\pat rest body -> Lambda Meta.default (pat :: rest) body)
        |> Parser.drop (Parser.keyword "" Token.Fun)
        |> Parser.keep (Pat.parser { inArgPosition = True, includeSpread = False })
        |> Parser.keep
            (Parser.loop []
                (\pats ->
                    Parser.oneOf
                        [ Parser.succeed (\pat -> pat :: pats)
                            |> Parser.keep (Pat.parser { inArgPosition = True, includeSpread = False })
                            |> Parser.map Parser.Continue
                        , Parser.succeed ()
                            |> Parser.drop (Parser.symbol "" Token.FatArrow)
                            |> Parser.map (\_ -> List.reverse pats)
                            |> Parser.map Parser.Break
                        ]
                )
            )
        |> Parser.keep (Pratt.subExpression 0 parsers)
        |> withParseMetadata


letParser : Pratt.Parsers () String Expr -> Parser () String Expr
letParser parsers =
    Parser.succeed (Let Meta.default)
        |> Parser.drop (Parser.keyword "" Token.Let)
        |> Parser.keep (Pat.parser { inArgPosition = True, includeSpread = False })
        |> Parser.drop (Parser.symbol "" Token.Equal)
        |> Parser.keep (Pratt.subExpression 1 parsers)
        |> Parser.drop (Parser.operator "" Op.Seq)
        |> Parser.keep (Pratt.subExpression 0 parsers)
        |> Parser.backtrackable
        |> withParseMetadata


placeholderParser : Parser () String Expr
placeholderParser =
    Parser.succeed (Placeholder Meta.default)
        |> Parser.drop (Parser.symbol "" Token.Underscore)
        |> withParseMetadata


scopedParser : Parser () String Expr
scopedParser =
    Parser.succeed (\n ns name -> Scoped Meta.default (n :: ns) name)
        |> Parser.keep (Parser.identifier "" Token.Upper)
        |> Parser.keep
            (Parser.loop []
                (\ns ->
                    Parser.succeed Basics.identity
                        |> Parser.drop (Parser.symbol "" Token.Period)
                        |> Parser.keep
                            (Parser.oneOf
                                [ Parser.succeed (\n -> n :: ns)
                                    |> Parser.keep (Parser.identifier "" Token.Upper)
                                    |> Parser.map Parser.Continue
                                , Parser.succeed ()
                                    |> Parser.map (\_ -> List.reverse ns)
                                    |> Parser.map Parser.Break
                                ]
                            )
                )
            )
        |> Parser.keep (Parser.identifier "" Token.Lower)
        |> withParseMetadata


varParser : Parser () String Expr
varParser =
    Parser.succeed (Var Meta.default)
        |> Parser.keep (Parser.identifier "" Token.Lower)
        |> withParseMetadata


whereParser : Pratt.Parsers () String Expr -> Parser () String Expr
whereParser parsers =
    let
        caseParser =
            Parser.succeed Triple.from
                |> Parser.drop (Parser.keyword "" Token.Is)
                |> Parser.keep (Pat.parser { inArgPosition = False, includeSpread = False })
                |> Parser.keep
                    (Parser.oneOf
                        [ Parser.succeed Just
                            |> Parser.drop (Parser.keyword "" Token.If)
                            |> Parser.keep (Pratt.subExpression 0 parsers)
                        , Parser.succeed Nothing
                        ]
                    )
                |> Parser.drop (Parser.symbol "" Token.FatArrow)
                |> Parser.keep (Pratt.subExpression 0 parsers)
    in
    Parser.succeed (\expr c cs -> Where Meta.default expr (c :: cs))
        |> Parser.drop (Parser.keyword "" Token.Where)
        |> Parser.keep (Pratt.subExpression 0 parsers)
        |> Parser.keep caseParser
        |> Parser.keep
            (Parser.loop []
                (\cs ->
                    Parser.oneOf
                        [ Parser.succeed (\c -> c :: cs)
                            |> Parser.keep caseParser
                            |> Parser.map Parser.Continue
                        , Parser.succeed ()
                            |> Parser.map (\_ -> List.reverse cs)
                            |> Parser.map Parser.Break
                        ]
                )
            )
        |> withParseMetadata



-- JSON ------------------------------------------------------------------------


{-| -}
encode : Expr -> Json.Encode.Value
encode expr =
    let
        encodeCase ( pat, guard, body ) =
            Json.taggedEncoder "Case"
                []
                [ Pat.encode pat
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
                [ Op.encode op
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
                [ Json.Encode.list Pat.encode args
                , encode body
                ]

        Let metadata name expr_ body ->
            Json.taggedEncoder "Let"
                (Meta.encode metadata)
                [ Pat.encode name
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


{-| -}
decoder : Json.Decode.Decoder Expr
decoder =
    let
        lazyDecoder =
            Json.Decode.lazy <| \_ -> decoder

        caseDecoder =
            Json.Decode.map3 Triple.from
                (Json.Decode.index 1 <| Pat.decoder)
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
                        (Json.Decode.index 1 <| Op.decoder)
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
                        (Json.Decode.index 1 <| Json.Decode.list Pat.decoder)
                        (Json.Decode.index 2 <| lazyDecoder)

                "Let" ->
                    Json.Decode.map4 Let
                        (Json.Decode.index 0 <| Meta.decoder)
                        (Json.Decode.index 1 <| Pat.decoder)
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


withParseMetadata : Parser () String Expr -> Parser () String Expr
withParseMetadata =
    let
        addComments comments expr =
            transformMeta (\metadata -> List.foldr Meta.addComment metadata comments) expr

        addSpan span expr =
            transformMeta (\metadata -> Meta.setSpan span metadata) expr
    in
    Parser.withComments "" addComments >> Parser.withSpan addSpan
