module Ren.Ast.Core exposing
    ( Expr(..)
    , ExprF(..), Pattern(..), Literal(..)
    , app, lam, let_, var, pat
    , arr, con, num, rec, str
    , isPAny, isPLit, isPTyp, isPVar
    , bindings
    , map, fold, foldWith, unfold
    , encodePattern, encodeLiteral
    , patternDecoder, literalDecoder
    )

{-| This module reflects the core functional representation of Ren code. It is
not much more than the simple 𝝺-calculus extended with pattern matching and some
different literals. It exists to make dealing with certain compiler passes, such
as type checking, much simpler

There exists [`lower`](./Expr#lower) and [`raise`](./Expr#raise) functions to
convert to and from the higher-level representation define in [`Expr.elm`](./Expr).
The composition of `lower >> raise` is idempotent, meaning repeated applications
will not change the result.


## Types

@docs Expr
@docs ExprF, Pattern, Literal


## Constructors

@docs app, lam, let_, var, pat
@docs arr, con, num, rec, str


## Queries

@docs isPAny, isPLit, isPTyp, isPVar
@docs bindings


## Manipulations

@docs map, fold, foldWith, unfold


## Conversions


## Utils


## JSON

@docs encodePattern, encodeLiteral
@docs patternDecoder, literalDecoder

-}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Data.Metadata as Metadata



-- TYPES -----------------------------------------------------------------------


{-| You can think of our [`ExprF`](#ExprF) as a kind of template that describes
a single "layer" of an expression. The `Expr` type, then, _fixes_ the recursion

  - essentially allowing us to represent an expression that may contain subexpressions.

Intuitively, this is necessary because without it we would have to write:

    ExprF (ExprF (ExprF (..)))

which extends forever! Representing the expression type in this way unlocks the
ability to use one of a number of _recursion schemes_ and allows us to create
recursive computations from non-recursive functions. This is nifty, but if you
can't easily model what you need to do using these schemes (like [`fold`](#fold)
or [`unfold`](#unfold)) then you might want to consider using [`raise`](./Expr#raise)
to lift your expression to the higher-level representation and work with it there.

-}
type Expr
    = Expr (ExprF Expr)


{-| -}
type ExprF r
    = EApp r r
    | ELam String r
    | ELet String r r
    | ELit (Literal r)
    | EPat r (List ( Pattern, Maybe r, r ))
    | EVar String


{-| -}
type Pattern
    = PAny
    | PLit (Literal Pattern)
    | PTyp String Pattern
    | PVar String


{-| -}
type Literal expr
    = LArr (List expr)
    | LCon String (List expr)
    | LNum Float
    | LRec (List ( String, expr ))
    | LStr String



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
app : Expr -> List Expr -> Expr
app fun args =
    case args of
        [] ->
            fun

        arg :: rest ->
            app (Expr <| EApp fun arg) rest


{-| -}
lam : List String -> Expr -> Expr
lam args body =
    case args of
        [] ->
            body

        arg :: rest ->
            Expr <| ELam arg <| lam rest body


{-| -}
let_ : List ( String, Expr ) -> Expr -> Expr
let_ bindings_ body =
    case bindings_ of
        [] ->
            body

        ( pattern, expr ) :: rest ->
            Expr <| ELet pattern expr <| let_ rest body


{-| -}
arr : List Expr -> Expr
arr elements =
    Expr <| ELit <| LArr elements


{-| -}
con : String -> List Expr -> Expr
con tag args =
    Expr <| ELit <| LCon tag args


{-| -}
num : Float -> Expr
num n =
    Expr <| ELit <| LNum n


{-| -}
rec : List ( String, Expr ) -> Expr
rec fields =
    Expr <| ELit <| LRec fields


{-| -}
str : String -> Expr
str s =
    Expr <| ELit <| LStr s


{-| -}
var : String -> Expr
var name =
    Expr <| EVar name


{-| -}
pat : Expr -> List ( Pattern, Maybe Expr, Expr ) -> Expr
pat expr cases =
    case cases of
        [] ->
            expr

        _ ->
            Expr <| EPat expr cases



-- QUERIES ---------------------------------------------------------------------


isPAny : Pattern -> Bool
isPAny p =
    case p of
        PAny ->
            True

        _ ->
            False


isPLit : Pattern -> Bool
isPLit p =
    case p of
        PLit _ ->
            True

        _ ->
            False


isPTyp : Pattern -> Bool
isPTyp p =
    case p of
        PTyp _ _ ->
            True

        _ ->
            False


isPVar : Pattern -> Bool
isPVar p =
    case p of
        PVar _ ->
            True

        _ ->
            False


bindings : Pattern -> List String
bindings p =
    case p of
        PAny ->
            []

        PLit (LArr ps) ->
            List.concatMap bindings ps

        PLit (LCon _ ps) ->
            List.concatMap bindings ps

        PLit (LRec ps) ->
            List.concatMap (bindings << Tuple.second) ps

        PLit _ ->
            []

        PTyp _ ps ->
            bindings ps

        PVar name ->
            [ name ]



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
map : (a -> b) -> ExprF a -> ExprF b
map f exprF =
    case exprF of
        EApp fun arg ->
            EApp (f fun) <| f arg

        ELam pattern expr ->
            ELam pattern <| f expr

        ELet pattern expr body ->
            ELet pattern (f expr) <| f body

        ELit (LArr elements) ->
            ELit <| LArr <| List.map f elements

        ELit (LCon tag args) ->
            ELit <| LCon tag <| List.map f args

        ELit (LNum n) ->
            ELit <| LNum n

        ELit (LRec fields) ->
            ELit <| LRec <| List.map (Tuple.mapSecond f) fields

        ELit (LStr s) ->
            ELit <| LStr s

        EVar name ->
            EVar name

        EPat expr cases ->
            EPat (f expr) <|
                List.map
                    (\( pattern, guard, body ) ->
                        ( pattern
                        , Maybe.map f guard
                        , f body
                        )
                    )
                    cases


{-| -}
fold : (ExprF a -> a) -> Expr -> a
fold f (Expr exprF) =
    exprF |> map (fold f) |> f


{-| -}
foldWith : (Expr -> ExprF a -> a) -> Expr -> a
foldWith f ((Expr exprF) as expr) =
    exprF |> map (foldWith f) |> f expr


{-| -}
unfold : (a -> ExprF a) -> a -> Expr
unfold f a =
    f a |> map (unfold f) |> Expr



-- CONVERSIONS -----------------------------------------------------------------
-- JSON ------------------------------------------------------------------------


encodePattern : Pattern -> Json.Encode.Value
encodePattern pattern =
    Json.Encode.list Basics.identity <|
        case pattern of
            PAny ->
                [ Metadata.encode "PAny" {}
                ]

            PLit literal ->
                [ Metadata.encode "PLit" {}
                , encodeLiteral encodePattern literal
                ]

            PTyp name pat_ ->
                [ Metadata.encode "PTyp" {}
                , Json.Encode.string name
                , encodePattern pat_
                ]

            PVar name ->
                [ Metadata.encode "PVar" {}
                , Json.Encode.string name
                ]


patternDecoder : Json.Decode.Decoder Pattern
patternDecoder =
    Metadata.decoder
        |> Json.Decode.andThen
            (\( key, _ ) ->
                case key of
                    "PAny" ->
                        Json.Decode.succeed PAny

                    "PLit" ->
                        Json.Decode.map PLit
                            (Json.Decode.index 1 <| literalDecoder <| Json.Decode.lazy (\_ -> patternDecoder))

                    "PTyp" ->
                        Json.Decode.map2 PTyp
                            (Json.Decode.index 1 <| Json.Decode.string)
                            (Json.Decode.index 2 <| Json.Decode.lazy (\_ -> patternDecoder))

                    "PVar" ->
                        Json.Decode.map PVar
                            (Json.Decode.index 1 <| Json.Decode.string)

                    _ ->
                        Json.Decode.fail <| "Unknown pattern type: " ++ key
            )


encodeLiteral : (expr -> Json.Encode.Value) -> Literal expr -> Json.Encode.Value
encodeLiteral encodeExpr_ literal =
    let
        encodeField ( k, v ) =
            Json.Encode.list Basics.identity
                [ Metadata.encode "Field" {}
                , Json.Encode.string k
                , encodeExpr_ v
                ]
    in
    Json.Encode.list Basics.identity <|
        case literal of
            LArr elements ->
                [ Metadata.encode "LArr" {}
                , Json.Encode.list encodeExpr_ elements
                ]

            LCon tag elements ->
                [ Metadata.encode "LCon" {}
                , Json.Encode.string tag
                , Json.Encode.list encodeExpr_ elements
                ]

            LNum n ->
                [ Metadata.encode "LNum" {}
                , Json.Encode.float n
                ]

            LRec fields ->
                [ Metadata.encode "LRec" {}
                , Json.Encode.list encodeField fields
                ]

            LStr s ->
                [ Metadata.encode "LStr" {}
                , Json.Encode.string s
                ]


literalDecoder : Json.Decode.Decoder expr -> Json.Decode.Decoder (Literal expr)
literalDecoder exprDecoder =
    let
        fieldDecoder =
            Metadata.decoder
                |> Json.Decode.andThen
                    (\( key, _ ) ->
                        if key == "Field" then
                            Json.Decode.map2 Tuple.pair
                                (Json.Decode.index 1 <| Json.Decode.string)
                                (Json.Decode.index 2 <| exprDecoder)

                        else
                            Json.Decode.fail <| "Unknown record field: " ++ key
                    )
    in
    Metadata.decoder
        |> Json.Decode.andThen
            (\( key, _ ) ->
                case key of
                    "LArr" ->
                        Json.Decode.map LArr
                            (Json.Decode.index 1 <| Json.Decode.list exprDecoder)

                    "LCon" ->
                        Json.Decode.map2 LCon
                            (Json.Decode.index 1 <| Json.Decode.string)
                            (Json.Decode.index 2 <| Json.Decode.list exprDecoder)

                    "LNum" ->
                        Json.Decode.map LNum
                            (Json.Decode.index 1 <| Json.Decode.float)

                    "LRec" ->
                        Json.Decode.map LRec
                            (Json.Decode.index 1 <| Json.Decode.list fieldDecoder)

                    "LStr" ->
                        Json.Decode.map LStr
                            (Json.Decode.index 1 <| Json.Decode.string)

                    _ ->
                        Json.Decode.fail <| "Unknown literal type: " ++ key
            )



-- UTILS -----------------------------------------------------------------------
