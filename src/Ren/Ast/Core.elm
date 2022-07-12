module Ren.Ast.Core exposing
    ( Expr(..)
    , ExprF(..), Pattern(..), Literal(..)
    , app, lam, let_, var, pat
    , arr, con, num, rec, str
    , map, fold, foldWith, unfold
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


## Manipulations

@docs map, fold, foldWith, unfold


## Conversions


## Utils

-}

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
let_ bindings body =
    case bindings of
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
-- UTILS -----------------------------------------------------------------------
