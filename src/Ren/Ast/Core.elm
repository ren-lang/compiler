module Ren.Ast.Core exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------


type Expr
    = Expr (ExprF Expr)


type ExprF r
    = EAbs String r
    | EApp r r
    | ELet String r r
    | ELit (Literal r)
    | EVar String
    | EPat r (List ( Pattern, Maybe r, r ))


type Pattern
    = PAny
    | PLit (Literal Pattern)
    | PTyp String Pattern
    | PVar String


type Literal expr
    = LArr (List expr)
    | LBool Bool
    | LCon String (List expr)
    | LNum Float
    | LRec (List ( String, expr ))
    | LStr String
    | LUnit



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


abs : List String -> Expr -> Expr
abs args body =
    case args of
        [] ->
            body

        arg :: rest ->
            Expr <| EAbs arg <| abs rest body


app : Expr -> List Expr -> Expr
app fun args =
    case args of
        [] ->
            fun

        arg :: rest ->
            app (Expr <| EApp fun arg) rest


let_ : List ( String, Expr ) -> Expr -> Expr
let_ bindings body =
    case bindings of
        [] ->
            body

        ( pattern, expr ) :: rest ->
            Expr <| ELet pattern expr <| let_ rest body


arr : List Expr -> Expr
arr elements =
    Expr <| ELit <| LArr elements


bool : Bool -> Expr
bool b =
    Expr <| ELit <| LBool b


con : String -> List Expr -> Expr
con tag args =
    Expr <| ELit <| LCon tag args


num : Float -> Expr
num n =
    Expr <| ELit <| LNum n


rec : List ( String, Expr ) -> Expr
rec fields =
    Expr <| ELit <| LRec fields


str : String -> Expr
str s =
    Expr <| ELit <| LStr s


unit : Expr
unit =
    Expr <| ELit <| LUnit


var : String -> Expr
var name =
    Expr <| EVar name


pat : Expr -> List ( Pattern, Maybe Expr, Expr ) -> Expr
pat expr cases =
    case cases of
        [] ->
            expr

        _ ->
            Expr <| EPat expr cases



-- QUERIES ---------------------------------------------------------------------
-- MANIPULATIONS ---------------------------------------------------------------


map : (a -> b) -> ExprF a -> ExprF b
map f exprF =
    case exprF of
        EAbs pattern expr ->
            EAbs pattern <| f expr

        EApp fun arg ->
            EApp (f fun) <| f arg

        ELet pattern expr body ->
            ELet pattern (f expr) <| f body

        ELit (LArr elements) ->
            ELit <| LArr <| List.map f elements

        ELit (LBool b) ->
            ELit <| LBool b

        ELit (LCon tag args) ->
            ELit <| LCon tag <| List.map f args

        ELit (LNum n) ->
            ELit <| LNum n

        ELit (LRec fields) ->
            ELit <| LRec <| List.map (Tuple.mapSecond f) fields

        ELit (LStr s) ->
            ELit <| LStr s

        ELit LUnit ->
            ELit <| LUnit

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


fold : (ExprF a -> a) -> Expr -> a
fold f (Expr exprF) =
    exprF |> map (fold f) |> f


foldWith : (Expr -> ExprF a -> a) -> Expr -> a
foldWith f ((Expr exprF) as expr) =
    exprF |> map (foldWith f) |> f expr


unfold : (a -> ExprF a) -> a -> Expr
unfold f a =
    f a |> map (unfold f) |> Expr



-- CONVERSIONS -----------------------------------------------------------------
-- UTILS -----------------------------------------------------------------------
