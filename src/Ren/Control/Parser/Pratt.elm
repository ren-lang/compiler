module Ren.Control.Parser.Pratt exposing
    ( Parsers, Config, Operator
    , expression, subExpression
    , literal, constant, prefix
    , infixLeft, infixRight, postfix
    )

{-|


## Types

@docs Parsers, Config, Operator


## Constructors

@docs expression, subExpression
@docs literal, constant, prefix
@docs infixLeft, infixRight, postfix

-}

-- IMPORTS --------------------------------------------------------------------

import Ren.Control.Parser as Parser exposing (Parser)



-- TYPES -----------------------------------------------------------------------


type Parsers c x e
    = Parsers (Config c x e)


type alias Config c x e =
    { oneOf : List (Parsers c x e -> Parser c x e)
    , andThenOneOf : List (Operator c x e)
    }


type alias Operator c x e =
    Parsers c x e -> ( Int, e -> Parser c x e )



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
expression : Config c x e -> Parser c x e
expression config =
    subExpression 0 <| Parsers config


{-| -}
subExpression : Int -> Parsers c x e -> Parser c x e
subExpression precedence ((Parsers { oneOf }) as config) =
    let
        parseExpr _ =
            Parser.oneOf <| List.map ((|>) config) oneOf

        go expr =
            Parser.oneOf
                [ Parser.succeed expr
                    |> Parser.andThen (operation config precedence)
                    |> Parser.map Parser.Continue
                , Parser.succeed expr
                    |> Parser.map Parser.Break
                ]
    in
    Parser.lazy parseExpr
        |> Parser.andThen (\expr -> Parser.loop expr go)


operation : Parsers c x e -> Int -> e -> Parser c x e
operation ((Parsers { andThenOneOf }) as config) currentPrecedence expr =
    let
        parseOperation ( precedence, parser ) =
            if precedence > currentPrecedence then
                Just <| parser expr

            else
                Nothing
    in
    Parser.oneOf <| List.filterMap ((|>) config >> parseOperation) andThenOneOf



--


{-| -}
literal : Parser c x e -> Parsers c x e -> Parser c x e
literal =
    Basics.always


{-| -}
constant : Parser c x () -> e -> Parsers c x e -> Parser c x e
constant constantParser e _ =
    Parser.map (Basics.always e) constantParser


{-| -}
prefix : Int -> Parser c x () -> (e -> e) -> Parsers c x e -> Parser c x e
prefix precedence operator apply config =
    Parser.succeed apply
        |> Parser.drop operator
        |> Parser.keep (subExpression precedence config)



--


{-| -}
infixLeft : Int -> Parser c x () -> (e -> e -> e) -> Operator c x e
infixLeft precedence =
    makeInfix ( precedence, precedence )


{-| -}
infixRight : Int -> Parser c x () -> (e -> e -> e) -> Operator c x e
infixRight precedence =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    makeInfix ( precedence, precedence - 1 )


{-| -}
postfix : Int -> Parser c x () -> (e -> e) -> Operator c x e
postfix leftPrecedence operator apply =
    \_ ->
        ( leftPrecedence
        , \lhs -> Parser.map (\_ -> apply lhs) operator
        )



-- UTILS -----------------------------------------------------------------------


makeInfix : ( Int, Int ) -> Parser c x () -> (e -> e -> e) -> Operator c x e
makeInfix ( leftPrecedence, rightPrecedence ) operator makeExpr =
    \config ->
        ( leftPrecedence
        , \lhs ->
            Parser.succeed (makeExpr lhs)
                |> Parser.drop operator
                |> Parser.keep (subExpression rightPrecedence config)
        )
