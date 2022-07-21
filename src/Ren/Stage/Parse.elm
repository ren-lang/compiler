module Ren.Stage.Parse exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Ren.Ast.Core as Core
import Ren.Ast.Expr as Expr exposing (Expr)
import Ren.Control.Parser as Parser exposing (Parser)
import Ren.Control.Parser.Pratt as Pratt
import Ren.Data.Declaration as Declaration exposing (Declaration)
import Ren.Data.Import as Import exposing (Import)
import Ren.Data.Module as Module exposing (Module)
import Ren.Data.Token as Token
import Util.Maybe as Maybe
import Util.Triple as Triple



--


parse : Token.Stream -> Result () Module
parse stream =
    stream
        |> Parser.run module_
        |> Result.mapError (\_ -> ())


parseDeclaration : Token.Stream -> Result () Declaration
parseDeclaration stream =
    stream
        |> Parser.run declaration
        |> Result.mapError (\_ -> ())


parseExpr : Token.Stream -> Result () Expr
parseExpr stream =
    stream
        |> Parser.run (expr { inArgPosition = False })
        |> Result.mapError (\_ -> ())



-- PARSERS: MODULE -------------------------------------------------------------


module_ : Parser () () Module
module_ =
    let
        makeModule imports declarations =
            Module.empty
                |> (\m -> List.foldl Module.addImport m imports)
                |> (\m -> List.foldl Module.addDeclaration m declarations)
    in
    Parser.succeed makeModule
        |> Parser.keep
            (Parser.loop []
                (\imps ->
                    Parser.oneOf
                        [ Parser.succeed (\imp -> imp :: imps)
                            |> Parser.keep import_
                            |> Parser.map Parser.Continue
                        , Parser.succeed ()
                            |> Parser.map (\_ -> List.reverse imps)
                            |> Parser.map Parser.Break
                        ]
                )
            )
        |> Parser.keep
            (Parser.loop []
                (\decs ->
                    Parser.oneOf
                        [ Parser.succeed (\dec -> dec :: decs)
                            |> Parser.keep declaration
                            |> Parser.map Parser.Continue
                        , Parser.succeed ()
                            |> Parser.map (\_ -> List.reverse decs)
                            |> Parser.map Parser.Break
                        ]
                )
            )



-- PARSERS: IMPORTS ------------------------------------------------------------


import_ : Parser () () Import
import_ =
    Parser.succeed Basics.identity
        |> Parser.drop (Parser.keyword () Token.Import)
        |> Parser.keep
            (Parser.oneOf
                [ Parser.succeed Import.package
                    |> Parser.drop (Parser.keyword () Token.Pkg)
                , Parser.succeed Import.external
                    |> Parser.drop (Parser.keyword () Token.Ext)
                , Parser.succeed Import.local
                ]
            )
        |> Parser.andThen
            (\makeImport ->
                Parser.succeed makeImport
                    |> Parser.keep (Parser.string ())
                    |> Parser.keep
                        (Parser.oneOf
                            [ Parser.succeed (::)
                                |> Parser.drop (Parser.keyword () Token.As)
                                |> Parser.keep (Parser.identifier () Token.Upper)
                                |> Parser.keep
                                    (Parser.loop []
                                        (\ns ->
                                            Parser.oneOf
                                                [ Parser.succeed (\n -> n :: ns)
                                                    |> Parser.drop (Parser.symbol () Token.Period)
                                                    |> Parser.keep (Parser.identifier () Token.Upper)
                                                    |> Parser.map Parser.Continue
                                                , Parser.succeed ()
                                                    |> Parser.map (\_ -> List.reverse ns)
                                                    |> Parser.map Parser.Break
                                                ]
                                        )
                                    )
                            , Parser.succeed []
                            ]
                        )
                    |> Parser.keep
                        (Parser.oneOf
                            [ Parser.succeed (::)
                                |> Parser.drop (Parser.keyword () Token.Exposing)
                                |> Parser.drop (Parser.symbol () <| Token.Brace Token.Left)
                                |> Parser.keep (Parser.identifier () Token.Lower)
                                |> Parser.keep
                                    (Parser.loop []
                                        (\ns ->
                                            Parser.oneOf
                                                [ Parser.succeed (\n -> n :: ns)
                                                    |> Parser.drop (Parser.symbol () Token.Comma)
                                                    |> Parser.keep (Parser.identifier () Token.Lower)
                                                    |> Parser.map Parser.Continue
                                                , Parser.succeed ()
                                                    |> Parser.drop (Parser.symbol () <| Token.Brace Token.Right)
                                                    |> Parser.map (\_ -> List.reverse ns)
                                                    |> Parser.map Parser.Break
                                                ]
                                        )
                                    )
                            , Parser.succeed []
                            ]
                        )
            )



-- PARSERS: DECLARATIONS -------------------------------------------------------


declaration : Parser () () Declaration
declaration =
    Parser.andThen (\pub -> Parser.oneOf [ localDeclaration pub, externalDeclaration pub ]) <|
        Parser.oneOf
            [ Parser.succeed True |> Parser.drop (Parser.keyword () Token.Pub)
            , Parser.succeed False
            ]


localDeclaration : Bool -> Parser () () Declaration
localDeclaration pub =
    Parser.succeed (Declaration.local pub)
        |> Parser.drop (Parser.keyword () Token.Let)
        |> Parser.keep (Parser.identifier () Token.Lower)
        |> Parser.drop (Parser.symbol () Token.Equal)
        |> Parser.keep (expr { inArgPosition = True })


externalDeclaration : Bool -> Parser () () Declaration
externalDeclaration pub =
    Parser.succeed (Declaration.external pub)
        |> Parser.drop (Parser.keyword () Token.Ext)
        |> Parser.keep (Parser.identifier () Token.Lower)
        |> Parser.drop (Parser.symbol () Token.Equal)
        |> Parser.keep (Parser.string ())



-- PARSERS: EXPRESSIONS --------------------------------------------------------


type alias Context =
    { inArgPosition : Bool
    }


expr : Context -> Parser () () Expr
expr context =
    Pratt.expression
        { oneOf =
            List.concat
                [ callables context

                -- When we're parsing function application or constructor literals,
                -- some expressions are never valid unless parenthesised. We keep
                -- track of this in the context, and if we are in an argument
                -- position then we don't want to include these parsers at all,
                -- they're already covered in `callables` in `parenthesised`.
                , if context.inArgPosition then
                    []

                  else
                    [ if_
                    , lambda
                    , let_
                    , where_
                    ]
                ]
        , andThenOneOf =
            if context.inArgPosition then
                []

            else
                operators
        , spaces =
            Parser.oneOf
                [ Parser.loop ()
                    (\_ ->
                        Parser.oneOf
                            [ Parser.succeed ()
                                |> Parser.drop (Parser.comment ())
                                |> Parser.map Parser.Continue
                            , Parser.succeed ()
                                |> Parser.map Parser.Break
                            ]
                    )
                , Parser.succeed ()
                ]
        }


operators : List (Pratt.Operator () () Expr)
operators =
    let
        operator ( assoc, precedence, sym ) =
            assoc precedence (Parser.operator () sym) (Expr.Binop sym)
    in
    List.map operator
        [ -- Left associativity
          ( Pratt.infixLeft, 1, Expr.Pipe )
        , ( Pratt.infixLeft, 4, Expr.Eq )
        , ( Pratt.infixLeft, 4, Expr.Gt )
        , ( Pratt.infixLeft, 4, Expr.Gte )
        , ( Pratt.infixLeft, 4, Expr.Lt )
        , ( Pratt.infixLeft, 4, Expr.Lte )
        , ( Pratt.infixLeft, 4, Expr.Neq )
        , ( Pratt.infixLeft, 6, Expr.Add )
        , ( Pratt.infixLeft, 6, Expr.Sub )
        , ( Pratt.infixLeft, 7, Expr.Div )
        , ( Pratt.infixLeft, 7, Expr.Mul )

        -- Right associativity
        , ( Pratt.infixRight, 2, Expr.Or )
        , ( Pratt.infixRight, 3, Expr.And )
        , ( Pratt.infixRight, 5, Expr.Concat )
        , ( Pratt.infixRight, 5, Expr.Cons )
        ]


callables : Context -> List (Pratt.Parsers () () Expr -> Parser () () Expr)
callables context =
    List.map (Parser.andThen access >> Parser.andThen (call context) >> Pratt.literal)
        [ parenthesised
        , placeholder
        , scoped
        , var
        , literal context Expr.Literal Expr.Var <| Parser.lazy <| \_ -> expr { inArgPosition = True }
        ]



--


parenthesised : Parser () () Expr
parenthesised =
    Parser.succeed Basics.identity
        |> Parser.drop (Parser.symbol () <| Token.Paren Token.Left)
        |> Parser.keep (Parser.lazy <| \_ -> expr { inArgPosition = False })
        |> Parser.drop (Parser.symbol () <| Token.Paren Token.Right)



--


access : Expr -> Parser () () Expr
access expr_ =
    let
        chainAccessors key keys =
            List.foldl (\k e -> Expr.Access e k) (Expr.Access expr_ key) keys
    in
    Parser.oneOf
        [ Parser.succeed chainAccessors
            |> Parser.drop (Parser.symbol () <| Token.Period)
            |> Parser.keep (Parser.identifier () Token.Lower)
            |> Parser.keep
                (Parser.many
                    (\ks ->
                        [ Parser.succeed (\k -> k :: ks)
                            |> Parser.drop (Parser.symbol () Token.Period)
                            |> Parser.keep (Parser.identifier () Token.Lower)
                            |> Parser.map Parser.Continue
                        , Parser.succeed ()
                            |> Parser.map (\_ -> List.reverse ks)
                            |> Parser.map Parser.Break
                        ]
                    )
                )
        , Parser.succeed expr_
        ]


call : Context -> Expr -> Parser () () Expr
call { inArgPosition } expr_ =
    let
        argParser =
            Parser.lazy <| \_ -> expr { inArgPosition = True }
    in
    if inArgPosition then
        Parser.succeed expr_

    else
        Parser.oneOf
            [ Parser.succeed (\arg args -> Expr.Call expr_ (arg :: args))
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
            , Parser.succeed expr_
            ]


if_ : Pratt.Parsers () () Expr -> Parser () () Expr
if_ parsers =
    Parser.succeed Expr.If
        |> Parser.drop (Parser.keyword () Token.If)
        |> Parser.keep (Pratt.subExpression 0 parsers)
        |> Parser.drop (Parser.keyword () Token.Then)
        |> Parser.keep (Pratt.subExpression 0 parsers)
        |> Parser.drop (Parser.keyword () Token.Else)
        |> Parser.keep (Pratt.subExpression 0 parsers)


lambda : Pratt.Parsers () () Expr -> Parser () () Expr
lambda parsers =
    Parser.succeed (\pat rest body -> Expr.Lambda (pat :: rest) body)
        |> Parser.drop (Parser.keyword () Token.Fun)
        |> Parser.keep (pattern { inArgPosition = True })
        |> Parser.keep
            (Parser.loop []
                (\pats ->
                    Parser.oneOf
                        [ Parser.succeed (\pat -> pat :: pats)
                            |> Parser.keep (pattern { inArgPosition = True })
                            |> Parser.map Parser.Continue
                        , Parser.succeed ()
                            |> Parser.drop (Parser.symbol () Token.FatArrow)
                            |> Parser.map (\_ -> List.reverse pats)
                            |> Parser.map Parser.Break
                        ]
                )
            )
        |> Parser.keep (Pratt.subExpression 0 parsers)


let_ : Pratt.Parsers () () Expr -> Parser () () Expr
let_ parsers =
    Parser.succeed Expr.Let
        |> Parser.drop (Parser.keyword () Token.Let)
        |> Parser.keep (pattern { inArgPosition = True })
        |> Parser.drop (Parser.symbol () Token.Equal)
        |> Parser.keep (Pratt.subExpression 0 parsers)
        |> Parser.drop (Parser.symbol () Token.Semicolon)
        |> Parser.keep (Pratt.subExpression 0 parsers)


placeholder : Parser () () Expr
placeholder =
    Parser.succeed Expr.Placeholder
        |> Parser.drop (Parser.symbol () Token.Underscore)


scoped : Parser () () Expr
scoped =
    Parser.succeed (\n ns name -> Expr.Scoped (n :: ns) name)
        |> Parser.keep (Parser.identifier () Token.Upper)
        |> Parser.keep
            (Parser.loop []
                (\ns ->
                    Parser.succeed Basics.identity
                        |> Parser.drop (Parser.symbol () Token.Period)
                        |> Parser.keep
                            (Parser.oneOf
                                [ Parser.succeed (\n -> n :: ns)
                                    |> Parser.keep (Parser.identifier () Token.Upper)
                                    |> Parser.map Parser.Continue
                                , Parser.succeed ()
                                    |> Parser.map (\_ -> List.reverse ns)
                                    |> Parser.map Parser.Break
                                ]
                            )
                )
            )
        |> Parser.keep (Parser.identifier () Token.Lower)


var : Parser () () Expr
var =
    Parser.succeed Expr.Var
        |> Parser.keep (Parser.identifier () Token.Lower)


where_ : Pratt.Parsers () () Expr -> Parser () () Expr
where_ parsers =
    let
        case_ =
            Parser.succeed Triple.from
                |> Parser.drop (Parser.keyword () Token.Is)
                |> Parser.keep (pattern { inArgPosition = False })
                |> Parser.keep
                    (Parser.oneOf
                        [ Parser.succeed Just
                            |> Parser.drop (Parser.keyword () Token.If)
                            |> Parser.keep (Pratt.subExpression 0 parsers)
                        , Parser.succeed Nothing
                        ]
                    )
                |> Parser.drop (Parser.symbol () Token.FatArrow)
                |> Parser.keep (Pratt.subExpression 0 parsers)
    in
    Parser.succeed (\expr_ c cs -> Expr.Where expr_ (c :: cs))
        |> Parser.drop (Parser.keyword () Token.Where)
        |> Parser.keep (Pratt.subExpression 0 parsers)
        |> Parser.keep case_
        |> Parser.keep
            (Parser.loop []
                (\cs ->
                    Parser.oneOf
                        [ Parser.succeed (\c -> c :: cs)
                            |> Parser.keep case_
                            |> Parser.map Parser.Continue
                        , Parser.succeed ()
                            |> Parser.map (\_ -> List.reverse cs)
                            |> Parser.map Parser.Break
                        ]
                )
            )



-- PARSERS: LITERALS -----------------------------------------------------------


literal : Context -> (Core.Literal a -> a) -> (String -> a) -> Parser () () a -> Parser () () a
literal context wrap fromString subexpr =
    Parser.map wrap <|
        Parser.oneOf
            [ array subexpr
            , constructor context subexpr
            , number
            , record fromString subexpr
            , string
            ]


array : Parser () () a -> Parser () () (Core.Literal a)
array subexpr =
    let
        elements =
            Parser.loop []
                (\els ->
                    Parser.oneOf
                        [ Parser.succeed (\el -> el :: els)
                            |> Parser.drop (Parser.symbol () <| Token.Comma)
                            |> Parser.keep subexpr
                            |> Parser.map Parser.Continue
                        , Parser.succeed (\_ -> List.reverse els)
                            |> Parser.keep (Parser.symbol () <| Token.Bracket Token.Right)
                            |> Parser.map Parser.Break
                        ]
                )
    in
    Parser.succeed Core.LArr
        |> Parser.drop (Parser.symbol () <| Token.Bracket Token.Left)
        |> Parser.keep
            (Parser.oneOf
                [ Parser.succeed (::)
                    |> Parser.keep subexpr
                    |> Parser.keep elements
                , Parser.succeed []
                    |> Parser.drop (Parser.symbol () <| Token.Bracket Token.Right)
                ]
            )


constructor : Context -> Parser () () a -> Parser () () (Core.Literal a)
constructor { inArgPosition } subexpr =
    let
        args =
            Parser.many
                (\xs ->
                    [ Parser.succeed (\x -> x :: xs)
                        |> Parser.keep subexpr
                        |> Parser.map Parser.Continue
                    , Parser.succeed (List.reverse xs)
                        |> Parser.map Parser.Break
                    ]
                )
    in
    Parser.succeed Core.LCon
        |> Parser.drop (Parser.symbol () <| Token.Hash)
        |> Parser.keep (Parser.identifier () Token.Lower)
        |> Parser.andThen
            (\con ->
                if inArgPosition then
                    Parser.succeed <| con []

                else
                    Parser.map con args
            )


number : Parser () () (Core.Literal a)
number =
    Parser.succeed Core.LNum
        |> Parser.keep (Parser.number ())


record : (String -> a) -> Parser () () a -> Parser () () (Core.Literal a)
record fromString subexpr =
    let
        field =
            Parser.succeed (\key val -> ( key, Maybe.withDefault (fromString key) val ))
                |> Parser.keep (Parser.identifier () Token.Lower)
                |> Parser.keep
                    (Parser.oneOf
                        [ Parser.succeed Just
                            |> Parser.drop (Parser.symbol () <| Token.Colon)
                            |> Parser.keep subexpr
                        , Parser.succeed Nothing
                        ]
                    )

        fields =
            Parser.loop []
                (\fs ->
                    Parser.oneOf
                        [ Parser.succeed (\f -> f :: fs)
                            |> Parser.drop (Parser.symbol () Token.Comma)
                            |> Parser.keep field
                            |> Parser.map Parser.Continue
                        , Parser.succeed (\_ -> List.reverse fs)
                            |> Parser.keep (Parser.symbol () <| Token.Brace Token.Right)
                            |> Parser.map Parser.Break
                        ]
                )
    in
    Parser.succeed Core.LRec
        |> Parser.drop (Parser.symbol () <| Token.Brace Token.Left)
        |> Parser.keep
            (Parser.oneOf
                [ Parser.succeed (::)
                    |> Parser.keep field
                    |> Parser.keep fields
                , Parser.succeed []
                    |> Parser.drop (Parser.symbol () <| Token.Brace Token.Right)
                ]
            )


string : Parser () () (Core.Literal a)
string =
    Parser.succeed Core.LStr
        |> Parser.keep (Parser.string ())



-- PARSERS: PATTERNS -----------------------------------------------------------


pattern : Context -> Parser () () Core.Pattern
pattern context =
    Parser.oneOf
        [ pparens
        , pany
        , plit context
        , ptyp
        , pvar
        ]


pparens : Parser () () Core.Pattern
pparens =
    Parser.succeed Basics.identity
        |> Parser.drop (Parser.symbol () <| Token.Paren Token.Left)
        |> Parser.keep (Parser.lazy <| \_ -> pattern { inArgPosition = False })
        |> Parser.drop (Parser.symbol () <| Token.Paren Token.Right)


pany : Parser () () Core.Pattern
pany =
    Parser.succeed Core.PAny
        |> Parser.drop (Parser.symbol () Token.Underscore)


plit : Context -> Parser () () Core.Pattern
plit context =
    literal context Core.PLit Core.PVar <| Parser.lazy (\_ -> pattern { inArgPosition = True })


ptyp : Parser () () Core.Pattern
ptyp =
    Parser.succeed Core.PTyp
        |> Parser.drop (Parser.symbol () Token.At)
        |> Parser.keep (Parser.identifier () Token.Upper)
        |> Parser.keep (Parser.lazy (\_ -> pattern { inArgPosition = True }))


pvar : Parser () () Core.Pattern
pvar =
    Parser.succeed Core.PVar
        |> Parser.keep (Parser.identifier () Token.Lower)
