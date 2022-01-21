module Ren.Compiler.Parse exposing
    ( run
    , Context(..), Error(..)
    )

{-|

@docs run
@docs Context, Error

-}

-- IMPORTS ---------------------------------------------------------------------

import Data.Either
import Dict
import Parser.Advanced as Parser exposing ((|.), (|=))
import Pratt.Advanced as Pratt
import Ren.AST.Expr as Expr exposing (Expr(..), ExprF(..))
import Ren.AST.Module as Module exposing (Module)
import Ren.Data.Span as Span exposing (Span)
import Ren.Data.Type as Type exposing (Type)
import Set exposing (Set)


{-| -}
run : String -> Result (List (Parser.DeadEnd Context Error)) (Module Span)
run =
    Parser.run module_



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Parser a =
    Parser.Parser Context Error a


{-| -}
type alias Config a =
    Pratt.Config Context Error a


{-| -}
type Context
    = InImport
    | InDeclaration
    | InExpr


{-| -}
type Error
    = ExpectingKeyword String
    | ExpectingSymbol String
    | ExpectingOperator String
    | ExpectingTypePattern (List String)
    | ExpectingChar
    | UnexpextedChar Char
    | ExpectingNumber
    | ExpectingEOF
    | ExpectingWhitespace
    | ExpectingCamelCase
    | ExpectingCapitalCase
    | InternalError String



--                                                                            --
-- MODULE PARSERS --------------------------------------------------------------
--                                                                            --


{-| -}
module_ : Parser (Module Span)
module_ =
    Parser.succeed Module
        |. Parser.spaces
        |= Parser.loop []
            (\imports ->
                Parser.oneOf
                    [ Parser.succeed (\i -> i :: imports)
                        |= import_
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed ()
                        |> Parser.map (\_ -> List.reverse imports)
                        |> Parser.map Parser.Done
                    ]
            )
        |. Parser.spaces
        |= Parser.loop []
            (\declarations ->
                Parser.oneOf
                    [ Parser.succeed (\d -> d :: declarations)
                        |= declaration
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed ()
                        |> Parser.map (\_ -> List.reverse declarations)
                        |> Parser.map Parser.Done
                    ]
            )
        |. Parser.spaces
        |. Parser.end ExpectingEOF


{-| -}
import_ : Parser Module.Import
import_ =
    Parser.succeed Module.Import
        |. keyword "import"
        |. Parser.spaces
        -- TODO: This doesn't handle escaped `"` characters.
        |. symbol "\""
        |= (Parser.getChompedString <| Parser.chompWhile ((/=) '"'))
        |. symbol "\""
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed Basics.identity
                |. keyword "as"
                |. Parser.spaces
                |= Parser.loop []
                    (\names ->
                        Parser.oneOf
                            [ Parser.succeed (\n -> n :: names)
                                |= uppercaseName Set.empty
                                |. symbol "."
                                |> Parser.map Parser.Loop
                                |> Parser.backtrackable
                            , Parser.succeed (\n -> n :: names)
                                |= uppercaseName Set.empty
                                |> Parser.map (Parser.Done << List.reverse)
                            , Parser.succeed ()
                                |> Parser.map (\_ -> List.reverse names)
                                |> Parser.map Parser.Done
                            ]
                    )
            , Parser.succeed []
            ]
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed Basics.identity
                |. keyword "exposing"
                |. Parser.spaces
                |= Parser.sequence
                    { start = Parser.Token "{" (ExpectingSymbol "{")
                    , separator = Parser.Token "," (ExpectingSymbol ",")
                    , end = Parser.Token "}" (ExpectingSymbol "}")
                    , spaces = Parser.spaces
                    , item = lowercaseName keywords
                    , trailing = Parser.Forbidden
                    }
                |> Parser.backtrackable
            , Parser.succeed []
            ]
        |> Parser.inContext InImport


{-| -}
declaration : Parser (Module.Declaration Span)
declaration =
    Parser.oneOf
        [ Parser.succeed (Module.Declaration False "_" Type.Any)
            |. keyword "run"
            |. Parser.commit ()
            |= expression
            |> Span.parser (|>)
            |> Parser.inContext InDeclaration
        , Parser.succeed Module.Declaration
            |= Parser.oneOf
                [ Parser.succeed True
                    |. keyword "pub"
                , Parser.succeed False
                ]
            |. Parser.spaces
            |. keyword "let"
            |. Parser.spaces
            |= lowercaseName keywords
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed Basics.identity
                    |. symbol ":"
                    |. Parser.spaces
                    |= type_
                    |. Parser.spaces
                , Parser.succeed Type.Any
                ]
            |. symbol "="
            |. Parser.spaces
            |= expression
            |> Span.parser (|>)
            |> Parser.inContext InDeclaration
        ]



--                                                                            --
-- EXPRESSION PARSERS ----------------------------------------------------------
--                                                                            --


{-| -}
expression : Parser (Expr Span)
expression =
    prattExpression
        -- These parsers start with a keyword
        [ conditional
        , match

        -- These parsers parse sub expressions
        , Pratt.literal annotation
        , lambda
        , application
        , Pratt.literal access

        --
        , Pratt.literal identifier

        -- Blocks and record literals can both begin with a `{`. I'm not sure it
        -- matters which one we try first, though.
        , block
        , literal

        -- Subexpressions are wrapped in parentheses.
        , Pratt.literal (Parser.lazy (\_ -> subexpression))
        ]


{-| -}
prattExpression : List (Config (Expr Span) -> Parser (Expr Span)) -> Parser (Expr Span)
prattExpression parsers =
    Pratt.expression
        { oneOf = parsers
        , andThenOneOf =
            let
                -- This cursed dummy Span is necessary because we need the
                -- type of the infix expression to be the same as its sub-expressions
                -- but we can't get the Span start/end of the parser until
                -- we've created the parser...
                --
                -- It's cursed but it works.
                dummySpan =
                    Span.fromTuples ( 0, 0 ) ( 0, 0 )

                -- Helper parser for handling infix operator parsing. Takes the
                -- required symbol as a string helpfully wraps it up in a the
                -- `Parser.symbol` parser.
                infix_ parser precedence sym op =
                    Tuple.mapSecond locateInfix
                        << parser precedence
                            (operator sym)
                            (\lhs rhs -> Expr.wrap dummySpan (Infix op lhs rhs))

                -- This annotates a parsed infix expression with start/end Span
                -- data by taking the start of the left operand and the end of
                -- the right one.
                --
                -- Of course, we have to pattern match on the parsed expression
                -- even though we know it will be an Infix one, so we throw an
                -- internal error if somehow things go wrong.
                locateInfix parser expr =
                    Parser.andThen
                        (\(Expr _ e) ->
                            case e of
                                Infix _ (Expr { start } _) (Expr { end } _) ->
                                    Expr (Span start end) e
                                        |> Parser.succeed

                                _ ->
                                    InternalError "Parsed something other than an `Infix` expression in `andThenOneOf`"
                                        |> Parser.problem
                        )
                        (parser expr)
            in
            [ infix_ Pratt.infixLeft 1 "|>" Expr.Pipe
            , infix_ Pratt.infixRight 9 ">>" Expr.Compose
            , infix_ Pratt.infixLeft 4 "==" Expr.Eq
            , infix_ Pratt.infixLeft 4 "!=" Expr.NotEq
            , infix_ Pratt.infixLeft 4 "<=" Expr.Lte
            , infix_ Pratt.infixLeft 4 ">=" Expr.Lte
            , infix_ Pratt.infixRight 3 "&&" Expr.And
            , infix_ Pratt.infixRight 2 "||" Expr.Or
            , infix_ Pratt.infixRight 5 "::" Expr.Cons
            , infix_ Pratt.infixRight 5 "++" Expr.Join

            -- ONE CHARACTER OPERATORS
            , infix_ Pratt.infixLeft 4 "<" Expr.Lt
            , infix_ Pratt.infixLeft 4 ">" Expr.Gt
            , infix_ Pratt.infixLeft 6 "+" Expr.Add
            , infix_ Pratt.infixLeft 6 "-" Expr.Sub
            , infix_ Pratt.infixLeft 7 "*" Expr.Mul
            , infix_ Pratt.infixRight 7 "^" Expr.Pow
            , infix_ Pratt.infixRight 7 "%" Expr.Mod
            ]
        , spaces = Parser.spaces
        }
        |> Parser.inContext InExpr


{-| -}
subexpression : Parser (Expr Span)
subexpression =
    Parser.succeed identity
        |. symbol "("
        |. Parser.spaces
        |= expression
        |. Parser.spaces
        |. symbol ")"


{-| -}
parenthesised : Parser (Expr Span)
parenthesised =
    Parser.oneOf
        [ -- These are all the expressions that can be unambiguously parsed
          -- without parentheses in contexts where parentheses might be necessary.
          Pratt.expression
            { oneOf =
                [ block
                , \config ->
                    Parser.succeed Literal
                        |= Parser.oneOf
                            [ array config
                            , boolean
                            , number
                            , record config
                            , string
                            , template config
                            , undefined
                            ]
                        |> Span.parser Expr
                , Pratt.literal identifier
                ]
            , andThenOneOf = []
            , spaces = Parser.spaces
            }
            |> Parser.inContext InExpr
        , Parser.lazy (\_ -> subexpression)
        ]



-- EXPRESSION PARSERS: ACCESSORS -----------------------------------------------


{-| -}
access : Parser (Expr Span)
access =
    Parser.succeed (\expr accessor accessors -> Access expr (accessor :: accessors))
        |= Parser.lazy (\_ -> parenthesised)
        |. Parser.spaces
        |. symbol "."
        |. Parser.commit ()
        |= lowercaseName Set.empty
        |. Parser.spaces
        |= Parser.loop []
            (\accessors ->
                Parser.oneOf
                    [ Parser.succeed (\accessor -> accessor :: accessors)
                        |. symbol "."
                        |= lowercaseName Set.empty
                        |> Parser.map Parser.Loop
                    , Parser.succeed ()
                        |> Parser.map (\_ -> List.reverse accessors)
                        |> Parser.map Parser.Done
                    ]
            )
        |> Parser.backtrackable
        |> Span.parser Expr



-- EXPRESSION PARSERS: APPLICATION ---------------------------------------------


{-| -}
application : Config (Expr Span) -> Parser (Expr Span)
application config =
    Parser.succeed (\f arg args -> Application f (arg :: args))
        |= Parser.oneOf
            [ access
            , block config
            , Parser.lazy (\_ -> subexpression)
            , identifier
            ]
        |. Parser.spaces
        |= parenthesised
        |. Parser.commit ()
        |. Parser.spaces
        |= Parser.loop []
            (\args ->
                Parser.oneOf
                    [ Parser.succeed (\arg -> arg :: args)
                        |= parenthesised
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed ()
                        |> Parser.map (\_ -> List.reverse args)
                        |> Parser.map Parser.Done
                    ]
            )
        |> Parser.backtrackable
        |> Span.parser Expr



-- EXPRESSION PARSERS: ANNOTATION ----------------------------------------------


annotation : Parser (Expr Span)
annotation =
    Parser.succeed Annotation
        |= parenthesised
        |. Parser.spaces
        |. keyword "as"
        |. Parser.commit ()
        |. Parser.spaces
        |= type_
        |> Parser.backtrackable
        |> Span.parser Expr



-- EXPRESSION PARSERS: BLOCKS --------------------------------------------------


{-| -}
block : Config (Expr Span) -> Parser (Expr Span)
block config =
    Parser.succeed Block
        |. symbol "{"
        |. Parser.spaces
        |= Parser.loop []
            (\bindings ->
                Parser.oneOf
                    [ Parser.succeed (\b -> b :: bindings)
                        |= binding config
                        |. Parser.commit ()
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed ()
                        |> Parser.map (\_ -> List.reverse bindings)
                        |> Parser.map Parser.Done
                    ]
            )
        |. Parser.spaces
        |. keyword "ret"
        |. Parser.commit ()
        |= Pratt.subExpression 0 config
        |. Parser.spaces
        |. symbol "}"
        |> Parser.backtrackable
        |> Span.parser Expr


{-| -}
binding : Config (Expr Span) -> Parser ( String, Expr Span )
binding config =
    Parser.oneOf
        [ Parser.succeed (Tuple.pair "_")
            |. keyword "run"
            |. Parser.commit ()
            |. Parser.spaces
            |= Pratt.subExpression 0 config
            |> Parser.backtrackable
        , Parser.succeed Tuple.pair
            |. keyword "let"
            |. Parser.commit ()
            |. Parser.spaces
            |= lowercaseName keywords
            |. Parser.spaces
            |. symbol "="
            |. Parser.spaces
            |= Pratt.subExpression 0 config
            |> Parser.backtrackable
        ]



-- EXPRESSION PARSERS: CONDITIONALS --------------------------------------------


{-| -}
conditional : Config (Expr Span) -> Parser (Expr Span)
conditional config =
    Parser.succeed Conditional
        |. keyword "if"
        |. Parser.commit ()
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |. Parser.spaces
        |. keyword "then"
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |. Parser.spaces
        |. keyword "else"
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |> Parser.backtrackable
        |> Span.parser Expr



-- EXPRESSION PARSERS: IDENTIFIERS ---------------------------------------------


{-| -}
identifier : Parser (Expr Span)
identifier =
    Parser.succeed Identifier
        |= Parser.oneOf
            [ placeholder
            , local
            , scoped
            ]
        |> Span.parser Expr


{-| -}
placeholder : Parser Expr.Identifier
placeholder =
    Parser.succeed Expr.Placeholder
        |. symbol "_"
        |= Parser.oneOf
            [ Parser.succeed Just
                |= lowercaseName keywords
            , Parser.succeed Nothing
            ]


{-| -}
local : Parser Expr.Identifier
local =
    Parser.succeed Expr.Local
        |= lowercaseName keywords


{-| -}
scoped : Parser Expr.Identifier
scoped =
    Parser.succeed Expr.Scoped
        |= uppercaseName Set.empty
        |. symbol "."
        |= Parser.oneOf
            [ Parser.lazy (\_ -> scoped)
            , local
            ]



-- EXPRESSION PARSERS: LAMBDAS -------------------------------------------------


{-| -}
lambda : Config (Expr Span) -> Parser (Expr Span)
lambda config =
    Parser.succeed (\arg args body -> Lambda (arg :: args) body)
        |= pattern
        |. Parser.spaces
        |= Parser.loop []
            (\args ->
                Parser.oneOf
                    [ Parser.succeed (\arg -> arg :: args)
                        |= pattern
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed ()
                        |> Parser.map (\_ -> List.reverse args)
                        |> Parser.map Parser.Done
                    ]
            )
        |. Parser.spaces
        |. symbol "=>"
        |. Parser.commit ()
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |> Parser.backtrackable
        |> Span.parser Expr



-- EXPRESSION PARSERS: LITERALS ------------------------------------------------


{-| -}
literal : Config (Expr Span) -> Parser (Expr Span)
literal config =
    Parser.succeed Literal
        |= Parser.oneOf
            [ array config
            , boolean
            , number
            , record config
            , string
            , template config
            , undefined
            , variant
            ]
        |> Span.parser Expr


{-| -}
array : Config (Expr Span) -> Parser (Expr.Literal (Expr Span))
array config =
    Parser.succeed Expr.Array
        |= Parser.sequence
            { start = Parser.Token "[" <| ExpectingSymbol "["
            , separator = Parser.Token "," <| ExpectingSymbol ","
            , end = Parser.Token "]" <| ExpectingSymbol "]"
            , item = Pratt.subExpression 0 config
            , spaces = Parser.spaces
            , trailing = Parser.Forbidden
            }


{-| -}
boolean : Parser (Expr.Literal expr)
boolean =
    Parser.succeed Expr.Boolean
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.keyword (Parser.Token "true" <| ExpectingKeyword "true")
            , Parser.succeed False
                |. Parser.keyword (Parser.Token "false" <| ExpectingKeyword "false")
            ]


{-| -}
number : Parser (Expr.Literal expr)
number =
    let
        numberConfig =
            { int = Ok Basics.toFloat
            , hex = Ok Basics.toFloat
            , octal = Ok Basics.toFloat
            , binary = Ok Basics.toFloat
            , float = Ok identity
            , invalid = ExpectingNumber
            , expecting = ExpectingNumber
            }
    in
    Parser.succeed Expr.Number
        |= Parser.oneOf
            [ Parser.succeed Basics.negate
                |. Parser.symbol (Parser.Token "-" <| ExpectingSymbol "-")
                |= Parser.number numberConfig
            , Parser.number numberConfig
            ]
        |. Parser.oneOf
            [ Parser.chompIf Char.isAlpha ExpectingNumber
                |> Parser.andThen (\_ -> Parser.problem ExpectingNumber)
            , Parser.succeed ()
            ]


{-| -}
record : Config (Expr Span) -> Parser (Expr.Literal (Expr Span))
record config =
    Parser.succeed Expr.Record
        |= Parser.sequence
            { start = Parser.Token "{" <| ExpectingSymbol "{"
            , separator = Parser.Token "," <| ExpectingSymbol ","
            , end = Parser.Token "}" <| ExpectingSymbol "}"
            , item =
                Parser.oneOf
                    [ Parser.succeed Tuple.pair
                        |= lowercaseName keywords
                        |. Parser.spaces
                        |. Parser.symbol (Parser.Token ":" <| ExpectingSymbol ":")
                        |. Parser.commit ()
                        |. Parser.spaces
                        |= Pratt.subExpression 0 config
                        |> Parser.backtrackable

                    -- We support record literal shorthand like JavaScript that
                    -- lets you write `{ foo }` as a shorthand for writing
                    -- `{ foo: foo }`. Because our expressions are annotated with
                    -- their source Span, we do some gymnastics to get that
                    -- Span data before we construct the identifier.
                    , Parser.succeed (\start key end -> ( Span.fromTuples start end, key ))
                        |= Parser.getPosition
                        |= lowercaseName keywords
                        |= Parser.getPosition
                        |> Parser.map (\( loc, key ) -> ( key, Expr loc (Identifier (Expr.Local key)) ))
                    ]
            , spaces = Parser.spaces
            , trailing = Parser.Forbidden
            }
        |> Parser.backtrackable


{-| -}
string : Parser (Expr.Literal expr)
string =
    Parser.succeed Expr.String
        |= quotedString '"'


{-| -}
template : Config (Expr Span) -> Parser (Expr.Literal (Expr Span))
template config =
    let
        char =
            Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.token (Parser.Token "\\" <| ExpectingSymbol "\\")
                    |= Parser.oneOf
                        [ Parser.map (\_ -> '\\') (Parser.token (Parser.Token "\\" <| ExpectingSymbol "\\"))
                        , Parser.map (\_ -> '"') (Parser.token (Parser.Token "\"" <| ExpectingSymbol "\"")) -- " (elm-vscode workaround)
                        , Parser.map (\_ -> '\'') (Parser.token (Parser.Token "'" <| ExpectingSymbol "'"))
                        , Parser.map (\_ -> '\n') (Parser.token (Parser.Token "n" <| ExpectingSymbol "n"))
                        , Parser.map (\_ -> '\t') (Parser.token (Parser.Token "t" <| ExpectingSymbol "t"))
                        , Parser.map (\_ -> '\u{000D}') (Parser.token (Parser.Token "r" <| ExpectingSymbol "r"))
                        ]
                , Parser.token (Parser.Token "`" <| ExpectingSymbol "`")
                    |> Parser.andThen (\_ -> Parser.problem <| UnexpextedChar '`')
                , Parser.chompIf ((/=) '\n') ExpectingChar
                    |> Parser.getChompedString
                    |> Parser.andThen
                        (String.uncons
                            >> Maybe.map (Tuple.first >> Parser.succeed)
                            >> Maybe.withDefault (Parser.problem <| InternalError "Multiple characters chomped in `parseChar`")
                        )
                ]

        -- Each template segment should either be a String or an expression to
        -- be interpolated. Right now instead of String segments we have Char
        -- segments, so we use this function in a fold to join all the characters
        -- into a strings.
        joinSegments segment segments =
            case ( segment, segments ) of
                ( Data.Either.Left c, (Data.Either.Left s) :: rest ) ->
                    Data.Either.Left (String.cons c s) :: rest

                ( Data.Either.Left c, rest ) ->
                    Data.Either.Left (String.fromChar c) :: rest

                ( Data.Either.Right e, rest ) ->
                    Data.Either.Right e :: rest
    in
    Parser.succeed (List.foldl joinSegments [] >> Expr.Template)
        |. Parser.symbol (Parser.Token "`" <| ExpectingSymbol "`")
        |. Parser.commit ()
        |= Parser.loop []
            (\segments ->
                Parser.oneOf
                    [ Parser.succeed (\expr -> Data.Either.Right expr :: segments)
                        |. Parser.symbol (Parser.Token "${" <| ExpectingSymbol "${")
                        |= Pratt.subExpression 0 config
                        |. Parser.symbol (Parser.Token "}" <| ExpectingSymbol "}")
                        |> Parser.map Parser.Loop
                    , Parser.succeed (\c -> Data.Either.Left c :: segments)
                        |= char
                        |> Parser.backtrackable
                        |> Parser.map Parser.Loop
                    , Parser.succeed segments
                        |> Parser.map Parser.Done
                    ]
            )
        |. Parser.symbol (Parser.Token "`" <| ExpectingSymbol "`")


{-| -}
undefined : Parser (Expr.Literal expr)
undefined =
    Parser.succeed Expr.Undefined
        |. symbol "()"


{-| -}
variant : Parser (Expr.Literal (Expr Span))
variant =
    Parser.succeed Expr.Variant
        |. symbol "#"
        |= lowercaseName Set.empty
        |. Parser.spaces
        |= Parser.loop []
            (\exprs ->
                Parser.oneOf
                    [ Parser.succeed (\expr -> expr :: exprs)
                        |= parenthesised
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed ()
                        |> Parser.map (\_ -> List.reverse exprs)
                        |> Parser.map Parser.Done
                    ]
            )



-- EXPRESSION PARSERS: MATCHES -------------------------------------------------


{-| -}
match : Config (Expr Span) -> Parser (Expr Span)
match config =
    Parser.succeed Match
        |. keyword "where"
        |. Parser.commit ()
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |. Parser.spaces
        |= Parser.loop []
            (\cases ->
                Parser.oneOf
                    [ Parser.succeed (\pat guard body -> ( pat, guard, body ) :: cases)
                        |. keyword "is"
                        |. Parser.spaces
                        |= pattern
                        |. Parser.spaces
                        |= Parser.oneOf
                            [ Parser.succeed Just
                                |. keyword "if"
                                |. Parser.spaces
                                |= prattExpression
                                    -- These parsers start with a keyword
                                    [ conditional
                                    , match

                                    -- These parsers parse sub expressions
                                    --, annotation
                                    , application
                                    , Pratt.literal access

                                    --
                                    , block
                                    , Pratt.literal identifier
                                    , literal

                                    -- Subexpressions are wrapped in parentheses.
                                    , Pratt.literal (Parser.lazy (\_ -> subexpression))
                                    ]
                            , Parser.succeed Nothing
                            ]
                        |. Parser.spaces
                        |. symbol "=>"
                        |. Parser.spaces
                        |= Pratt.subExpression 0 config
                        |> Parser.map Parser.Loop
                    , Parser.succeed ()
                        |> Parser.map (\_ -> List.reverse cases)
                        |> Parser.map Parser.Done
                    ]
            )
        |> Parser.backtrackable
        |> Span.parser Expr



--                                                                            --
-- PATTERN PARSERS -------------------------------------------------------------
--                                                                            --


{-| -}
pattern : Parser Expr.Pattern
pattern =
    Parser.oneOf
        [ arrayDestructure
        , literalPattern
        , name
        , recordDestructure
        , templateDestructure
        , typeof
        , variantDestructure
        , wildcard
        ]


{-| -}
arrayDestructure : Parser Expr.Pattern
arrayDestructure =
    Parser.succeed Expr.ArrayDestructure
        |. symbol "["
        |. Parser.commit ()
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed (::)
                |= Parser.lazy (\_ -> pattern)
                |. Parser.spaces
                |= Parser.loop []
                    (\patterns ->
                        Parser.oneOf
                            [ Parser.succeed (\pat -> pat :: patterns)
                                |. symbol ","
                                |. Parser.spaces
                                |= Parser.lazy (\_ -> pattern)
                                |> Parser.backtrackable
                                |> Parser.map Parser.Loop
                            , Parser.succeed (\pat -> pat :: patterns)
                                |. symbol ","
                                |. Parser.spaces
                                |= spread
                                |> Parser.map List.reverse
                                |> Parser.map Parser.Done
                            , Parser.succeed ()
                                |> Parser.map (\_ -> List.reverse patterns)
                                |> Parser.map Parser.Done
                            ]
                    )
            , Parser.succeed []
            ]
        |. Parser.spaces
        |. symbol "]"
        |> Parser.backtrackable


{-| -}
literalPattern : Parser Expr.Pattern
literalPattern =
    Parser.succeed Expr.LiteralPattern
        |= Parser.oneOf
            [ boolean
            , number
            , string
            , undefined
            ]


{-| -}
name : Parser Expr.Pattern
name =
    Parser.succeed Expr.Name
        |= lowercaseName keywords


{-| -}
recordDestructure : Parser Expr.Pattern
recordDestructure =
    let
        keyAndPattern =
            Parser.succeed Tuple.pair
                |= lowercaseName keywords
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed Just
                        |. symbol ":"
                        |. Parser.spaces
                        |= Parser.lazy (\_ -> pattern)
                    , Parser.succeed Nothing
                    ]
    in
    Parser.succeed Expr.RecordDestructure
        |. symbol "{"
        |. Parser.commit ()
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed (::)
                |= keyAndPattern
                |. Parser.spaces
                |= Parser.loop []
                    (\patterns ->
                        Parser.oneOf
                            [ Parser.succeed (\pat -> pat :: patterns)
                                |. symbol ","
                                |. Parser.spaces
                                |= keyAndPattern
                                |> Parser.backtrackable
                                |> Parser.map Parser.Loop
                            , Parser.succeed Basics.identity
                                |. symbol ","
                                |. Parser.spaces
                                |= spread
                                -- We need to get the name of the binding
                                -- introduced by the spread pattern so we can
                                -- store it in the pattern list with a "key".
                                |> Parser.andThen
                                    (\pat ->
                                        case pat of
                                            Expr.Spread key ->
                                                (( key, Just pat ) :: patterns)
                                                    |> Parser.succeed

                                            _ ->
                                                InternalError ""
                                                    |> Parser.problem
                                    )
                                |> Parser.map List.reverse
                                |> Parser.map Parser.Done
                            , Parser.succeed ()
                                |> Parser.map (\_ -> List.reverse patterns)
                                |> Parser.map Parser.Done
                            ]
                    )
            , Parser.succeed []
            ]
        |. Parser.spaces
        |. symbol "}"
        |> Parser.backtrackable


{-| -}
spread : Parser Expr.Pattern
spread =
    Parser.succeed Expr.Spread
        |. symbol "..."
        |. Parser.commit ()
        |= lowercaseName keywords
        |> Parser.backtrackable


{-| -}
templateDestructure : Parser Expr.Pattern
templateDestructure =
    let
        char =
            Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.token (Parser.Token "\\" <| ExpectingSymbol "\\")
                    |= Parser.oneOf
                        [ Parser.map (\_ -> '\\') (Parser.token (Parser.Token "\\" <| ExpectingSymbol "\\"))
                        , Parser.map (\_ -> '"') (Parser.token (Parser.Token "\"" <| ExpectingSymbol "\"")) -- " (elm-vscode workaround)
                        , Parser.map (\_ -> '\'') (Parser.token (Parser.Token "'" <| ExpectingSymbol "'"))
                        , Parser.map (\_ -> '\n') (Parser.token (Parser.Token "n" <| ExpectingSymbol "n"))
                        , Parser.map (\_ -> '\t') (Parser.token (Parser.Token "t" <| ExpectingSymbol "t"))
                        , Parser.map (\_ -> '\u{000D}') (Parser.token (Parser.Token "r" <| ExpectingSymbol "r"))
                        ]
                , Parser.token (Parser.Token "`" <| ExpectingSymbol "`")
                    |> Parser.andThen (\_ -> Parser.problem <| UnexpextedChar '`')
                , Parser.chompIf ((/=) '\n') ExpectingChar
                    |> Parser.getChompedString
                    |> Parser.andThen
                        (String.uncons
                            >> Maybe.map (Tuple.first >> Parser.succeed)
                            >> Maybe.withDefault (Parser.problem <| InternalError "Multiple characters chomped in `parseChar`")
                        )
                ]

        -- Each template segment should either be a String or an expression to
        -- be interpolated. Right now instead of String segments we have Char
        -- segments, so we use this function in a fold to join all the characters
        -- into a strings.
        joinSegments segment segments =
            case ( segment, segments ) of
                ( Data.Either.Left c, (Data.Either.Left s) :: rest ) ->
                    Data.Either.Left (String.cons c s) :: rest

                ( Data.Either.Left c, rest ) ->
                    Data.Either.Left (String.fromChar c) :: rest

                ( Data.Either.Right e, rest ) ->
                    Data.Either.Right e :: rest
    in
    Parser.succeed (List.foldl joinSegments [] >> Expr.TemplateDestructure)
        |. symbol "`"
        |. Parser.commit ()
        |= Parser.loop []
            (\segments ->
                Parser.oneOf
                    [ Parser.succeed (\expr -> Data.Either.Right expr :: segments)
                        |. symbol "${"
                        |= Parser.lazy (\_ -> pattern)
                        |. symbol "}"
                        |> Parser.map Parser.Loop
                    , Parser.succeed (\c -> Data.Either.Left c :: segments)
                        |= char
                        |> Parser.backtrackable
                        |> Parser.map Parser.Loop
                    , Parser.succeed segments
                        |> Parser.map Parser.Done
                    ]
            )
        |. symbol "`"
        |> Parser.backtrackable


{-| -}
typeof : Parser Expr.Pattern
typeof =
    Parser.succeed Expr.Typeof
        |. symbol "@"
        |. Parser.commit ()
        |= uppercaseName Set.empty
        |. Parser.spaces
        |= Parser.lazy (\_ -> pattern)
        |> Parser.backtrackable


{-| -}
variantDestructure : Parser Expr.Pattern
variantDestructure =
    Parser.succeed Expr.VariantDestructure
        |. symbol "#"
        |. Parser.commit ()
        |= lowercaseName Set.empty
        |. Parser.spaces
        |= Parser.loop []
            (\patterns ->
                Parser.oneOf
                    [ Parser.succeed (\pat -> pat :: patterns)
                        |= Parser.lazy (\_ -> pattern)
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed ()
                        |> Parser.map (\_ -> List.reverse patterns)
                        |> Parser.map Parser.Done
                    ]
            )
        |> Parser.backtrackable


{-| -}
wildcard : Parser Expr.Pattern
wildcard =
    Parser.succeed Expr.Wildcard
        |. symbol "_"
        |= Parser.oneOf
            [ lowercaseName Set.empty
                |> Parser.map Just
            , Parser.succeed Nothing
            ]



--                                                                            --
-- TYPE PARSERS ----------------------------------------------------------------
--                                                                            --


type_ : Parser Type
type_ =
    Parser.oneOf
        [ fun
        , app
        , var
        , con
        , any
        , rec
        , hole
        , Parser.lazy (\_ -> subtype)
        ]


subtype : Parser Type
subtype =
    Parser.succeed identity
        |. symbol "("
        |. Parser.spaces
        |= Parser.lazy (\_ -> type_)
        |. Parser.spaces
        |. symbol ")"
        |> Parser.backtrackable


var : Parser Type
var =
    Parser.succeed Type.Var
        |= lowercaseName keywords


con : Parser Type
con =
    Parser.oneOf
        [ Parser.succeed Type.Con
            |= uppercaseName Set.empty
        , Parser.succeed (Type.Con "()")
            |. Parser.symbol (Parser.Token "()" <| ExpectingSymbol "()")
        ]


app : Parser Type
app =
    let
        typeWithoutApp =
            Parser.oneOf
                [ subtype
                , var
                , con
                , any
                , hole
                ]
    in
    Parser.succeed (\con_ arg args -> Type.App con_ (arg :: args))
        |= typeWithoutApp
        |. Parser.spaces
        |= typeWithoutApp
        |. Parser.spaces
        |= Parser.loop []
            (\args ->
                Parser.oneOf
                    [ Parser.succeed (\arg -> arg :: args)
                        |= typeWithoutApp
                        |. Parser.spaces
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse args)
                        |> Parser.map Parser.Done
                    ]
            )
        |> Parser.backtrackable


fun : Parser Type
fun =
    let
        typeWithoutFun =
            Parser.oneOf
                [ subtype
                , any
                , var
                , con
                , app
                , rec
                , hole
                ]
    in
    Parser.succeed Type.Fun
        |= typeWithoutFun
        |. Parser.spaces
        |. Parser.oneOf
            [ symbol "->"
            , symbol "→"
            ]
        |. Parser.spaces
        |= Parser.lazy (\_ -> type_)
        |> Parser.backtrackable


rec : Parser Type
rec =
    Parser.succeed (Type.Rec << Dict.fromList)
        |= Parser.sequence
            { start = Parser.Token "{" (ExpectingSymbol "{")
            , separator = Parser.Token "," (ExpectingSymbol ",")
            , end = Parser.Token "}" (ExpectingSymbol "}")
            , spaces = Parser.spaces
            , item =
                Parser.succeed Tuple.pair
                    |= lowercaseName keywords
                    |. Parser.spaces
                    |. symbol ":"
                    |. Parser.spaces
                    |= Parser.lazy (\_ -> type_)
            , trailing = Parser.Forbidden
            }


any : Parser Type
any =
    Parser.succeed Type.Any
        |. symbol "*"


hole : Parser Type
hole =
    Parser.succeed Type.Hole
        |. symbol "?"



--                                                                            --
-- UTILITIES -------------------------------------------------------------------
--                                                                            --


{-| -}
lowercaseName : Set String -> Parser String
lowercaseName reserved =
    Parser.variable
        { expecting = ExpectingCamelCase
        , start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = reserved
        }


{-| -}
uppercaseName : Set String -> Parser String
uppercaseName reserved =
    Parser.variable
        { expecting = ExpectingCapitalCase
        , start = Char.isUpper
        , inner = Char.isAlphaNum
        , reserved = reserved
        }


{-| -}
symbol : String -> Parser ()
symbol s =
    Parser.symbol (Parser.Token s <| ExpectingSymbol s)


{-| -}
keyword : String -> Parser ()
keyword s =
    Parser.keyword (Parser.Token s <| ExpectingKeyword s)


{-| -}
operator : String -> Parser ()
operator s =
    Parser.symbol (Parser.Token s <| ExpectingOperator s)


{-| -}
quotedString : Char -> Parser String
quotedString quote =
    let
        s =
            String.fromChar quote

        char =
            Parser.oneOf
                [ Parser.succeed identity
                    |. symbol s
                    |= Parser.oneOf
                        [ Parser.map (\_ -> '\\') (symbol "\\")
                        , Parser.map (\_ -> '"') (symbol "\"") -- " (elm-vscode workaround)
                        ]
                , symbol s
                    |> Parser.andThen (\_ -> Parser.problem <| UnexpextedChar quote)
                , Parser.chompIf ((/=) '\n') ExpectingChar
                    |> Parser.getChompedString
                    |> Parser.andThen
                        (String.uncons
                            >> Maybe.map (Tuple.first >> Parser.succeed)
                            >> Maybe.withDefault (Parser.problem <| InternalError "Multiple characters chomped in `character`")
                        )
                ]
    in
    Parser.succeed String.fromList
        |. symbol s
        |= Parser.loop []
            (\cs ->
                Parser.oneOf
                    [ Parser.succeed (\c -> c :: cs)
                        |= char
                        |> Parser.backtrackable
                        |> Parser.map Parser.Loop
                    , Parser.succeed (List.reverse cs)
                        |> Parser.map Parser.Done
                    ]
            )
        |. symbol s


{-| -}
keywords : Set String
keywords =
    Set.fromList <|
        List.concat
            -- Imports
            [ [ "import", "as", "exposing" ]

            -- Declarations
            , [ "pub", "extern" ]

            -- Bindings
            , [ "fun", "let", "ret" ]

            -- Conditionals
            , [ "if", "then", "else" ]

            -- Pattern matching
            , [ "where", "is" ]

            -- Literals
            , [ "true", "false" ]
            ]
