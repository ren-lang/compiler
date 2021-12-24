module Ren.Compiler.Parse exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Data.Either
import Parser.Advanced as Parser exposing ((|.), (|=))
import Pratt.Advanced as Pratt
import Ren.AST.Expr as Expr exposing (Expr(..), ExprF(..))
import Set exposing (Set)


run : String -> Result (List (Parser.DeadEnd Context Error)) (Expr Location)
run =
    Parser.run expression



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Parser a =
    Parser.Parser Context Error a


{-| -}
type alias Config a =
    Pratt.Config Context Error a


{-| -}
type Context
    = InExpr


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


{-| -}
type alias Location =
    { start : ( Int, Int )
    , end : ( Int, Int )
    }



--


{-| Takes a parser for an unwrapped expression and then attaches some location
metadata to it.
-}
locateExpr : Parser (ExprF (Expr Location)) -> Parser (Expr Location)
locateExpr parser =
    Parser.succeed (\start expr end -> Expr (Location start end) expr)
        |= Parser.getPosition
        |= parser
        |= Parser.getPosition



--                                                                            --
-- EXPRESSION PARSERS ----------------------------------------------------------
--                                                                            --


expression : Parser (Expr Location)
expression =
    Pratt.expression
        { oneOf =
            -- These parsers start with a keyword
            [ lambda
            , conditional
            , match

            -- These parsers parse sub expressions
            --, annotation
            , application
            , Pratt.literal access

            --
            , block
            , literal
            , Pratt.literal (Parser.lazy (\_ -> subexpression))
            , Pratt.literal identifier
            ]
        , andThenOneOf =
            let
                -- This cursed dummy location is necessary because we need the
                -- type of the infix expression to be the same as its sub-expressions
                -- but we can't get the location start/end of the parser until
                -- we've created the parser...
                --
                -- It's cursed but it works.
                dummyLocation =
                    Location ( 0, 0 ) ( 0, 0 )

                -- Helper parser for handling infix operator parsing. Takes the
                -- required symbol as a string helpfully wraps it up in a the
                -- `Parser.symbol` parser.
                infix_ parser precedence sym op =
                    Tuple.mapSecond locateInfix
                        << parser precedence
                            (operator sym)
                            (\lhs rhs -> Expr.wrap dummyLocation (Infix op lhs rhs))

                -- This annotates a parsed infix expression with start/end location
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
                                    Expr (Location start end) e
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


subexpression : Parser (Expr Location)
subexpression =
    Parser.succeed identity
        |. symbol "("
        |. Parser.spaces
        |= expression
        |. Parser.spaces
        |. symbol ")"


parenthesised : Parser (Expr Location)
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
                        |> locateExpr
                , Pratt.literal identifier
                ]
            , andThenOneOf = []
            , spaces = Parser.spaces
            }
            |> Parser.inContext InExpr
        , Parser.lazy (\_ -> subexpression)
        ]



-- EXPRESSION PARSERS: ACCESSORS -----------------------------------------------


access : Parser (Expr Location)
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
        |> locateExpr



-- EXPRESSION PARSERS: APPLICATION ---------------------------------------------


application : Config (Expr Location) -> Parser (Expr Location)
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
        |> locateExpr



-- EXPRESSION PARSERS: BLOCKS --------------------------------------------------


block : Config (Expr Location) -> Parser (Expr Location)
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
        |> locateExpr


binding : Config (Expr Location) -> Parser ( String, Expr Location )
binding config =
    Parser.succeed Tuple.pair
        |. keyword "let"
        |. Parser.commit ()
        |. Parser.spaces
        |= lowercaseName keywords
        |. Parser.spaces
        |. symbol "="
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |> Parser.backtrackable



-- EXPRESSION PARSERS: CONDITIONALS --------------------------------------------


conditional : Config (Expr Location) -> Parser (Expr Location)
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
        |> locateExpr



-- EXPRESSION PARSERS: IDENTIFIERS ---------------------------------------------


identifier : Parser (Expr Location)
identifier =
    Parser.succeed Identifier
        |= Parser.oneOf
            [ placeholder
            , local
            , scoped
            ]
        |> locateExpr


placeholder : Parser Expr.Identifier
placeholder =
    Parser.succeed Expr.Placeholder
        |. symbol "_"
        |= Parser.oneOf
            [ Parser.succeed Just
                |= lowercaseName keywords
            , Parser.succeed Nothing
            ]


local : Parser Expr.Identifier
local =
    Parser.succeed Expr.Local
        |= lowercaseName keywords


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


lambda : Config (Expr Location) -> Parser (Expr Location)
lambda config =
    Parser.succeed (\arg args body -> Lambda (arg :: args) body)
        |. keyword "fun"
        |. Parser.commit ()
        |. Parser.spaces
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
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |> Parser.backtrackable
        |> locateExpr



-- EXPRESSION PARSERS: LITERALS ------------------------------------------------


literal : Config (Expr Location) -> Parser (Expr Location)
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
        |> locateExpr


array : Config (Expr Location) -> Parser (Expr.Literal (Expr Location))
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


boolean : Parser (Expr.Literal expr)
boolean =
    Parser.succeed Expr.Boolean
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.keyword (Parser.Token "true" <| ExpectingKeyword "true")
            , Parser.succeed False
                |. Parser.keyword (Parser.Token "false" <| ExpectingKeyword "false")
            ]


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


record : Config (Expr Location) -> Parser (Expr.Literal (Expr Location))
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
                    -- their source location, we do some gymnastics to get that
                    -- location data before we construct the identifier.
                    , Parser.succeed (\start key end -> ( Location start end, key ))
                        |= Parser.getPosition
                        |= lowercaseName keywords
                        |= Parser.getPosition
                        |> Parser.map (\( loc, key ) -> ( key, Expr loc (Identifier (Expr.Local key)) ))
                    ]
            , spaces = Parser.spaces
            , trailing = Parser.Forbidden
            }
        |> Parser.backtrackable


string : Parser (Expr.Literal expr)
string =
    Parser.succeed Expr.String
        |= quotedString '"'


template : Config (Expr Location) -> Parser (Expr.Literal (Expr Location))
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


undefined : Parser (Expr.Literal expr)
undefined =
    Parser.succeed Expr.Undefined
        |. symbol "()"


variant : Parser (Expr.Literal (Expr Location))
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


match : Config (Expr Location) -> Parser (Expr Location)
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
                                |= Pratt.subExpression 0 config
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
        |> locateExpr



--                                                                            --
-- PATTERN PARSERS -------------------------------------------------------------
--                                                                            --


pattern : Parser Expr.Pattern
pattern =
    Parser.oneOf
        [ arrayDestructure
        , literalPattern
        , name
        , recordDestructure
        , typeof
        , variantDestructure
        , wildcard
        ]


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


literalPattern : Parser Expr.Pattern
literalPattern =
    Parser.succeed Expr.LiteralPattern
        |= Parser.oneOf
            [ boolean
            , number
            , string
            , undefined
            ]


name : Parser Expr.Pattern
name =
    Parser.succeed Expr.Name
        |= lowercaseName keywords


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


spread : Parser Expr.Pattern
spread =
    Parser.succeed Expr.Spread
        |. symbol "..."
        |. Parser.commit ()
        |= lowercaseName keywords
        |> Parser.backtrackable


typeof : Parser Expr.Pattern
typeof =
    Parser.succeed Expr.Typeof
        |. symbol "@"
        |. Parser.commit ()
        |= uppercaseName Set.empty
        |. Parser.spaces
        |= Parser.lazy (\_ -> pattern)
        |> Parser.backtrackable


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
-- UTILITIES -------------------------------------------------------------------
--                                                                            --


lowercaseName : Set String -> Parser String
lowercaseName reserved =
    Parser.variable
        { expecting = ExpectingCamelCase
        , start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = reserved
        }


uppercaseName : Set String -> Parser String
uppercaseName reserved =
    Parser.variable
        { expecting = ExpectingCapitalCase
        , start = Char.isUpper
        , inner = Char.isAlphaNum
        , reserved = reserved
        }


symbol : String -> Parser ()
symbol s =
    Parser.symbol (Parser.Token s <| ExpectingSymbol s)


keyword : String -> Parser ()
keyword s =
    Parser.keyword (Parser.Token s <| ExpectingKeyword s)


operator : String -> Parser ()
operator s =
    Parser.symbol (Parser.Token s <| ExpectingOperator s)


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
                        , Parser.map (\_ -> '\'') (symbol "'")
                        , Parser.map (\_ -> '\n') (symbol "n")
                        , Parser.map (\_ -> '\t') (symbol "t")
                        , Parser.map (\_ -> '\u{000D}') (symbol "r")
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


keywords : Set String
keywords =
    Set.fromList <|
        List.concat
            -- Bindings
            [ [ "fun", "let" ]

            -- Conditionals
            , [ "if", "then", "else" ]

            -- Pattern matching
            , [ "where", "is" ]

            -- Literals
            , [ "true", "false" ]

            -- Imports
            , [ "import", "as", "exposing" ]

            -- Declarations
            , [ "pub", "extern" ]
            ]
