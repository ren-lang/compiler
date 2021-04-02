module Data.Parser 
    ( Parser
    , Error(..)

    -- RUNNING PARSERS
    , run

    -- IGNORING INPUT
    , succeed
    , fail
    , failWith

    -- PRIMITIVE PARSERS
    , any
    , eof
    , string
    , spaces
    , whitespace

    -- COMBINATORS
    , map
    , map2
    , bind
    , lazy
    , alt
    , oneOf

    -- CHAINING PARSERS
    , apply, (|=)
    , const, (|.)
    ) where


import Control.Alt (class Alt, (<|>))
import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Functor (class Functor, (<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Prelude (($), (==), (||), Unit, unit)
import Prim (Array, Boolean, Char, String)


-- TYPES -----------------------------------------------------------------------


-- |
newtype Parser a = 
    Parser (String -> Either Error (Tuple String a))

-- |
data Error
    = BadParser String
    | Custom String
    | EOF
    | Expected String String
    | InvalidInput String


-- RUNNING PARSERS -------------------------------------------------------------


-- |
run :: forall a. Parser a -> String -> Either Error a
run (Parser parse) input =
    Tuple.snd <$> parse input

--
runwrap :: forall a. Parser a -> String -> Either Error (Tuple String a)
runwrap (Parser parse) input =
    parse input


-- IGNORING INPUT --------------------------------------------------------------


-- |
succeed :: forall a. a -> Parser a
succeed value =
    Parser $ \input -> 
        Right  $ Tuple input value

-- |
fail :: forall a. String -> Parser a 
fail message =
    Parser $ \_ ->
        Left $ Custom message 

-- |
failWith :: forall a. Error -> Parser a 
failWith error =
    Parser $ \_ ->
        Left error


-- PRIMITIVE PARSERS -----------------------------------------------------------


-- |
any :: Parser Char
any =
    Parser $ \input ->
        case String.CodeUnits.uncons input of 
            Just { head, tail } ->
                Right $ Tuple tail head
            
            Nothing ->
                Left EOF

-- |
eof :: Parser Unit
eof =
    Parser $ \input ->
        if String.null input then
            Right $ Tuple "" unit

        else
            Left $ InvalidInput input

-- |
string :: String -> Parser String
string value =
    Parser $ \input ->
        case String.stripPrefix (String.Pattern value) input of
            Just rest ->
                Right $ Tuple rest value

            Nothing ->
                Left $ Expected value $ String.take (String.length value) input

-- |
spaces :: Parser Unit
spaces =
    (\_ -> unit) <$> takeWhile (\c -> c == ' ')

-- |
whitespace :: Parser Unit
whitespace =
    (\_ -> unit) <$> takeWhile (\c -> c == ' ' || c == '\t' || c == '\n')


-- COMBINATORS -----------------------------------------------------------------


-- |
map :: forall a b. (a -> b) -> Parser a -> Parser b
map f (Parser parse) =
    Parser $ \input ->
        case parse input of
            Right (Tuple rest value) ->
                Right $ Tuple rest (f value)

            Left error ->
                Left error

-- |
map2 :: forall a b c. (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f parseA parseB =
    Parser $ \input ->
        case runwrap parseA input of
            Right (Tuple input' a) ->
                case runwrap parseB input' of
                    Right (Tuple rest b) ->
                        Right $ Tuple rest (f a b)

                    Left error ->
                        Left error
            
            Left error ->
                Left error

-- |
bind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
bind parseA f =
    Parser $ \input ->
        case runwrap parseA input of
            Right (Tuple rest value) ->
                runwrap (f value) rest

            Left error ->
                Left error

-- |
lazy :: forall a. (Unit -> Parser a) -> Parser a
lazy parse =
    Parser $ \input ->
        runwrap (parse unit) input

-- |
alt :: forall a. Parser a -> Parser a -> Parser a
alt parseA parseB =
    Parser $ \input ->
        runwrap parseA input <|> runwrap parseB input

-- |
oneOf :: forall a. Array (Parser a) -> Parser a
oneOf parsers =
    case parsers of
        [] -> 
            failWith $ BadParser "There must be at least one parser in the array given to `oneOf`."

        [ a ] ->
            a

        [ a, b ] ->
            a <|> b

        _ ->
            (oneOf $ Array.take 2 parsers) <|> (oneOf $ Array.drop 2 parsers)


-- PREDICATE PARSERS -----------------------------------------------------------


-- |
takeIf :: (Char -> Boolean) -> Parser Char
takeIf predicate =
    Parser $ \input ->
        case String.CodeUnits.uncons input of
            Just { head, tail } ->
                if predicate head then
                    Right $ Tuple tail head

                else
                    Left $ InvalidInput (String.CodeUnits.singleton head)
            
            Nothing ->
                Left EOF

-- |
takeWhile :: (Char -> Boolean) -> Parser String
takeWhile predicate =
    Parser $ \input ->
        case String.CodeUnits.uncons input of
            Just { head, tail } ->
                if predicate head then
                    runwrap (takeWhile' predicate head) tail

                else
                    Right $ Tuple tail ""
            
            Nothing ->
                Left EOF

takeWhile' :: (Char -> Boolean) -> Char -> Parser String
takeWhile' predicate c =
    (\s -> String.CodeUnits.singleton c <> s) <$> takeWhile predicate

-- |
takeIfAndWhile :: (Char -> Boolean) -> Parser String
takeIfAndWhile predicate =
    succeed cons
        |= takeIf predicate
        |= takeWhile predicate
    where
        cons c s = String.CodeUnits.singleton c <> s


-- CHAINING PARSERS ------------------------------------------------------------


-- |
apply :: forall a b. Parser (a -> b) -> Parser a -> Parser b
apply parseF parseA =
    map2 (\f a -> f a) parseF parseA

infixl 5 apply as |=

-- |
const :: forall a b. Parser a -> Parser b -> Parser a
const parseA parseB =
    map2 (\a b -> a) parseA parseB

infixl 6 const as |.


-- INSTANCES -------------------------------------------------------------------


instance altParser :: Alt Parser where
    alt = alt

instance applicateParser :: Applicative Parser where
    pure = succeed

instance applyParser :: Apply Parser where
    apply = apply

instance bindParser :: Bind Parser where
    bind = bind

instance functorParser :: Functor Parser where
    map = map 

instance monadParser :: Monad Parser
