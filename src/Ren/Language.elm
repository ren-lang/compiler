module Ren.Language exposing
    ( keywords
    , Module, Import, Visibility(..)
    , Declaration(..), Variant(..)
    , Expression(..), Accessor(..), Identifier(..), Literal(..), TemplateSegment(..), Operator(..)
    , Pattern(..), PrimitiveType(..)
    )

{-|

@docs keywords
@docs Module, Import, Visibility
@docs Declaration, Variant
@docs Expression, Accessor, Identifier, Literal, TemplateSegment, Operator
@docs Pattern, PrimitiveType

-}

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Set exposing (Set)



-- CONSTANTS -------------------------------------------------------------------


{-|

    Set.fromList
        [ "if"
        , "then"
        , "else"
        , "import"
        , "as"
        , "exposing"
        , "pub"
        , "fun"
        , "let"
        , "enum"
        , "ret"
        , "true"
        , "false"
        , "when"
        , "is"
        ]

-}
keywords : Set String
keywords =
    Set.fromList
        [ "if"
        , "then"
        , "else"
        , "import"
        , "as"
        , "exposing"
        , "pub"
        , "fun"
        , "let"
        , "enum"
        , "ret"
        , "true"
        , "false"
        , "when"
        , "is"
        ]



-- MODULE TYPES ----------------------------------------------------------------


{-| -}
type alias Module =
    { imports : List Import
    , declarations : List ( Visibility, Declaration )
    }


{-| -}
type alias Import =
    { path : String
    , name : List String
    , bindings : List String
    }


{-| -}
type Visibility
    = Public
    | Private



-- DECLARATION TYPES -----------------------------------------------------------


{-| -}
type Declaration
    = Function String (List Pattern) Expression
    | Variable Pattern Expression
    | Enum String (List Variant)


{-| -}
type Variant
    = Variant String Int



-- EXPRESSION TYPES ------------------------------------------------------------


{-| -}
type Expression
    = Access Expression (List Accessor)
    | Application Expression (List Expression)
    | Block (List Declaration) Expression
    | Conditional Expression Expression Expression
    | Identifier Identifier
    | Infix Operator Expression Expression
    | Lambda (List Pattern) Expression
    | Literal Literal
    | Match Expression (List ( Pattern, Maybe Expression, Expression ))
    | SubExpression Expression


{-| -}
type Accessor
    = Computed Expression
    | Fixed String


{-| -}
type Identifier
    = Local String
    | Constructor String
    | Scoped (List String) Identifier
    | Operator Operator
    | Field String


{-| -}
type Literal
    = Array (List Expression)
    | Boolean Bool
    | Number Float
    | Object (Dict String Expression)
    | String String
    | Template (List TemplateSegment)
    | Undefined


{-| -}
type TemplateSegment
    = Text String
    | Expr Expression


{-| -}
type Operator
    = Pipe
    | Compose
      -- MATHS
    | Add
    | Sub
    | Mul
    | Div
    | Pow
    | Mod
      -- COMPARISON
    | Eq
    | NotEq
    | Lt
    | Lte
    | Gt
    | Gte
      -- LOGIC
    | And
    | Or
      -- ARRAYS
    | Cons
    | Join


{-| -}
type Pattern
    = ArrayDestructure (List Pattern)
    | Name String
    | ObjectDestructure (List ( String, Maybe Pattern ))
    | Value Literal
    | VariantDestructure String (List Pattern)
    | Typeof PrimitiveType String
    | Wildcard (Maybe String)


{-| -}
type PrimitiveType
    = BooleanP
    | NumberP
    | StringP
    | FunctionP
