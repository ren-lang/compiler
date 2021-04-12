module Cherry.Data.AST exposing 
    ( Module, Import
    , Declaration(..), Visibility(..), Binding
    , Expression(..), Accessor(..), Identifier(..), Operator(..), Pattern(..), Literal(..)
    )


-- MODULE TYPES ----------------------------------------------------------------


{-| -}
type alias Module =
    { imports : List Import
    , declarations : List Declaration
    }

{-| -}
type alias Import =
    { path : String
    , name : List String
    , exposedBindings : List String
    }


-- DECLARATION TYPES -----------------------------------------------------------


{-| -}
type Declaration
    = Fun Visibility String (List Pattern) Expression (List Binding)
    | Let Visibility String Expression (List Binding)

{-| -}
type Visibility
    = Public
    | Private

{-| -}
type alias Binding =
    { name : Pattern
    , body : Expression
    }


-- EXPRESSION TYPES ------------------------------------------------------------


{-| -}
type Expression
    = Access Expression (List Accessor)
    | Application Expression (List Expression)
    | Identifier Identifier
    | Infix Operator Expression Expression
    | Conditional Expression Expression Expression
    | Lambda (List Pattern) Expression
    | Literal Literal

{-| -}
type Accessor
    = Computed Expression
    | Fixed String

{-| -}
type Identifier
    = Local String
    | Scoped (List String) String
    | Operator Operator
    | ObjectField String

{-| -}
type Operator
    = Pipe | Compose | Discard
    -- MATHS
    | Add | Sub | Mul | Div | Pow | Mod
    -- COMPARISON
    | Eq | NotEq | Lt | Lte | Gt | Gte
    -- LOGIC
    | And | Or
    -- ARRAYS
    | Cons | Join

{-| -}
type Pattern
    = ArrayDestructure (List Pattern)
    | Name String
    | ObjectDestructure (List ( String, Maybe Pattern ))
    | Value Literal
    | Wildcard (Maybe String)

{-| -}
type Literal
    = Array (List Expression)
    | Boolean Bool
    | Number Float
    | Object (List ( String, Expression ))
    | String String
