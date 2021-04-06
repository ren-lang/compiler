module Cherry.AST.Expression exposing 
    ( Expression(..)
    , Accessor(..), Operator(..), Literal(..), Variable(..)
    )


-- TYPES -----------------------------------------------------------------------


{-| -}
type Expression
    = Access Expression Accessor
    | Application Expression Expression
    | InfixOp Operator Expression Expression
    | Conditional Expression Expression Expression
    | Lambda (List Variable) Expression
    | Literal Literal
    | Variable Variable

{-| -}
type Accessor
    = Computed Expression
    | Fixed String

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
type Literal
    = Array (List Expression)
    | Boolean Bool
    | Number Float
    | Object (List ( String, Expression ))
    | String String

{-| -}
type Variable
    = ArrayDestructure (List Variable)
    | Local String
    | ObjectDestructure (List Variable)
    | Operator Operator
    | Scoped (List String) String
