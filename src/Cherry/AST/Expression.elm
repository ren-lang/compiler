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

{-| Accessors encompass the different ways we might acccess the fields or
properties of an object.

The computed accessor evaluates an expression and uses its result as the key to
access an object with:

    obj['user-' + 5]

    obj[keyFrom foo]

The fixed accessor accessess an object using a literal key. That key must follow
the same rules as a valid variable binding (eg starts witha lowercase, can only
contain alpha-numeric characters):

    obj.user5

    obj.user-5

Note that the second example would subtract 5 from whatever `obj.user` evaluated
too, unlike the computed accessor that would access the object using the key
'user-5'.
-}
type Accessor
    = Computed Expression
    | Fixed String

{-| Unlike some other languages, operators are fixed and known at compile-time
in Cherry. This allows us to perform some optimisations, like removing the pipe
operator altogether. Here's a list of what symbol(s) each operator is associated
with and what they do. The JS strict equality operator (===) is used to mark "is
the same as" and distinguish it from Cherry's equality operator (==). 

    Pipe (|>): Pass a value into a function
        x |> f === f x

    Compose (>>): Create a new function by calling two functions in sequence
        f >> g === fun x => g (f x)

    Discard (;): Evaluate the left side, ignore the result, and evalue the right
        x; y === x |> fun _ => y

    ... You know what maths, comparison, and logic operators look like...

    Cons (::): Prepend a value to an array
        a :: [ b ] === [ a, b ]

    Join (++): Concatenate two arrays together
        [ a, b ] ++ [ c, d ] === [ a, b, c, d ]
-}
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


-- 