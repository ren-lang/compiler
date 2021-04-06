module Cherry.AST.Declaration exposing 
    ( Declaration(..)
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.AST.Expression as Expression exposing (Expression)


-- TYPES -----------------------------------------------------------------------


{-| There are only two types of top-level declaration allowed, `Function`s and
`Variable` declarations.

An example of some function declarations:

    fun foo = x y => x + y * z where z = 50

    fun bar = { a, b } =>
        if a > b then
            a

        else
            c

    pub fun baz = a [ b, c ] => [ a, b, c ]

Function arguments may be simple variable bindings such as `x` or `a` or they may
be obtained through array and/or object destructuring. Note that function bodies
are a *single* expression

An example of some variable declarations:

    let foo = 'hello world' |> String.toUpperCase

    let bar = String.repeat n 'test' where n = 10

    pub let baz = { a: 'hello', b: 'Cherry' }

Both function and variable declarations may optionally be made public by prefixing
the `pub` keyword. Additionally both declarations may attach any number of `where`
bindings after the body expression.
-}
type Declaration
    = Function Bool String (List Expression.Variable) Expression (List ( String, Expression ))
    | Variable Bool String Expression (List ( String, Expression ))
