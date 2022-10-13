module Ren.Data.Error exposing (..)

-- TYPES -----------------------------------------------------------------------


type alias Error =
    String


type Context
    = Mod
    | Import
      -- Declarations
    | Decl
    | LetDecl
    | ExtDecl
      -- Expressions
    | Expr
    | Access
    | Annotation
    | Binop
    | Call
    | If
    | Lambda
    | Let
    | Lit
    | Placeholder
    | Scoped
    | Where
    | Var
      -- Patterns
    | Pat
    | Any
