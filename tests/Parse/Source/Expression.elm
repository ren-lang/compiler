module Parse.Source.Expression exposing (suite)

-- IMPORTS ---------------------------------------------------------------------

import Fuzz exposing (..)
import Parse.Source.Helpers
    exposing
        ( shouldSucceed
        )
import Ren.Data.Expression as Expression exposing (..)
import Ren.Data.Expression.Accessor exposing (..)
import Ren.Data.Expression.Identifier exposing (..)
import Ren.Data.Expression.Literal exposing (..)
import Ren.Data.Expression.Operator exposing (..)
import Ren.Data.Expression.Pattern exposing (..)
import Test exposing (..)



-- SUITES ----------------------------------------------------------------------


suite : Test
suite =
    describe "Expression parsing."
        [ shouldSucceed Expression.parser
            "f a b"
            (Application
                (local "f")
                [ local "a"
                , local "b"
                ]
            )
        , shouldSucceed Expression.parser
            "(f a) b"
            (Application
                (SubExpression
                    (Application (local "f")
                        [ local "a" ]
                    )
                )
                [ local "b" ]
            )
        , shouldSucceed Expression.parser
            "f ()"
            (Application
                (local "f")
                [ Literal Undefined ]
            )
        , shouldSucceed Expression.parser
            "a |> f"
            (Infix Pipe
                (local "a")
                (local "f")
            )
        , shouldSucceed Expression.parser
            "b |> f a"
            (Infix Pipe
                (local "b")
                (Application (local "f")
                    [ local "a" ]
                )
            )
        , shouldSucceed Expression.parser
            "if a then b else c"
            (Conditional
                (local "a")
                (local "b")
                (local "c")
            )
        , shouldSucceed Expression.parser
            "if f a then b else c"
            (Conditional
                (Application (local "f") [ local "a" ])
                (local "b")
                (local "c")
            )
        , shouldSucceed Expression.parser
            "if a then f b else c"
            (Conditional
                (local "a")
                (Application (local "f") [ local "b" ])
                (local "c")
            )
        , shouldSucceed Expression.parser
            "if f a then f b else f c"
            (Conditional
                (Application (local "f") [ local "a" ])
                (Application (local "f") [ local "b" ])
                (Application (local "f") [ local "c" ])
            )
        , shouldSucceed Expression.parser
            "if f a then f b else if f c then f d else f e"
            (Conditional
                (Application (local "f") [ local "a" ])
                (Application (local "f") [ local "b" ])
                (Conditional
                    (Application (local "f") [ local "c" ])
                    (Application (local "f") [ local "d" ])
                    (Application (local "f") [ local "e" ])
                )
            )
        , shouldSucceed Expression.parser
            "3 + (if a then b else c) - 1"
            (Infix Sub
                (Infix Add
                    (Literal (Number 3))
                    (SubExpression
                        (Conditional
                            (Identifier (Local "a"))
                            (Identifier (Local "b"))
                            (Identifier (Local "c"))
                        )
                    )
                )
                (Literal (Number 1))
            )
        ]
