module Parse.Source.Declaration exposing (suite)

-- IMPORTS ---------------------------------------------------------------------

import Parse.Source.Helpers
    exposing
        ( shouldFail
        , shouldSucceed
        )
import Ren.Data.Declaration as Declaration exposing (..)
import Ren.Data.Declaration.Visibility exposing (..)
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
    describe "Declaration parsing."
        [ shouldSucceed Declaration.parser
            "let x = 10"
            (Variable
                { comment = []
                , visibility = Private
                , name = Name "x"
                , bindings = []
                , body = Expression.number 10
                }
            )
        , shouldSucceed Declaration.parser
            "pub let x = 10"
            (Variable
                { comment = []
                , visibility = Public
                , name = Name "x"
                , bindings = []
                , body = Expression.number 10
                }
            )
        , shouldSucceed Declaration.parser
            """
            let x = {
                let y = 10
                ret y * 2
            }
            """
            (Variable
                { comment = []
                , visibility = Private
                , name = Name "x"
                , bindings =
                    [ Variable
                        { comment = []
                        , visibility = Private
                        , name = Name "y"
                        , bindings = []
                        , body = Expression.number 10
                        }
                    ]
                , body = Infix Mul (Expression.local "y") (Expression.number 2)
                }
            )
        , shouldSucceed Declaration.parser
            "fun foo = x => f x"
            (Function
                { comment = []
                , visibility = Private
                , name = Name "foo"
                , args = [ Name "x" ]
                , bindings = []
                , body = Application (Expression.local "f") [ Expression.local "x" ]
                }
            )
        , shouldSucceed Declaration.parser
            """
            fun foo = x => {
                let y = 10
                ret f x y

            }
            """
            (Function
                { comment = []
                , visibility = Private
                , name = Name "foo"
                , args = [ Name "x" ]
                , bindings =
                    [ Variable
                        { comment = []
                        , visibility = Private
                        , name = Name "y"
                        , bindings = []
                        , body = Expression.number 10
                        }
                    ]
                , body =
                    Application (Expression.local "f")
                        [ Expression.local "x", Expression.local "y" ]
                }
            )
        ]
