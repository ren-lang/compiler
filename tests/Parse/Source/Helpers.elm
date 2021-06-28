module Parse.Source.Helpers exposing
    ( shouldFail
    , shouldSucceed
    )

-- IMPORTS ---------------------------------------------------------------------

import Expect
import Fuzz exposing (..)
import Parser exposing (Parser)
import Test exposing (..)


shouldSucceed : Parser a -> String -> a -> Test
shouldSucceed parser input expected =
    test input
        (\_ ->
            Parser.run parser input
                |> Expect.equal (Ok expected)
        )


shouldFail : Parser a -> String -> Test
shouldFail parser input =
    test input
        (\_ ->
            Parser.run parser input
                |> Expect.err
        )
