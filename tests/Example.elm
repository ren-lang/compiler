module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Example suite"
        [ test "Example test" <|
            \_ -> 1 |> Expect.equal 1
        ]
