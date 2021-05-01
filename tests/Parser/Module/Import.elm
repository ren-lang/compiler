module Parser.Module.Import exposing 
    ( suite
    )


-- IMPORTS ---------------------------------------------------------------------


import Ren.Data.Module.Import as Import exposing (Import(..))
import Parser

import Expect
import Test exposing (..)


-- SUITES ----------------------------------------------------------------------


suite : Test
suite =
    describe "Module.Import"
        [ importParserTests ]

importParserTests : Test
importParserTests =
    describe "Imports"
        [ test "with no namespace" <| \_ ->
            Parser.run Import.parser "import 'console' exposing { log }"
                |> Expect.equal (Ok <|
                    Import.Import
                        { path = "console"
                        , name = []
                        , exposed = [ "log" ]
                        }
                    )
        , test "with a single namespace" <| \_ ->
            Parser.run Import.parser "import 'console' as Console exposing { log }"
                |> Expect.equal (Ok <|
                    Import.Import
                        { path = "console"
                        , name = [ "Console" ]
                        , exposed = [ "log" ]
                        }
                    )
        , test "with multiple namespaces" <| \_ ->
            Parser.run Import.parser "import 'console' as Foo.Bar.Baz exposing { log }"
                |> Expect.equal (Ok <|
                    Import.Import
                        { path = "console"
                        , name = [ "Foo", "Bar", "Baz" ]
                        , exposed = [ "log" ]
                        }
                    )
        , test "fails if trailing namespaces are not title case" <| \_ ->
            Parser.run Import.parser "import 'console' as Foo.bar.baz exposing { log }"
                |> Expect.err
        , test "fails if some namespaces are not title case" <| \_ ->
            Parser.run Import.parser "import 'console' as foo.Bar.baz exposing { log }"
                |> Expect.err
        , test "fails if 'as' keyword used with no namespace" <| \_ ->
            Parser.run Import.parser "import 'console' as exposing { log }"
                |> Expect.err
        , test "fails if namespaces start with a non-alphanumeric char" <| \_ ->
            Parser.run Import.parser "import 'console' as #Foo exposing { log }"
                |> Expect.err
        , test "fails if namespaces start with a leading period" <| \_ ->
            Parser.run Import.parser "import 'console' as .Foo exposing { log }"
                |> Expect.err
        , test "fails if namespaces have a trailing period" <| \_ ->
            Parser.run Import.parser "import 'console' as Foo. exposing { log }"
                |> Expect.err
        ]
