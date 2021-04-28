module Cherry.Data.Module exposing 
    ( Module(..), Import
    , module_, import_
    , imports, exposes
    , fromJSON, decoder
    , fromSource, parser
    )


{-| Table of Contents

* Types
    * [Module](#Module)
    * [Import](#Import)
* Helpers
    * Constructors
        * [module_](#module_)
        * [import_](#import_)
    * Queries
        * [imports](#imports)
        * [exposes](#exposes)
* Parsing
    * [fromJSON](#fromJSON)
    * [decoder](#decoder)
    * [fromSource](#fromSource)
    * [parser](#parser)

---

## Types

@docs Module, Import

---

## Helpers

### Constructors

@docs module_, import_

### Queries

@docs imports, exposes

---

## Parsing

@docs fromJSON, decoder
@docs fromSource, parser

-}


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.Declaration as Declaration exposing 
    ( Declaration
    , Visibility(..)
    )
import Cherry.Data.Module.Import as Import
import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing (Parser, (|=), (|.))



-- TYPES -----------------------------------------------------------------------


{-| -}
type Module
    = Module
        { imports : List Import
        , declarations : List Declaration
        }

{-| Imports can take a few different shapes. 

They can be aliased:

    import 'foo/bar' as Foo.Bar

They can expose unqualified bindings:

    import 'foo/bar' exposing { baz }

Or both:

    import 'foo/bar' as Foo.Bar exposing { baz }

Or neither:

    import 'foo/bar'

-}
type alias Import
    = Import.Import


-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
module_ : List Import -> List Declaration -> Module
module_ imports_ declarations =
    Module
        { imports = imports_
        , declarations = declarations
        }

{-| -}
import_ : String -> List String -> List String -> Import
import_ =
    Import.import_


-- HELPERS ---------------------------------------------------------------------


{-| Check if a module imports a function or variable with the given name. -}
imports : String -> Module -> Bool
imports name (Module data) =
    data.imports
        |> List.map Import.exposed
        |> List.any (List.member name)


{-| Check if a module exposes a function or variable with the given name. -}
exposes : String -> Module -> Bool
exposes name (Module data) =
    data.declarations
        |> List.filter (Declaration.visibility >> (==) Public)
        |> List.map Declaration.name
        |> List.member name


-- PARSING JSON ----------------------------------------------------------------


{-| -}
fromJSON : Json.Decode.Value -> Result Json.Decode.Error Module
fromJSON json =
    Json.Decode.decodeValue decoder json

{-| -}
decoder : Decoder Module
decoder =
    Json.Decode.Extra.taggedObject "Module" <|
        Json.Decode.map2 module_
            (Json.Decode.field "imports" <|
                Json.Decode.list Import.decoder
            )
            (Json.Decode.field "declarations" <|
                Json.Decode.list Declaration.decoder
            )


-- PARSING SOURCE --------------------------------------------------------------


{-| -}
fromSource : String -> Result (List Parser.DeadEnd) Module
fromSource source =
    Parser.run parser source

{-| -}
parser : Parser Module
parser =
    Parser.succeed module_
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.spaces
            , item = Import.parser
            , trailing = Parser.Optional
            }
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.spaces
            , item = Declaration.parser
            , trailing = Parser.Optional
            }
        |. Parser.spaces
        |. Parser.end
