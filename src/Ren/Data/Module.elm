module Ren.Data.Module exposing
    ( Module(..), Import
    , module_, import_
    , imports, exposes
    , addImport, addDeclaration
    , fromJSON, decoder
    , fromSource, parser
    )

{-| Table of Contents

  - Types
      - [Module](#Module)
      - [Import](#Import)
  - Helpers
      - Constructors
          - [module\_](#module_)
          - [import\_](#import_)
      - Queries
          - [imports](#imports)
          - [exposes](#exposes)
      - Modifications
          - [addImport](#addImport)
          - [addDefaultImports](#addDefaultImports)
  - Parsing
      - [fromJSON](#fromJSON)
      - [decoder](#decoder)
      - [fromSource](#fromSource)
      - [parser](#parser)

---


## Types

@docs Module, Import

---


## Helpers


### Constructors

@docs module_, import_


### Queries

@docs imports, exposes


### Modifications

@docs addImport, addDeclaration

---


## Parsing

@docs fromJSON, decoder
@docs fromSource, parser

-}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing ((|.), (|=), Parser)
import Ren.Data.Declaration as Declaration exposing (Declaration)
import Ren.Data.Declaration.Visibility exposing (Visibility(..))
import Ren.Data.Module.Import as Import



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
type alias Import =
    Import.Import



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



-- QUERIES ---------------------------------------------------------------------


{-| Check if a module imports a function or variable with the given name.
-}
imports : String -> Module -> Bool
imports name (Module data) =
    data.imports
        |> List.map Import.exposed
        |> List.any (List.member name)


{-| Check if a module exposes a function or variable with the given name.
-}
exposes : String -> Module -> Bool
exposes name (Module data) =
    data.declarations
        |> List.filter (Declaration.visibility >> (==) Public)
        |> List.map Declaration.name
        |> List.member name



-- MODIFICATIONS ---------------------------------------------------------------


{-| -}
addImport : Import -> Module -> Module
addImport import__ (Module data) =
    Module { data | imports = import__ :: data.imports }


{-| -}
addDeclaration : Declaration -> Module -> Module
addDeclaration declaration (Module data) =
    Module { data | declarations = declaration :: data.declarations }



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
