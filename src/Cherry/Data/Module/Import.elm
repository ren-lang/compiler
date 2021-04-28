module Cherry.Data.Module.Import exposing 
    ( Import(..), import_
    , path, name, exposed
    , decoder
    , parser
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.Keywords as Keywords
import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Parser exposing (Parser, (|=), (|.))
import Parser.Extra
import Set



-- TYPES -----------------------------------------------------------------------


{-| -}
type Import
    = Import
        { path : String
        , name : List String
        , exposed : List String
        }


-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
import_ : String -> List String -> List String -> Import
import_ path_ name_ exposed_ =
    Import
        { path = path_
        , name = name_
        , exposed = exposed_
        }


--


{-| -}
path : Import -> String
path (Import data) =
    data.path

{-| -}
name : Import -> List String
name (Import data) =
    data.name

{-| -}
exposed : Import -> List String
exposed (Import data) =
    data.exposed

-- PARSING JSON ----------------------------------------------------------------


{-| -}
decoder : Decoder Import
decoder =
    Json.Decode.Extra.taggedObject "Import" <|
        Json.Decode.map3 import_
            (Json.Decode.field "path" Json.Decode.string)
            (Json.Decode.field "name" <|
                Json.Decode.list Json.Decode.string
            )
            (Json.Decode.field "exposed" <|
                Json.Decode.list Json.Decode.string
            )


-- PARSING SOURCE --------------------------------------------------------------


{-| -}
parser : Parser Import
parser =
    Parser.succeed import_
        |. Parser.keyword "import"
        |. Parser.Extra.spaces
        |= Parser.oneOf
            [ Parser.Extra.string '\''
            , Parser.Extra.string '"'
            ]
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.Extra.spaces
                |. Parser.keyword "as"
                |. Parser.Extra.spaces
                |= Parser.sequence
                    { start = ""
                    , separator = "."
                    , end = ""
                    , item = Parser.variable
                        { start = Char.isUpper
                        , inner = Char.isAlphaNum
                        , reserved = Set.empty
                        }
                    , spaces = Parser.succeed ()
                    , trailing = Parser.Forbidden
                    }
            , Parser.succeed []
            ]
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.Extra.spaces
                |. Parser.keyword "exposing"
                |. Parser.Extra.spaces
                |= Parser.sequence
                    { start = "{"
                    , separator = ","
                    , end = "}"
                    , item = Parser.variable
                        { start = Char.isLower
                        , inner = Char.isAlphaNum
                        , reserved = Keywords.all
                        }
                    , spaces = Parser.spaces
                    , trailing = Parser.Forbidden
                    }
            , Parser.succeed []
            ]
