module Cherry.Encode.JSON exposing (..)


-- IMPORTS ---------------------------------------------------------------------


import Cherry.AST.Module exposing (..)
import Cherry.Encode.JSON.Declaration as Declaration
import Json.Encode
import Json.Encode.Extra


-- RUNNING THE ENCODER ---------------------------------------------------------


encode : Module -> String
encode module_ =
    encoder module_
        |> Json.Encode.encode 4


-- ENCODER ---------------------------------------------------------------------


encoder : Module -> Json.Encode.Value
encoder { imports, declarations } =
    Json.Encode.Extra.taggedObject "Module"
        [ ( "imports", Json.Encode.list importEncoder imports )
        , ( "declarations", Json.Encode.list Declaration.encoder declarations )
        ]


-- IMPORT ENCODER --------------------------------------------------------------


importEncoder : Import -> Json.Encode.Value
importEncoder (Import path as_ exposing_) =
    let
        asEncoder a =
            ( "as", Json.Encode.list Json.Encode.string a )
    
        exposingEncoder e =
            ( "exposing", Json.Encode.list Json.Encode.string e )
    in
    Json.Encode.Extra.taggedObject "Module.Import" <|
        ( "path", Json.Encode.string path ) :: List.filterMap identity
            [ Maybe.map asEncoder as_
            , Maybe.map exposingEncoder exposing_
            ]