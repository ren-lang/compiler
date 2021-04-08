module Cherry.Stage.Generate.JSON.Module exposing 
    ( run, generator
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Generate.JSON.Declaration as Declaration
import Json.Encode
import Json.Encode.Extra


-- RUNNING THE GENERATOR -------------------------------------------------------


{-| -}
run : AST.Module -> String
run ast =
    generator ast
        |> Json.Encode.encode 4


-- GENERATORS ------------------------------------------------------------------


{-| -}
generator : AST.Module -> Json.Encode.Value
generator { imports, declarations } =
    Json.Encode.Extra.taggedObject "AST.Module"
        [ ( "imports", Json.Encode.list importGenerator imports )
        , ( "declarations", Json.Encode.list Declaration.generator declarations )
        ]
    
{-| -}
importGenerator : AST.Import -> Json.Encode.Value
importGenerator { path, name, exposedBindings } =
    Json.Encode.Extra.taggedObject "AST.Import"
        [ ( "path", Json.Encode.string path )
        , ( "name", Json.Encode.list Json.Encode.string name )
        , ( "exposedBindings", Json.Encode.list Json.Encode.string exposedBindings )
        ]
