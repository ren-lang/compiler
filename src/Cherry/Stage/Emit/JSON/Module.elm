module Cherry.Stage.Emit.JSON.Module exposing 
    ( emit
    )


-- IMPORTS ---------------------------------------------------------------------


import Cherry.Data.AST as AST
import Cherry.Stage.Emit.JSON.Declaration as Declaration
import Json.Encode
import Json.Encode.Extra


-- EMITTERS --------------------------------------------------------------------


{-| -}
emit : AST.Module -> Json.Encode.Value
emit { imports, declarations } =
    Json.Encode.Extra.taggedObject "AST.Module"
        [ ( "imports", Json.Encode.list importEmitter imports )
        , ( "declarations", Json.Encode.list Declaration.emit declarations )
        ]
    
{-| -}
importEmitter : AST.Import -> Json.Encode.Value
importEmitter { path, name, exposedBindings } =
    Json.Encode.Extra.taggedObject "AST.Import"
        [ ( "path", Json.Encode.string path )
        , ( "name", Json.Encode.list Json.Encode.string name )
        , ( "exposedBindings", Json.Encode.list Json.Encode.string exposedBindings )
        ]
