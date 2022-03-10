module Ren.Compiler.Emit exposing
    ( run
    , Target(..)
    )

{-|

@docs run
@docs Target

-}

-- IMPORTS ---------------------------------------------------------------------

import Ren.AST.Module as Module exposing (Module)
import Ren.Compiler.Emit.ESModule as ESModule
import Ren.Data.Type as Type



--


{-| -}
run : Target -> Module meta -> String
run target m =
    case target of
        ESModule ->
            ESModule.run m

        DEBUG_Types ->
            let
                showDeclaration declr =
                    case declr of
                        Module.Ext _ name type_ _ ->
                            Just <| name ++ " : " ++ (Type.toString <| Type.reduce type_)

                        Module.Let _ name type_ _ _ ->
                            Just <| name ++ " : " ++ (Type.toString <| Type.reduce type_)

                        Module.Run _ _ ->
                            Nothing

                        Module.Type _ _ _ _ _ ->
                            Nothing
            in
            m.declarations
                |> List.filterMap showDeclaration
                |> String.join "\n\n"



-- TYPES -----------------------------------------------------------------------


{-| -}
type Target
    = ESModule
    | DEBUG_Types
