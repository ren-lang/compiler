module Ren.Compiler.Emit exposing
    ( run
    , Target(..)
    )

{-|

@docs run
@docs Target

-}

-- IMPORTS ---------------------------------------------------------------------

import Ren.AST.Module exposing (Module)
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
                showDeclaration { name, type_ } =
                    name ++ " : " ++ (Type.toString <| Type.reduce type_)
            in
            m.declarations
                |> List.map showDeclaration
                |> String.join "\n\n"



-- TYPES -----------------------------------------------------------------------


type Target
    = ESModule
    | DEBUG_Types
