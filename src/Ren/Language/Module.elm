module Ren.Language.Module exposing
    ( insertImport, removeImport, removeImportByPath
    , insertDeclaration, removeDeclaration
    , imports, exports
    , references, referencesNamespace, referencesQualified
    )

{-|

@docs insertImport, removeImport, removeImportByPath
@docs insertDeclaration, removeDeclaration
@docs imports, exports
@docs references, referencesNamespace, referencesQualified

-}

-- IMPORTS ---------------------------------------------------------------------

import Ren.Language exposing (..)
import Ren.Language.Declaration



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
insertImport : Import -> Module -> Module
insertImport i m =
    { m | imports = m.imports ++ [ i ] }


{-| -}
removeImport : Import -> Module -> Module
removeImport i m =
    { m | imports = List.filter ((/=) i) m.imports }


{-| -}
removeImportByPath : String -> Module -> Module
removeImportByPath p m =
    { m | imports = List.filter (.path >> (/=) p) m.imports }


{-| -}
insertDeclaration : ( Visibility, Declaration ) -> Module -> Module
insertDeclaration d m =
    { m | declarations = m.declarations ++ [ d ] }


{-| -}
removeDeclaration : Declaration -> Module -> Module
removeDeclaration d m =
    { m | declarations = List.filter (Tuple.second >> (/=) d) m.declarations }



-- QUERIES ---------------------------------------------------------------------


{-| -}
imports : String -> Module -> Bool
imports name module_ =
    module_.imports
        |> List.concatMap .bindings
        |> List.any ((==) name)


{-| -}
exports : String -> Module -> Bool
exports name module_ =
    module_.declarations
        |> List.filter (Tuple.first >> (==) Public)
        |> List.any
            (\( _, declaration ) ->
                case declaration of
                    Function n _ _ ->
                        name == n

                    Variable n _ ->
                        exportsInPattern name n

                    Enum n variants ->
                        name == n || List.any (\(Variant t _) -> name == t) variants
            )


exportsInPattern : String -> Pattern -> Bool
exportsInPattern name pattern =
    case pattern of
        ArrayDestructure elements ->
            List.any (exportsInPattern name) elements

        Name n ->
            name == n

        ObjectDestructure entries ->
            List.any
                (\( k, v ) ->
                    Maybe.map (exportsInPattern name) v
                        |> Maybe.withDefault (name == k)
                )
                entries

        VariantDestructure _ _ ->
            False

        Value _ ->
            False

        Typeof _ n ->
            name == n

        Wildcard _ ->
            False


{-| -}
references : String -> Module -> Bool
references name module_ =
    List.any
        (Tuple.second >> Ren.Language.Declaration.references name)
        module_.declarations


{-| -}
referencesNamespace : List String -> Module -> Bool
referencesNamespace namespace module_ =
    List.any
        (Tuple.second >> Ren.Language.Declaration.referencesNamespace namespace)
        module_.declarations


{-| -}
referencesQualified : List String -> String -> Module -> Bool
referencesQualified namespace name module_ =
    List.any
        (Tuple.second >> Ren.Language.Declaration.referencesQualified namespace name)
        module_.declarations
