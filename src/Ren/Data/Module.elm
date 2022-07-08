module Ren.Data.Module exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Ren.Ast.Expr exposing (Expr)
import Ren.Data.Declaration as Declaration exposing (Declaration)
import Ren.Data.Import as Import exposing (Import)
import Util.List as List



-- TYPES -----------------------------------------------------------------------


type alias Module =
    { imports : List Import
    , declarations : List Declaration
    }



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


empty : Module
empty =
    { imports = []
    , declarations = []
    }



-- QUERIES ---------------------------------------------------------------------


imports : String -> Module -> Bool
imports path mod =
    List.any (\imp -> imp.path == path) mod.imports


importsLocal : String -> Module -> Bool
importsLocal path mod =
    List.any (\imp -> Import.isLocal imp && imp.path == path) mod.imports


importsPackage : String -> Module -> Bool
importsPackage path mod =
    List.any (\imp -> Import.isPackage imp && imp.path == path) mod.imports


importsExternal : String -> Module -> Bool
importsExternal path mod =
    List.any (\imp -> Import.isExternal imp && imp.path == path) mod.imports


declares : String -> Module -> Bool
declares name mod =
    List.any (\dec -> Declaration.name dec == name) mod.declarations


declaresLocal : String -> Module -> Bool
declaresLocal name mod =
    List.any (\dec -> Declaration.isLocal dec && Declaration.name dec == name) mod.declarations


declaresExternal : String -> Module -> Bool
declaresExternal name mod =
    List.any (\dec -> Declaration.isExternal dec && Declaration.name dec == name) mod.declarations


exports : String -> Module -> Bool
exports name mod =
    List.any (\dec -> Declaration.isPublic dec && Declaration.name dec == name) mod.declarations


exportsLocal : String -> Module -> Bool
exportsLocal name mod =
    List.any (\dec -> Declaration.isPublic dec && Declaration.isLocal dec && Declaration.name dec == name) mod.declarations


exportsExternal : String -> Module -> Bool
exportsExternal name mod =
    List.any (\dec -> Declaration.isPublic dec && Declaration.isExternal dec && Declaration.name dec == name) mod.declarations



-- MANIPULATIONS ---------------------------------------------------------------


addImport : Import -> Module -> Module
addImport imp mod =
    let
        -- Check if an import with the same path, source, and name already exists.
        -- This means we can merge the incoming import with and existing one and
        -- save importing a module twice.
        sameImport { path, source, name } =
            path == imp.path && source == imp.source && name == imp.name
    in
    { mod
        | imports =
            if List.any (Import.alike imp) mod.imports then
                List.updateBy sameImport
                    (\{ unqualified } ->
                        { imp
                            | unqualified =
                                List.concat [ imp.unqualified, unqualified ]
                                    |> List.uniques
                        }
                    )
                    mod.imports

            else
                imp :: mod.imports
    }


addLocalImport : String -> List String -> List String -> Module -> Module
addLocalImport path name unqualified mod =
    addImport
        (Import.local
            path
            name
            unqualified
        )
        mod


addPackageImport : String -> List String -> List String -> Module -> Module
addPackageImport path name unqualified mod =
    addImport
        (Import.package
            path
            name
            unqualified
        )
        mod


addExternalImport : String -> List String -> List String -> Module -> Module
addExternalImport path name unqualified mod =
    addImport
        (Import.external
            path
            name
            unqualified
        )
        mod


addDeclaration : Declaration -> Module -> Module
addDeclaration dec mod =
    { mod
        | declarations =
            mod.declarations
                |> List.filter (Declaration.name >> (==) (Declaration.name dec))
                |> (::) dec
    }


addLocalDeclaration : Bool -> String -> Expr -> Module -> Module
addLocalDeclaration pub name expr mod =
    addDeclaration
        (Declaration.local
            pub
            name
            expr
        )
        mod


addExternalDeclaration : Bool -> String -> String -> Module -> Module
addExternalDeclaration pub name path mod =
    addDeclaration
        (Declaration.external
            pub
            name
            path
        )
        mod



-- CONVERSIONS -----------------------------------------------------------------
-- UTILS -----------------------------------------------------------------------
