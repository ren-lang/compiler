module Ren.Ast.Mod exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Decl as Decl exposing (Decl)
import Ren.Ast.Mod.Import as Import exposing (Import)
import Ren.Ast.Mod.Meta as Meta exposing (Meta)
import Util.Json as Json
import Util.List as List



-- TYPES -----------------------------------------------------------------------


type Mod
    = Mod Meta (List Import) (List Decl)



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------
-- QUERIES ---------------------------------------------------------------------


imports : String -> Mod -> Bool
imports path (Mod _ imps _) =
    List.any (\imp -> imp.path == path) imps


importsLocal : String -> Mod -> Bool
importsLocal path (Mod _ imps _) =
    List.any (\imp -> Import.isLocal imp && imp.path == path) imps


importsPackage : String -> Mod -> Bool
importsPackage path (Mod _ imps _) =
    List.any (\imp -> Import.isPackage imp && imp.path == path) imps


importsExternal : String -> Mod -> Bool
importsExternal path (Mod _ imps _) =
    List.any (\imp -> Import.isExternal imp && imp.path == path) imps


declares : String -> Mod -> Bool
declares name (Mod _ _ decls) =
    List.any (\dec -> Decl.name dec == name) decls


declaresLocal : String -> Mod -> Bool
declaresLocal name (Mod _ _ decls) =
    List.any (\dec -> Decl.isLocal dec && Decl.name dec == name) decls


declaresExternal : String -> Mod -> Bool
declaresExternal name (Mod _ _ decls) =
    List.any (\dec -> Decl.isExternal dec && Decl.name dec == name) decls


exports : String -> Mod -> Bool
exports name (Mod _ _ decls) =
    List.any (\dec -> Decl.isPublic dec && Decl.name dec == name) decls


exportsLocal : String -> Mod -> Bool
exportsLocal name (Mod _ _ decls) =
    List.any (\dec -> Decl.isPublic dec && Decl.isLocal dec && Decl.name dec == name) decls


exportsExternal : String -> Mod -> Bool
exportsExternal name (Mod _ _ decls) =
    List.any (\dec -> Decl.isPublic dec && Decl.isExternal dec && Decl.name dec == name) decls



-- MANIPULATIONS ---------------------------------------------------------------


addImport : Import -> Mod -> Mod
addImport imp (Mod meta imps decls) =
    let
        -- Check if an import with the same path, source, and name already exists.
        -- This means we can merge the incoming import with and existing one and
        -- save importing a module twice.
        sameImport { path, source, name } =
            path == imp.path && source == imp.source && name == imp.name
    in
    Mod meta
        (if List.any (Import.alike imp) imps then
            List.updateBy sameImport
                (\{ unqualified } ->
                    { imp
                        | unqualified =
                            List.concat [ imp.unqualified, unqualified ]
                                |> List.uniques
                    }
                )
                imps

         else
            imp :: imps
        )
        decls


addDecl : Decl -> Mod -> Mod
addDecl dec (Mod meta imps decls) =
    Mod meta
        imps
        (decls
            |> List.filter (Decl.name >> (/=) (Decl.name dec))
            |> (::) dec
        )



-- CONVERSIONS -----------------------------------------------------------------
-- JSON ------------------------------------------------------------------------


encode : Mod -> Json.Encode.Value
encode (Mod meta imps decls) =
    Json.taggedEncoder "Mod"
        (Meta.encode meta)
        [ Json.Encode.list Import.encode imps
        , Json.Encode.list Decl.encode decls
        ]


decoder : Json.Decode.Decoder Mod
decoder =
    Json.taggedDecoder
        (\key ->
            if key == "Mod" then
                Json.Decode.map3 Mod
                    (Json.Decode.index 0 <| Meta.decoder)
                    (Json.Decode.index 1 <| Json.Decode.list Import.decoder)
                    (Json.Decode.index 2 <| Json.Decode.list Decl.decoder)

            else
                Json.Decode.fail <| "Unknown module: " ++ key
        )



-- UTILS -----------------------------------------------------------------------
