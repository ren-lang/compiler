module Ren.Ast.Decl.Meta exposing (..)

{-| ##Types

@docs Meta


## Constants

@docs default


## Constructors

@docs new


## JSON

@docs encode, decoder

-}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode
import Ren.Ast.Type as Type exposing (Type)
import Ren.Data.Span as Span exposing (Span)



-- TYPES -----------------------------------------------------------------------


{-| -}
type alias Meta =
    { tipe : Type
    , inferred : Bool
    , span : Span
    , comment : List String
    }



-- CONSTANTS -------------------------------------------------------------------


{-| -}
default : Meta
default =
    { tipe = Type.Hole
    , inferred = False
    , span = Span.from ( 1, 1 ) ( 1, 1 )
    , comment = []
    }



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
new : Span -> Meta
new span =
    { tipe = Type.Hole
    , inferred = False
    , span = span
    , comment = []
    }



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
setSpan : Span -> Meta -> Meta
setSpan span meta =
    { meta | span = span }


{-| -}
addComment : String -> Meta -> Meta
addComment comment meta =
    { meta | comment = comment :: meta.comment }


setComments : List String -> Meta -> Meta
setComments comments meta =
    { meta | comment = comments }


setType : Type -> Meta -> Meta
setType tipe meta =
    { meta | tipe = tipe }



-- JSON ------------------------------------------------------------------------


{-| -}
encode : Meta -> List ( String, Json.Encode.Value )
encode meta =
    [ ( "type", Type.encode meta.tipe )
    , ( "inferred", Json.Encode.bool meta.inferred )
    , ( "span", Span.encode meta.span )
    , ( "comment", Json.Encode.list Json.Encode.string meta.comment )
    ]


{-| -}
decoder : Json.Decode.Decoder Meta
decoder =
    Json.Decode.map4 Meta
        (Json.Decode.field "type" <| Type.decoder)
        (Json.Decode.field "inferred" <| Json.Decode.bool)
        (Json.Decode.field "span" <| Span.decoder)
        (Json.Decode.field "comment" <| Json.Decode.list Json.Decode.string)
