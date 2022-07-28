module Ren.Ast.Expr.Meta exposing (..)

{-| ##Types

@docs Meta


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
    , span : Span
    , comment : List String
    }



-- CONSTANTS -------------------------------------------------------------------


{-| -}
default : Meta
default =
    { tipe = Type.Hole
    , span = Span.from ( 1, 1 ) ( 1, 1 )
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



-- JSON ------------------------------------------------------------------------


{-| -}
encode : Meta -> List ( String, Json.Encode.Value )
encode meta =
    [ ( "type", Type.encode meta.tipe )
    , ( "span", Span.encode meta.span )
    , ( "comment", Json.Encode.list Json.Encode.string meta.comment )
    ]


{-| -}
decoder : Json.Decode.Decoder Meta
decoder =
    Json.Decode.map3 Meta
        (Json.Decode.field "type" <| Type.decoder)
        (Json.Decode.field "span" <| Span.decoder)
        (Json.Decode.field "comment" <| Json.Decode.list Json.Decode.string)
