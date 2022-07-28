module Util.Dict exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Util.Maybe as Maybe



-- MODIFICATIONS ---------------------------------------------------------------


upsert : comparable -> a -> (a -> a) -> Dict comparable a -> Dict comparable a
upsert k v f dict =
    Dict.update k (Maybe.map f >> Maybe.or (Just v)) dict


transform : comparable -> (a -> a) -> Dict comparable a -> Dict comparable a
transform k f dict =
    Dict.update k
        (Maybe.map f)
        dict


combine : (a -> a -> a) -> Dict comparable a -> Dict comparable a -> Dict comparable a
combine f a b =
    Dict.merge
        (\k x d -> Dict.insert k x d)
        (\k x y d -> Dict.insert k (f x y) d)
        (\k x d -> Dict.insert k x d)
        a
        b
        Dict.empty
