module Util.Dict exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Dict exposing (Dict)
import Util.Maybe as Maybe



-- MODIFICATIONS ---------------------------------------------------------------


upsert : comparable -> a -> Dict comparable a -> Dict comparable a
upsert k v dict =
    Dict.update k (Maybe.or (Just v)) dict
