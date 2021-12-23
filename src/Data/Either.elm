module Data.Either exposing (..)

-- TYPES -----------------------------------------------------------------------


type Either a b
    = Left a
    | Right b



-- CONSTRUCTORS ----------------------------------------------------------------


fromMaybe : b -> Maybe a -> Either a b
fromMaybe b maybe =
    case maybe of
        Just a ->
            Left a

        Nothing ->
            Right b


fromResult : Result b a -> Either a b
fromResult result =
    case result of
        Ok a ->
            Left a

        Err b ->
            Right b



-- MANIPULATIONS ---------------------------------------------------------------


map : (a -> c) -> Either a b -> Either c b
map f either =
    case either of
        Left a ->
            Left (f a)

        Right b ->
            Right b


mapRight : (b -> c) -> Either a b -> Either a c
mapRight f either =
    case either of
        Left a ->
            Left a

        Right b ->
            Right (f b)


mapBoth : (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f g either =
    case either of
        Left a ->
            Left (f a)

        Right b ->
            Right (g b)


andThen : (a -> Either c b) -> Either a b -> Either c b
andThen f either =
    case either of
        Left a ->
            f a

        Right b ->
            Right b



-- CONVERSIONS -----------------------------------------------------------------


toMaybe : Either a b -> Maybe a
toMaybe either =
    case either of
        Left a ->
            Just a

        Right _ ->
            Nothing


toResult : Either a b -> Result b a
toResult either =
    case either of
        Left a ->
            Ok a

        Right b ->
            Err b


extract : (a -> c) -> (b -> c) -> Either a b -> c
extract f g either =
    case either of
        Left a ->
            f a

        Right b ->
            g b
