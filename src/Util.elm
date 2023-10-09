module Util exposing (find, get, getMinToFront, getOr, listContain, quickSort)

import Html exposing (a)


quickSort : (a -> comparable) -> List a -> List a
quickSort f lst =
    case lst of
        head :: rest ->
            let
                ( left, right ) =
                    List.partition (\n -> f head > f n) rest
            in
            quickSort f left ++ quickSort f right

        _ ->
            lst


get : Int -> List a -> Maybe a
get x lst =
    case ( x, lst ) of
        ( 0, h :: _ ) ->
            Just h

        ( _, [] ) ->
            Nothing

        ( y, _ :: t ) ->
            get (y - 1) t


getOr : Int -> List a -> a -> a
getOr x lst fallback =
    case get x lst of
        Just v ->
            v

        Nothing ->
            fallback


getMinToFrontHelper : (a -> comparable) -> a -> List a -> List a -> List a
getMinToFrontHelper f min acc_lst lst =
    case lst of
        [] ->
            min :: acc_lst

        h :: rest ->
            if f h < f min then
                getMinToFrontHelper f h (min :: acc_lst) rest

            else
                getMinToFrontHelper f min (h :: acc_lst) rest


getMinToFront : (a -> comparable) -> List a -> List a
getMinToFront f lst =
    case lst of
        [] ->
            []

        head :: rest ->
            getMinToFrontHelper f head [] rest


listContain : List a -> a -> Bool
listContain lst elm =
    List.foldl (\e acc -> acc || e == elm) False lst


find : List a -> (a -> Bool) -> Maybe Int
find lst pred =
    lst
        |> List.indexedMap (\i a -> ( i, a ))
        |> List.foldl
            (\x acc ->
                case ( acc, x ) of
                    ( Nothing, ( i, a ) ) ->
                        if pred a then
                            Just i

                        else
                            Nothing

                    ( v, _ ) ->
                        v
            )
            Nothing
