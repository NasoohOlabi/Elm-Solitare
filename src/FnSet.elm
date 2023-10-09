module FnSet exposing (..)


type alias Set a =
    a -> Bool


emptySet : Set a
emptySet _ =
    False


addToSet : a -> Set a -> Set a
addToSet a s =
    if s a then
        s

    else
        \x -> x == a || s x


removeFromSet : a -> Set a -> Set a
removeFromSet a s =
    if s a then
        \x -> a /= x && s x

    else
        s


oneTwoExample : number -> Bool
oneTwoExample =
    emptySet
        |> addToSet 1
        |> addToSet 2
        |> addToSet 100
        |> addToSet 12
        |> addToSet 14
        |> addToSet 1
