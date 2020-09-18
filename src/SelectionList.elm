module SelectionList exposing (SelectionList, attemptToggle, create, getUnselected)

import List.Extra


type SelectionList a
    = SelectionList Int (List ( Bool, a ))


create : Int -> List a -> SelectionList a
create limit xs =
    let
        list : List ( Bool, a )
        list =
            List.map (Tuple.pair False) xs
    in
    SelectionList limit list


getLimit : SelectionList a -> Int
getLimit (SelectionList limit _) =
    limit


getList : SelectionList a -> List ( Bool, a )
getList (SelectionList _ xs) =
    xs


numSelected : List ( Bool, a ) -> Int
numSelected =
    List.map Tuple.first >> List.filter identity >> List.length


{-| If the provided index is in range, runs the given function on the item at that index and if it returns Just, replaces that index with the result
-}
attemptReplaceAtIndex : Int -> (a -> Maybe a) -> List a -> Maybe (List a)
attemptReplaceAtIndex index fn list =
    List.Extra.getAt index list
        |> Maybe.andThen fn
        |> Maybe.map
            (\val ->
                List.concat
                    [ List.take index list
                    , [ val ]
                    , List.drop (index + 1) list
                    ]
            )


toggle : ( Bool, a ) -> ( Bool, a )
toggle =
    Tuple.mapFirst not


attemptToggle : Int -> SelectionList a -> Maybe (SelectionList a)
attemptToggle index (SelectionList limit list) =
    let
        limitReached : Bool
        limitReached =
            numSelected list >= limit
    in
    list
        |> attemptReplaceAtIndex index
            (\selectionTuple ->
                if Tuple.first selectionTuple && limitReached then
                    Nothing

                else
                    Just (toggle selectionTuple)
            )
        |> Maybe.map (SelectionList limit)


getUnselected : SelectionList a -> List a
getUnselected (SelectionList _ list) =
    list
        |> List.filter (Tuple.first >> not)
        |> List.map Tuple.second
