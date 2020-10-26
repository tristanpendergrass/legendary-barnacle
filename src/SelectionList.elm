module SelectionList exposing (SelectionItemStatus(..), SelectionList, attemptToggle, create, getSelected, getUnselected, map)

import List.Extra
import Maybe.Extra
import PlayerCard exposing (PlayerCard)


type SelectionList
    = SelectionList Int (List ( Bool, PlayerCard ))


create : Int -> List PlayerCard -> SelectionList
create limit xs =
    let
        list : List ( Bool, PlayerCard )
        list =
            List.map (Tuple.pair False) xs
    in
    SelectionList limit list


getLimit : SelectionList -> Int
getLimit (SelectionList limit _) =
    limit


getList : SelectionList -> List ( Bool, PlayerCard )
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


toggle : ( Bool, PlayerCard ) -> ( Bool, PlayerCard )
toggle =
    Tuple.mapFirst not


attemptToggle : Int -> SelectionList -> Maybe SelectionList
attemptToggle index (SelectionList limit list) =
    let
        totalSpent : Int
        totalSpent =
            list
                |> List.filter Tuple.first
                |> List.map (Tuple.second >> PlayerCard.getDestroyCost)
                |> List.foldl (+) 0

        replaceFn : ( Bool, PlayerCard ) -> Maybe ( Bool, PlayerCard )
        replaceFn selectionTuple =
            let
                ( isSelected, playerCard ) =
                    selectionTuple

                limitReached : Bool
                limitReached =
                    totalSpent + PlayerCard.getDestroyCost playerCard > limit
            in
            if limitReached && not isSelected then
                Nothing

            else
                Just (toggle selectionTuple)
    in
    list
        |> attemptReplaceAtIndex index replaceFn
        |> Maybe.map (SelectionList limit)


getUnselected : SelectionList -> List PlayerCard
getUnselected (SelectionList _ list) =
    list
        |> List.filter (Tuple.first >> not)
        |> List.map Tuple.second


type SelectionItemStatus
    = Selected
    | Selectable
    | Unselectable


map : (Int -> SelectionItemStatus -> PlayerCard -> b) -> SelectionList -> List b
map mapFn selectionList =
    case selectionList of
        SelectionList _ list ->
            list
                |> List.indexedMap
                    (\index ( isSelected, item ) ->
                        if isSelected then
                            mapFn index Selected item

                        else if Maybe.Extra.isJust (attemptToggle index selectionList) then
                            mapFn index Selectable item

                        else
                            mapFn index Unselectable item
                    )
                |> List.reverse


getSelected : SelectionList -> List PlayerCard
getSelected (SelectionList _ list) =
    list
        |> List.filterMap
            (\( isSelected, item ) ->
                if isSelected then
                    Just item

                else
                    Nothing
            )
