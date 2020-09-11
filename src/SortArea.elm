module SortArea exposing (ChangeOrderType, DiscardType, SortArea, changeOrder, getCards)

import PlayerCard exposing (PlayerCard)


type SortArea a
    = OneRevealed a DiscardChoiceOne
    | TwoRevealed a a DiscardChoiceTwo
    | ThreeRevealed a a a DiscardChoiceThree


type DiscardChoiceOne
    = DiscardZeroOfOne
    | DiscardOneOfOne


type DiscardChoiceTwo
    = DiscardZeroOfTwo
    | DiscardOneOfTwo
    | DiscardTwoOfTwo


type DiscardChoiceThree
    = DiscardZeroOfThree
    | DiscardOneOfThree
    | DiscardTwoOfThree
    | DiscardThreeOfThree


type ChangeOrderType
    = OneToTwo
    | OneToThree
    | TwoToThree


type DiscardType
    = First
    | Second
    | Third


getCards : SortArea a -> { cardsToKeep : List a, cardsToDiscard : List a }
getCards sortArea =
    case sortArea of
        OneRevealed first DiscardZeroOfOne ->
            { cardsToKeep = [ first ], cardsToDiscard = [] }

        OneRevealed first DiscardOneOfOne ->
            { cardsToKeep = [], cardsToDiscard = [ first ] }

        TwoRevealed first second DiscardZeroOfTwo ->
            { cardsToKeep = [ first, second ], cardsToDiscard = [] }

        TwoRevealed first second DiscardOneOfTwo ->
            { cardsToKeep = [ second ], cardsToDiscard = [ first ] }

        TwoRevealed first second DiscardTwoOfTwo ->
            { cardsToKeep = [ first ], cardsToDiscard = [ second ] }

        ThreeRevealed first second third DiscardZeroOfThree ->
            { cardsToKeep = [ first, second, third ], cardsToDiscard = [] }

        ThreeRevealed first second third DiscardOneOfThree ->
            { cardsToKeep = [ second, third ], cardsToDiscard = [ first ] }

        ThreeRevealed first second third DiscardTwoOfThree ->
            { cardsToKeep = [ first, third ], cardsToDiscard = [ second ] }

        ThreeRevealed first second third DiscardThreeOfThree ->
            { cardsToKeep = [ first, second ], cardsToDiscard = [ third ] }


changeOrder : ChangeOrderType -> SortArea a -> SortArea a
changeOrder changeOrderType sortArea =
    case ( sortArea, changeOrderType ) of
        ( TwoRevealed first second DiscardZeroOfTwo, OneToTwo ) ->
            TwoRevealed second first DiscardZeroOfTwo

        ( ThreeRevealed first second third DiscardZeroOfThree, OneToTwo ) ->
            ThreeRevealed second first third DiscardZeroOfThree

        ( ThreeRevealed first second third DiscardThreeOfThree, OneToTwo ) ->
            ThreeRevealed second first third DiscardThreeOfThree

        ( ThreeRevealed first second third DiscardZeroOfThree, OneToThree ) ->
            ThreeRevealed third second first DiscardZeroOfThree

        ( ThreeRevealed first second third DiscardTwoOfThree, OneToThree ) ->
            ThreeRevealed third second first DiscardTwoOfThree

        ( ThreeRevealed first second third DiscardZeroOfThree, TwoToThree ) ->
            ThreeRevealed first third second DiscardZeroOfThree

        ( ThreeRevealed first second third DiscardOneOfThree, TwoToThree ) ->
            ThreeRevealed first third second DiscardOneOfThree

        _ ->
            sortArea
