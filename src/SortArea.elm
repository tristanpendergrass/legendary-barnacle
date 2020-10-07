module SortArea exposing
    ( ChangeOrderType(..)
    , DiscardChoiceOne(..)
    , DiscardChoiceThree(..)
    , DiscardChoiceTwo(..)
    , SortArea(..)
    , SortIndex(..)
    , attemptReveal
    , changeOrder
    , create
    , getCards
    , toggleDiscard
    )


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


type SortIndex
    = First
    | Second
    | Third


create : a -> SortArea a
create x =
    OneRevealed x DiscardZeroOfOne


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


toggleDiscard : SortIndex -> SortArea a -> SortArea a
toggleDiscard sortIndex sortArea =
    case ( sortArea, sortIndex ) of
        ( OneRevealed first DiscardZeroOfOne, First ) ->
            OneRevealed first DiscardOneOfOne

        ( OneRevealed first DiscardOneOfOne, First ) ->
            OneRevealed first DiscardZeroOfOne

        ( TwoRevealed first second DiscardZeroOfTwo, First ) ->
            TwoRevealed first second DiscardOneOfTwo

        ( TwoRevealed first second DiscardZeroOfTwo, Second ) ->
            TwoRevealed first second DiscardTwoOfTwo

        ( TwoRevealed first second DiscardOneOfTwo, First ) ->
            TwoRevealed first second DiscardZeroOfTwo

        ( TwoRevealed first second DiscardOneOfTwo, Second ) ->
            TwoRevealed first second DiscardTwoOfTwo

        ( TwoRevealed first second DiscardTwoOfTwo, First ) ->
            TwoRevealed first second DiscardOneOfTwo

        ( TwoRevealed first second DiscardTwoOfTwo, Second ) ->
            TwoRevealed first second DiscardZeroOfTwo

        ( ThreeRevealed first second third DiscardZeroOfThree, First ) ->
            ThreeRevealed first second third DiscardOneOfThree

        ( ThreeRevealed first second third DiscardZeroOfThree, Second ) ->
            ThreeRevealed first second third DiscardTwoOfThree

        ( ThreeRevealed first second third DiscardZeroOfThree, Third ) ->
            ThreeRevealed first second third DiscardThreeOfThree

        ( ThreeRevealed first second third DiscardOneOfThree, First ) ->
            ThreeRevealed first second third DiscardZeroOfThree

        ( ThreeRevealed first second third DiscardOneOfThree, Second ) ->
            ThreeRevealed first second third DiscardTwoOfThree

        ( ThreeRevealed first second third DiscardOneOfThree, Third ) ->
            ThreeRevealed first second third DiscardThreeOfThree

        ( ThreeRevealed first second third DiscardTwoOfThree, First ) ->
            ThreeRevealed first second third DiscardOneOfThree

        ( ThreeRevealed first second third DiscardTwoOfThree, Second ) ->
            ThreeRevealed first second third DiscardZeroOfThree

        ( ThreeRevealed first second third DiscardTwoOfThree, Third ) ->
            ThreeRevealed first second third DiscardThreeOfThree

        ( ThreeRevealed first second third DiscardThreeOfThree, First ) ->
            ThreeRevealed first second third DiscardThreeOfThree

        ( ThreeRevealed first second third DiscardThreeOfThree, Second ) ->
            ThreeRevealed first second third DiscardTwoOfThree

        ( ThreeRevealed first second third DiscardThreeOfThree, Third ) ->
            ThreeRevealed first second third DiscardZeroOfThree

        _ ->
            sortArea


createTwoRevealed : a -> DiscardChoiceOne -> a -> SortArea a
createTwoRevealed first discardChoice second =
    case discardChoice of
        DiscardZeroOfOne ->
            TwoRevealed first second DiscardZeroOfTwo

        DiscardOneOfOne ->
            TwoRevealed first second DiscardOneOfTwo


createThreeRevealed : a -> a -> DiscardChoiceTwo -> a -> SortArea a
createThreeRevealed first second discardChoice third =
    case discardChoice of
        DiscardZeroOfTwo ->
            ThreeRevealed first second third DiscardZeroOfThree

        DiscardOneOfTwo ->
            ThreeRevealed first second third DiscardOneOfThree

        DiscardTwoOfTwo ->
            ThreeRevealed first second third DiscardTwoOfThree


attemptReveal : SortArea a -> Maybe (a -> SortArea a)
attemptReveal sortArea =
    case sortArea of
        OneRevealed first discardChoice ->
            Just (createTwoRevealed first discardChoice)

        TwoRevealed first second discardChoice ->
            Just (createThreeRevealed first second discardChoice)

        ThreeRevealed _ _ _ _ ->
            Nothing
