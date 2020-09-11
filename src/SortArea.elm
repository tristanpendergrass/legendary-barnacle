module SortArea exposing (ChangeOrderType, DiscardType, SortArea, getCards, getDiscard)

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
    | TwoToOne
    | TwoToThree
    | ThreeToOne
    | ThreeToTwo


type DiscardType
    = First
    | Second
    | Third


getCards : SortArea a -> List a
getCards sortArea =
    case sortArea of
        OneRevealed first _ ->
            [ first ]

        TwoRevealed first second _ ->
            [ first, second ]

        ThreeRevealed first second third _ ->
            [ first, second, third ]


getDiscard : SortArea a -> Maybe a
getDiscard sortArea =
    case sortArea of
        OneRevealed card DiscardOneOfOne ->
            Just card

        TwoRevealed first _ DiscardOneOfTwo ->
            Just first

        TwoRevealed _ second DiscardTwoOfTwo ->
            Just second

        ThreeRevealed first _ _ DiscardOneOfThree ->
            Just first

        ThreeRevealed _ second _ DiscardTwoOfThree ->
            Just second

        ThreeRevealed _ _ third DiscardThreeOfThree ->
            Just third

        _ ->
            Nothing
