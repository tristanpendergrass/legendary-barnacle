module Deck exposing (Deck, DrawTwiceResult(..), createDeck, discard, draw, drawTwice, drawTwiceWithReshuffle, drawWithReshuffle, putOnDrawPile, reshuffle)

import Random
import Random.List


type Deck a
    = Deck (List a) (List a)


createDeck : List a -> Deck a
createDeck drawPile =
    Deck drawPile []


shuffleNonEmptyList : ( a, List a ) -> Random.Generator ( a, List a )
shuffleNonEmptyList ( head, rest ) =
    (head :: rest)
        |> Random.List.shuffle
        |> Random.map
            (\shuffledList ->
                case shuffledList of
                    [] ->
                        ( head, [] )

                    headShuffled :: restShuffled ->
                        ( headShuffled, restShuffled )
            )


drawWithReshuffle : Deck a -> List a -> Random.Generator (Maybe ( a, Deck a, List a ))
drawWithReshuffle deck cardsToAdd =
    case ( deck, cardsToAdd ) of
        ( Deck [] [], _ ) ->
            Random.constant Nothing

        ( Deck (topDraw :: rest) discardPile, _ ) ->
            Random.constant (Just ( topDraw, Deck rest discardPile, cardsToAdd ))

        ( Deck [] (topDiscard :: rest), [] ) ->
            ( topDiscard, rest )
                |> shuffleNonEmptyList
                |> Random.map
                    (\( topShuffledDiscard, shuffledDiscard ) ->
                        Just ( topShuffledDiscard, Deck shuffledDiscard [], cardsToAdd )
                    )

        ( Deck [] (topDiscard :: rest), cardToAdd :: restCardsToAdd ) ->
            ( cardToAdd, topDiscard :: rest )
                |> shuffleNonEmptyList
                |> Random.map
                    (\( topShuffledDiscard, shuffledDiscard ) ->
                        Just ( topShuffledDiscard, Deck shuffledDiscard [], restCardsToAdd )
                    )


draw : Deck a -> Maybe ( a, Deck a )
draw deck =
    case deck of
        Deck [] _ ->
            Nothing

        Deck (drawnCard :: rest) discardPile ->
            Just ( drawnCard, Deck rest discardPile )


drawTwice : Deck a -> DrawTwiceResult a
drawTwice deck =
    case deck of
        Deck [] _ ->
            NothingDrawn

        Deck [ first ] discardPile ->
            DrewOne (Deck [] discardPile) first

        Deck (first :: second :: rest) discardPile ->
            DrewTwo (Deck rest discardPile) first second


discard : List a -> Deck a -> Deck a
discard cards (Deck drawPile discardPile) =
    Deck drawPile (List.append cards discardPile)


reshuffle : Deck a -> Random.Generator (Deck a)
reshuffle (Deck drawPile discardPile) =
    List.append discardPile drawPile
        |> Random.List.shuffle
        |> Random.map createDeck


type DrawTwiceResult a
    = DrewTwo (Deck a) a a
    | DrewOne (Deck a) a
    | NothingDrawn


drawTwiceWithReshuffle : Deck a -> Random.Generator (DrawTwiceResult a)
drawTwiceWithReshuffle deck =
    drawWithReshuffle deck
        |> Random.andThen
            (\drawResult ->
                case drawResult of
                    Nothing ->
                        Random.constant NothingDrawn

                    Just ( firstCard, deckWithOneDrawn ) ->
                        drawWithReshuffle deckWithOneDrawn
                            |> Random.map
                                (\secondDrawResult ->
                                    case secondDrawResult of
                                        Nothing ->
                                            DrewOne deckWithOneDrawn firstCard

                                        Just ( secondCard, deckWithTwoDrawn ) ->
                                            DrewTwo deckWithTwoDrawn firstCard secondCard
                                )
            )


putOnDrawPile : List a -> Deck a -> Deck a
putOnDrawPile newCards (Deck drawPile discardPile) =
    Deck (List.append newCards drawPile) discardPile
