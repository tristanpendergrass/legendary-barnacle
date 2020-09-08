module Deck exposing (Deck, DrawTwiceResult(..), createDeck, discard, draw, drawTwice, reshuffle)

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


draw : Deck a -> Random.Generator (Maybe ( a, Deck a ))
draw deck =
    case deck of
        Deck [] [] ->
            Random.constant Nothing

        Deck (topDraw :: rest) discardPile ->
            Random.constant (Just ( topDraw, Deck rest discardPile ))

        Deck [] (topDiscard :: rest) ->
            ( topDiscard, rest )
                |> shuffleNonEmptyList
                |> Random.map
                    (\( topShuffledDiscard, shuffledDiscard ) ->
                        Just ( topShuffledDiscard, Deck shuffledDiscard [] )
                    )


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


drawTwice : Deck a -> Random.Generator (DrawTwiceResult a)
drawTwice deck =
    draw deck
        |> Random.andThen
            (\drawResult ->
                case drawResult of
                    Nothing ->
                        Random.constant NothingDrawn

                    Just ( firstCard, deckWithOneDrawn ) ->
                        draw deckWithOneDrawn
                            |> Random.map
                                (\secondDrawResult ->
                                    case secondDrawResult of
                                        Nothing ->
                                            DrewOne deckWithOneDrawn firstCard

                                        Just ( secondCard, deckWithTwoDrawn ) ->
                                            DrewTwo deckWithTwoDrawn firstCard secondCard
                                )
            )
