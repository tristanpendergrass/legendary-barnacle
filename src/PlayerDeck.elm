module PlayerDeck exposing (PlayerDeck, create, discard, draw, putOnTop)

import AgingCard exposing (AgingCard)
import PlayerCard exposing (PlayerCard)
import Random
import Random.List


type PlayerDeck
    = PlayerDeck (List PlayerCard) (List PlayerCard) (List AgingCard)



-- Public functions


create : List AgingCard -> List PlayerCard -> PlayerDeck
create agingCards drawPile =
    PlayerDeck drawPile [] agingCards


{-| Returns the top card of the draw pile.
If draw pile is empty: add top aging card if available to discard pile, shuffle and draw from that.
If discard pile is also empty: return Nothing
-}
draw : PlayerDeck -> Random.Generator (Maybe ( PlayerCard, PlayerDeck ))
draw playerDeck =
    case playerDeck of
        PlayerDeck [] [] _ ->
            Random.constant Nothing

        PlayerDeck (topDraw :: rest) discardPile agingCards ->
            Random.constant (Just ( topDraw, PlayerDeck rest discardPile agingCards ))

        PlayerDeck [] (topDiscard :: rest) [] ->
            ( topDiscard, rest )
                |> shuffleNonEmptyList
                |> Random.map
                    (\( topShuffledDiscard, shuffledDiscard ) ->
                        Just ( topShuffledDiscard, PlayerDeck shuffledDiscard [] [] )
                    )

        PlayerDeck [] (topDiscard :: rest) (topAgingCard :: restAgingCards) ->
            ( topDiscard, PlayerCard.fromAgingCard topAgingCard :: rest )
                |> shuffleNonEmptyList
                |> Random.map
                    (\( drawnCard, shuffledCards ) ->
                        Just ( drawnCard, PlayerDeck shuffledCards [] restAgingCards )
                    )


discard : List PlayerCard -> PlayerDeck -> PlayerDeck
discard cards (PlayerDeck drawPile discardPile agingCards) =
    PlayerDeck drawPile (List.append cards discardPile) agingCards


putOnTop : List PlayerCard -> PlayerDeck -> PlayerDeck
putOnTop cards (PlayerDeck drawPile discardPile agingCards) =
    PlayerDeck (List.append cards drawPile) discardPile agingCards



-- Private functions


shuffleNonEmptyList : ( a, List a ) -> Random.Generator ( a, List a )
shuffleNonEmptyList ( head, rest ) =
    (head :: rest)
        |> Random.List.shuffle
        |> Random.map
            (\shuffledList ->
                case shuffledList of
                    [] ->
                        -- should never happen
                        ( head, [] )

                    headShuffled :: restShuffled ->
                        ( headShuffled, restShuffled )
            )
