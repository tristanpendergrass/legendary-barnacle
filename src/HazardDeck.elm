module HazardDeck exposing
    ( DrawTwiceResult(..)
    , HazardDeck
    , create
    , discard
    , discardPileCount
    , draw
    , drawPileCount
    , drawTwice
    , reshuffle
    )

import HazardCard exposing (HazardCard)
import Random
import Random.List


type HazardDeck
    = HazardDeck (List HazardCard) (List HazardCard)


create : List HazardCard -> HazardDeck
create drawPile =
    HazardDeck drawPile []


draw : HazardDeck -> Maybe ( HazardCard, HazardDeck )
draw deck =
    case deck of
        HazardDeck [] _ ->
            Nothing

        HazardDeck (drawnCard :: rest) discardPile ->
            Just ( drawnCard, HazardDeck rest discardPile )


drawTwice : HazardDeck -> DrawTwiceResult
drawTwice deck =
    case deck of
        HazardDeck [] _ ->
            NothingDrawn

        HazardDeck [ first ] discardPile ->
            DrewOne (HazardDeck [] discardPile) first

        HazardDeck (first :: second :: rest) discardPile ->
            DrewTwo (HazardDeck rest discardPile) first second


discard : List HazardCard -> HazardDeck -> HazardDeck
discard cards (HazardDeck drawPile discardPile) =
    HazardDeck drawPile (List.append cards discardPile)


type DrawTwiceResult
    = DrewTwo HazardDeck HazardCard HazardCard
    | DrewOne HazardDeck HazardCard
    | NothingDrawn


reshuffle : HazardDeck -> Random.Generator HazardDeck
reshuffle (HazardDeck drawPile discardPile) =
    List.append discardPile drawPile
        |> Random.List.shuffle
        |> Random.map create


drawPileCount : HazardDeck -> Int
drawPileCount (HazardDeck drawPile _) =
    List.length drawPile


discardPileCount : HazardDeck -> Int
discardPileCount (HazardDeck _ discardPile) =
    List.length discardPile
