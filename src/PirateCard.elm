module PirateCard exposing (PirateCard, getTwoPirates)

import Random
import Random.List


type PirateCard
    = Normal { hazardValue : Int, freeCards : Int }
    | DrawCostsLife
    | HalfCardsAreNull
    | CardsGetPlusOneValue
    | FightAllRemainingHazards
    | PlusTwoHazardPointsPerAgingCard



-- Pirate Card Data


pirateCards : List PirateCard
pirateCards =
    [ Normal { hazardValue = 8, freeCards = 30 }
    , Normal { hazardValue = 9, freeCards = 35 }
    , Normal { hazardValue = 20, freeCards = 6 }
    , Normal { hazardValue = 25, freeCards = 7 }
    , Normal { hazardValue = 40, freeCards = 10 }
    , DrawCostsLife
    , PlusTwoHazardPointsPerAgingCard
    , CardsGetPlusOneValue
    , HalfCardsAreNull
    , FightAllRemainingHazards
    ]


getTwoPirates : Random.Generator ( PirateCard, PirateCard )
getTwoPirates =
    pirateCards
        |> Random.List.shuffle
        |> Random.map
            (\shuffledPirateCards ->
                case shuffledPirateCards of
                    first :: second :: _ ->
                        ( first, second )

                    _ ->
                        -- should never reach this condition, return two arbitrarily chosen cards
                        ( DrawCostsLife, PlusTwoHazardPointsPerAgingCard )
            )
