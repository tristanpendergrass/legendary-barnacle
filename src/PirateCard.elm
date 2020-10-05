module PirateCard exposing (PirateCard, getFreeCards, getStrength, getTwoPirates)

import Random
import Random.List


type PirateCard
    = Normal { hazardValue : Int, freeCards : Int }



-- | DrawCostsLife
-- | HalfCardsAreNull
-- | CardsGetPlusOneValue
-- | FightAllRemainingHazards
-- | PlusTwoHazardPointsPerAgingCard
-- Pirate Card Data


normal1 : PirateCard
normal1 =
    Normal { hazardValue = 30, freeCards = 8 }


normal2 : PirateCard
normal2 =
    Normal { hazardValue = 35, freeCards = 9 }


pirateCards : List PirateCard
pirateCards =
    [ normal1
    , normal2
    , Normal { hazardValue = 20, freeCards = 6 }
    , Normal { hazardValue = 25, freeCards = 7 }
    , Normal { hazardValue = 40, freeCards = 10 }

    -- , DrawCostsLife
    -- , PlusTwoHazardPointsPerAgingCard
    -- , CardsGetPlusOneValue
    -- , HalfCardsAreNull
    -- , FightAllRemainingHazards
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
                        ( normal1, normal2 )
            )


getStrength : PirateCard -> Int
getStrength pirateCard =
    case pirateCard of
        Normal { hazardValue } ->
            hazardValue


getFreeCards : PirateCard -> Int
getFreeCards pirateCard =
    case pirateCard of
        Normal { freeCards } ->
            freeCards
