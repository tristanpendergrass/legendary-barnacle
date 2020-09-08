module PlayerCard exposing (PlayerCard, fromAgingCard, fromHazardCard, fromRobinsonCard, getFightingValue, hasAbility)

import AgingCard exposing (AgingCard)
import HazardCard exposing (HazardCard)
import RobinsonCard exposing (RobinsonCard)


hasAbility : PlayerCard -> Bool
hasAbility card =
    case card of
        AgingType agingCard ->
            AgingCard.hasAbility agingCard

        HazardType hazardCard ->
            HazardCard.hasAbility hazardCard

        RobinsonType robinsonCard ->
            RobinsonCard.hasAbility robinsonCard


fromAgingCard : AgingCard -> PlayerCard
fromAgingCard =
    AgingType


fromHazardCard : HazardCard -> PlayerCard
fromHazardCard =
    HazardType


fromRobinsonCard : RobinsonCard -> PlayerCard
fromRobinsonCard =
    RobinsonType


type PlayerCard
    = AgingType AgingCard
    | HazardType HazardCard
    | RobinsonType RobinsonCard


getFightingValue : PlayerCard -> Int
getFightingValue playerCard =
    case playerCard of
        AgingType agingCard ->
            AgingCard.getFightingValue agingCard

        HazardType hazardCard ->
            HazardCard.getFightingValue hazardCard

        RobinsonType robinsonCard ->
            RobinsonCard.getFightingValue robinsonCard
