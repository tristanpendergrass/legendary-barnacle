module PlayerCard exposing
    ( PlayerCard
    , fromAgingCard
    , fromHazardCard
    , fromRobinsonCard
    , getAbility
    , getFightingValue
    , hasAbility
    , isAgingCard
    )

import AgingCard exposing (AgingCard)
import FightStats exposing (SpecialAbility)
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


getAbility : PlayerCard -> Maybe SpecialAbility
getAbility playerCard =
    case playerCard of
        AgingType card ->
            AgingCard.getAbility card

        HazardType card ->
            HazardCard.getAbility card

        RobinsonType card ->
            RobinsonCard.getAbility card


isAgingCard : PlayerCard -> Bool
isAgingCard playerCard =
    case playerCard of
        AgingType _ ->
            True

        _ ->
            False
