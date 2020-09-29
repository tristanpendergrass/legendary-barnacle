module PlayerCard exposing
    ( PlayerCard
    , fromAgingCard
    , fromHazardCard
    , fromRobinsonCard
    , getAbility
    , getStrength
    , getTitle
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


getStrength : PlayerCard -> Int
getStrength playerCard =
    case playerCard of
        AgingType agingCard ->
            AgingCard.getStrength agingCard

        HazardType hazardCard ->
            HazardCard.getStrength hazardCard

        RobinsonType robinsonCard ->
            RobinsonCard.getStrength robinsonCard


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


getTitle : PlayerCard -> String
getTitle playerCard =
    case playerCard of
        AgingType card ->
            AgingCard.getTitle card

        HazardType card ->
            HazardCard.getFightingTitle card

        RobinsonType card ->
            RobinsonCard.getTitle card
