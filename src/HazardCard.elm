module HazardCard exposing (HazardCard, getAbility, getFightingValue, getFreeCards, getGreenValue, getInitial, getRedValue, getYellowValue, hasAbility)

import FightStats exposing (FightStats, SpecialAbility(..))
import Maybe.Extra
import Random
import Random.List


type HazardCard
    = HazardCard HazardStats FightStats


type alias HazardPhaseValues =
    { green : Int
    , yellow : Int
    , red : Int
    }


type alias HazardStats =
    { title : String
    , freeCards : Int
    , hazardPhaseValues : HazardPhaseValues
    }


getGreenValue : HazardCard -> Int
getGreenValue (HazardCard { hazardPhaseValues } _) =
    hazardPhaseValues.green


getYellowValue : HazardCard -> Int
getYellowValue (HazardCard { hazardPhaseValues } _) =
    hazardPhaseValues.yellow


getRedValue : HazardCard -> Int
getRedValue (HazardCard { hazardPhaseValues } _) =
    hazardPhaseValues.red



-- Hazard Stats


withTheRaft : HazardStats
withTheRaft =
    { title = "With the raft to the wreck", freeCards = 1, hazardPhaseValues = { green = 0, yellow = 1, red = 3 } }


exploringTheIsland : HazardStats
exploringTheIsland =
    { title = "Exploring the island", freeCards = 2, hazardPhaseValues = { green = 1, yellow = 3, red = 6 } }


furtherExploringTheIsland : HazardStats
furtherExploringTheIsland =
    { title = "Further exploring the island", freeCards = 3, hazardPhaseValues = { green = 2, yellow = 5, red = 8 } }


wildAnimals : HazardStats
wildAnimals =
    { title = "Wild Animals", freeCards = 4, hazardPhaseValues = { green = 4, yellow = 7, red = 11 } }


cannibals : HazardStats
cannibals =
    { title = "Cannibals", freeCards = 5, hazardPhaseValues = { green = 5, yellow = 9, red = 14 } }



-- Hazard Card data


food : HazardCard
food =
    HazardCard withTheRaft { title = "food", fightingValue = 0, specialAbility = Just PlusOneLife }


hazardCards : List HazardCard
hazardCards =
    [ food
    , food
    , HazardCard withTheRaft { title = "realization", fightingValue = 0, specialAbility = Just Destroy }
    , HazardCard withTheRaft { title = "deception", fightingValue = 0, specialAbility = Just BelowTheStack }
    , HazardCard withTheRaft { title = "mimicry", fightingValue = 0, specialAbility = Just Copy }
    , HazardCard withTheRaft { title = "strategy", fightingValue = 0, specialAbility = Just ExchangeTwo }
    , HazardCard withTheRaft { title = "strategy", fightingValue = 0, specialAbility = Just ExchangeTwo }
    , HazardCard withTheRaft { title = "equipment", fightingValue = 0, specialAbility = Just DrawTwo }
    , HazardCard withTheRaft { title = "equipment", fightingValue = 0, specialAbility = Just DrawTwo }
    , HazardCard withTheRaft { title = "books", fightingValue = 0, specialAbility = Just PhaseMinusOne }
    , HazardCard exploringTheIsland { title = "food", fightingValue = 1, specialAbility = Just PlusOneLife }
    , HazardCard exploringTheIsland { title = "food", fightingValue = 1, specialAbility = Just PlusOneLife }
    , HazardCard exploringTheIsland { title = "realization", fightingValue = 1, specialAbility = Just Destroy }
    , HazardCard exploringTheIsland { title = "deception", fightingValue = 1, specialAbility = Just BelowTheStack }
    , HazardCard exploringTheIsland { title = "repeat", fightingValue = 1, specialAbility = Just Double }
    , HazardCard exploringTheIsland { title = "weapon", fightingValue = 2, specialAbility = Nothing }
    , HazardCard exploringTheIsland { title = "weapon", fightingValue = 2, specialAbility = Nothing }
    , HazardCard exploringTheIsland { title = "mimicry", fightingValue = 1, specialAbility = Just Copy }
    , HazardCard furtherExploringTheIsland { title = "food", fightingValue = 2, specialAbility = Just PlusOneLife }
    , HazardCard furtherExploringTheIsland { title = "realization", fightingValue = 2, specialAbility = Just Destroy }
    , HazardCard furtherExploringTheIsland { title = "strategy", fightingValue = 2, specialAbility = Just ExchangeOne }
    , HazardCard furtherExploringTheIsland { title = "experience", fightingValue = 2, specialAbility = Just DrawOne }
    , HazardCard furtherExploringTheIsland { title = "repeat", fightingValue = 2, specialAbility = Just Double }
    , HazardCard furtherExploringTheIsland { title = "vision", fightingValue = 2, specialAbility = Just SortThree }
    , HazardCard wildAnimals { title = "realization", fightingValue = 3, specialAbility = Just Destroy }
    , HazardCard wildAnimals { title = "vision", fightingValue = 3, specialAbility = Just SortThree }
    , HazardCard wildAnimals { title = "experience", fightingValue = 3, specialAbility = Just DrawOne }
    , HazardCard wildAnimals { title = "strategy", fightingValue = 3, specialAbility = Just ExchangeOne }
    , HazardCard cannibals { title = "weapon", fightingValue = 4, specialAbility = Nothing }
    , HazardCard cannibals { title = "weapon", fightingValue = 4, specialAbility = Nothing }
    ]


getInitial : ( HazardCard, HazardCard, List HazardCard )
getInitial =
    ( food, food, hazardCards )



-- getInitial : Random.Generator ( ( HazardCard, HazardCard ), List HazardCard )
-- getInitial =
--     hazardCards
--         |> Random.List.shuffle
--         |> Random.map
--             (\shuffledList ->
--                 case shuffledList of
--                     first :: second :: rest ->
--                         ( ( first, second ), rest )
--                     _ ->
--                         -- should not be reachable
--                         ( ( food, food ), shuffledList )
--             )


getFreeCards : HazardCard -> Int
getFreeCards (HazardCard { freeCards } _) =
    freeCards


getFightStats : HazardCard -> FightStats
getFightStats (HazardCard _ fightStats) =
    fightStats


hasAbility : HazardCard -> Bool
hasAbility =
    getFightStats
        >> .specialAbility
        >> Maybe.Extra.isJust


getAbility : HazardCard -> Maybe SpecialAbility
getAbility =
    getFightStats >> .specialAbility


getFightingValue : HazardCard -> Int
getFightingValue =
    getFightStats >> .fightingValue
