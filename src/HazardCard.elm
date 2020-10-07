module HazardCard exposing
    ( HazardCard
    , getAbility
    , getDummy
    , getFightingTitle
    , getFreeCards
    , getGreenValue
    , getInitial
    , getRedValue
    , getStrength
    , getTestCards
    , getTitle
    , getYellowValue
    , hasAbility
    )

import FightStats exposing (FightStats, SpecialAbility(..))
import Maybe.Extra


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


getTestCards : List HazardCard
getTestCards =
    [ HazardCard furtherExploringTheIsland { title = "vision", strength = 2, specialAbility = Just SortThree }
    , HazardCard withTheRaft { title = "realization", strength = 0, specialAbility = Just Destroy }
    , HazardCard exploringTheIsland { title = "repeat", strength = 1, specialAbility = Just Double }
    , HazardCard exploringTheIsland { title = "repeat", strength = 1, specialAbility = Just Double }
    , food
    , food
    ]


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
    HazardCard withTheRaft { title = "food", strength = 0, specialAbility = Just PlusOneLife }


hazardCards : List HazardCard
hazardCards =
    [ HazardCard withTheRaft { title = "realization", strength = 0, specialAbility = Just Destroy }
    , HazardCard withTheRaft { title = "deception", strength = 0, specialAbility = Just BelowTheStack }
    , HazardCard withTheRaft { title = "mimicry", strength = 0, specialAbility = Just Copy }
    , HazardCard withTheRaft { title = "strategy", strength = 0, specialAbility = Just ExchangeTwo }
    , HazardCard withTheRaft { title = "strategy", strength = 0, specialAbility = Just ExchangeTwo }
    , HazardCard withTheRaft { title = "equipment", strength = 0, specialAbility = Just DrawTwo }
    , HazardCard withTheRaft { title = "equipment", strength = 0, specialAbility = Just DrawTwo }
    , HazardCard withTheRaft { title = "books", strength = 0, specialAbility = Just PhaseMinusOne }
    , HazardCard exploringTheIsland { title = "food", strength = 1, specialAbility = Just PlusOneLife }
    , HazardCard exploringTheIsland { title = "food", strength = 1, specialAbility = Just PlusOneLife }
    , HazardCard exploringTheIsland { title = "realization", strength = 1, specialAbility = Just Destroy }
    , HazardCard exploringTheIsland { title = "deception", strength = 1, specialAbility = Just BelowTheStack }
    , HazardCard exploringTheIsland { title = "repeat", strength = 1, specialAbility = Just Double }
    , HazardCard exploringTheIsland { title = "weapon", strength = 2, specialAbility = Nothing }
    , HazardCard exploringTheIsland { title = "weapon", strength = 2, specialAbility = Nothing }
    , HazardCard exploringTheIsland { title = "mimicry", strength = 1, specialAbility = Just Copy }
    , HazardCard furtherExploringTheIsland { title = "food", strength = 2, specialAbility = Just PlusOneLife }
    , HazardCard furtherExploringTheIsland { title = "realization", strength = 2, specialAbility = Just Destroy }
    , HazardCard furtherExploringTheIsland { title = "strategy", strength = 2, specialAbility = Just ExchangeOne }
    , HazardCard furtherExploringTheIsland { title = "experience", strength = 2, specialAbility = Just DrawOne }
    , HazardCard furtherExploringTheIsland { title = "repeat", strength = 2, specialAbility = Just Double }
    , HazardCard furtherExploringTheIsland { title = "vision", strength = 2, specialAbility = Just SortThree }
    , HazardCard wildAnimals { title = "realization", strength = 3, specialAbility = Just Destroy }
    , HazardCard wildAnimals { title = "vision", strength = 3, specialAbility = Just SortThree }
    , HazardCard wildAnimals { title = "experience", strength = 3, specialAbility = Just DrawOne }
    , HazardCard wildAnimals { title = "strategy", strength = 3, specialAbility = Just ExchangeOne }
    , HazardCard cannibals { title = "weapon", strength = 4, specialAbility = Nothing }
    , HazardCard cannibals { title = "weapon", strength = 4, specialAbility = Nothing }
    ]


getInitial : ( HazardCard, HazardCard, List HazardCard )
getInitial =
    ( food, food, hazardCards )


getDummy : HazardCard
getDummy =
    food



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


hasAbility : HazardCard -> Bool
hasAbility (HazardCard _ { specialAbility }) =
    Maybe.Extra.isJust specialAbility


getAbility : HazardCard -> Maybe SpecialAbility
getAbility (HazardCard _ { specialAbility }) =
    specialAbility


getStrength : HazardCard -> Int
getStrength (HazardCard _ { strength }) =
    strength


getTitle : HazardCard -> String
getTitle (HazardCard { title } _) =
    title


getFightingTitle : HazardCard -> String
getFightingTitle (HazardCard _ { title }) =
    title
