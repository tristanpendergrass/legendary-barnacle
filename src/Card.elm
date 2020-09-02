module Card exposing (Card, getInitialAgingCards, getInitialHazardCards, getInitialRobinsonCards)

import Random
import Random.List


type Card
    = HazardCard HazardStats FightStats
    | RobinsonCard FightStats
    | AgingCard AgingType FightStats


type alias HazardPhaseValues =
    { green : Int
    , yellow : Int
    , red : Int
    }


type SpecialAbility
    = PlusOneLife
    | PlusTwoLife
    | DrawOne
    | DrawTwo
    | Destroy
    | Double
    | Copy
    | PhaseMinusOne
    | SortThree
    | ExchangeOne
    | ExchangeTwo
    | BelowTheStack
      -- Aging abilities
    | MinusOneLife
    | MinusTwoLife
    | HighestCardNull
    | StopDrawing


type alias FightStats =
    { title : String
    , fightingValue : Int
    , specialAbility : Maybe SpecialAbility
    }


type alias HazardStats =
    { title : String
    , freeCards : Int
    , hazardPhaseValues : HazardPhaseValues
    }


type AgingType
    = Normal
    | Difficult



-- Aging Card Data


veryTired : Card
veryTired =
    AgingCard Normal { title = "very tired", fightingValue = 0, specialAbility = Just StopDrawing }


stupid : Card
stupid =
    AgingCard Normal { title = "stupid", fightingValue = -2, specialAbility = Nothing }


hungry : Card
hungry =
    AgingCard Normal { title = "hungry", fightingValue = 0, specialAbility = Just MinusOneLife }


scared : Card
scared =
    AgingCard Normal { title = "scared", fightingValue = 0, specialAbility = Just HighestCardNull }


distracted : Card
distracted =
    AgingCard Normal { title = "distracted", fightingValue = -1, specialAbility = Nothing }


veryStupid : Card
veryStupid =
    AgingCard Normal { title = "very stupid", fightingValue = -3, specialAbility = Nothing }


moronic : Card
moronic =
    AgingCard Difficult { title = "moronic", fightingValue = -4, specialAbility = Nothing }


suicidal : Card
suicidal =
    AgingCard Difficult { title = "suicidal", fightingValue = -5, specialAbility = Nothing }


veryHungry : Card
veryHungry =
    AgingCard Difficult { title = "very hungry", fightingValue = 0, specialAbility = Just MinusTwoLife }


normalAgingCards : List Card
normalAgingCards =
    [ veryTired
    , stupid
    , stupid
    , hungry
    , scared
    , scared
    , distracted
    ]


difficultAgingCards : List Card
difficultAgingCards =
    [ moronic, suicidal, veryHungry ]


getInitialAgingCards : Random.Generator (List Card)
getInitialAgingCards =
    Random.map2
        List.append
        (Random.List.shuffle normalAgingCards)
        (Random.List.shuffle difficultAgingCards)



-- Robinson Card data


distractedRobinson : Card
distractedRobinson =
    RobinsonCard { title = "distracted", fightingValue = -1, specialAbility = Nothing }


focused : Card
focused =
    RobinsonCard { title = "focused", fightingValue = 1, specialAbility = Nothing }


weak : Card
weak =
    RobinsonCard { title = "weak", fightingValue = 0, specialAbility = Nothing }


genius : Card
genius =
    RobinsonCard { title = "genius", fightingValue = 2, specialAbility = Nothing }


eating : Card
eating =
    RobinsonCard { title = "eating", fightingValue = 0, specialAbility = Just PlusOneLife }


robinsonCards : List Card
robinsonCards =
    List.concat
        [ List.repeat 5 distractedRobinson
        , List.repeat 3 focused
        , List.repeat 7 weak
        , [ genius, eating ]
        ]


getInitialRobinsonCards : Random.Generator (List Card)
getInitialRobinsonCards =
    Random.List.shuffle robinsonCards



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


food : Card
food =
    HazardCard withTheRaft { title = "food", fightingValue = 0, specialAbility = Just PlusOneLife }


hazardCards : List Card
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


getInitialHazardCards : Random.Generator ( ( Card, Card ), List Card )
getInitialHazardCards =
    hazardCards
        |> Random.List.shuffle
        |> Random.map
            (\shuffledList ->
                case shuffledList of
                    first :: second :: rest ->
                        ( ( first, second ), rest )

                    _ ->
                        -- should not be reachable
                        ( ( food, food ), shuffledList )
            )
