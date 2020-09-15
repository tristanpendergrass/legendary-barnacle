module AgingCard exposing (AgingCard, getAbility, getFightingValue, getInitial, hasAbility)

import FightStats exposing (FightStats, SpecialAbility(..))
import Maybe.Extra
import Random
import Random.List


type AgingType
    = Normal
    | Difficult


type AgingCard
    = AgingCard AgingType FightStats



-- Aging Card Data


veryTired : AgingCard
veryTired =
    AgingCard Normal { title = "very tired", fightingValue = 0, specialAbility = Just StopDrawing }


stupid : AgingCard
stupid =
    AgingCard Normal { title = "stupid", fightingValue = -2, specialAbility = Nothing }


hungry : AgingCard
hungry =
    AgingCard Normal { title = "hungry", fightingValue = 0, specialAbility = Just MinusOneLife }


scared : AgingCard
scared =
    AgingCard Normal { title = "scared", fightingValue = 0, specialAbility = Just HighestCardNull }


distracted : AgingCard
distracted =
    AgingCard Normal { title = "distracted", fightingValue = -1, specialAbility = Nothing }


veryStupid : AgingCard
veryStupid =
    AgingCard Normal { title = "very stupid", fightingValue = -3, specialAbility = Nothing }


moronic : AgingCard
moronic =
    AgingCard Difficult { title = "moronic", fightingValue = -4, specialAbility = Nothing }


suicidal : AgingCard
suicidal =
    AgingCard Difficult { title = "suicidal", fightingValue = -5, specialAbility = Nothing }


veryHungry : AgingCard
veryHungry =
    AgingCard Difficult { title = "very hungry", fightingValue = 0, specialAbility = Just MinusTwoLife }


normalAgingCards : List AgingCard
normalAgingCards =
    [ veryTired
    , stupid
    , stupid
    , hungry
    , scared
    , scared
    , distracted
    ]


difficultAgingCards : List AgingCard
difficultAgingCards =
    [ moronic, suicidal, veryHungry ]



-- Utility methods


getInitial : Random.Generator (List AgingCard)
getInitial =
    Random.map2
        List.append
        (Random.List.shuffle normalAgingCards)
        (Random.List.shuffle difficultAgingCards)


getFightStats : AgingCard -> FightStats
getFightStats (AgingCard _ fightStats) =
    fightStats


hasAbility : AgingCard -> Bool
hasAbility =
    getFightStats
        >> .specialAbility
        >> Maybe.Extra.isJust


getAbility : AgingCard -> Maybe SpecialAbility
getAbility =
    getFightStats >> .specialAbility


getFightingValue : AgingCard -> Int
getFightingValue =
    getFightStats >> .fightingValue
