module AgingCard exposing (AgingCard, getAbility, getInitial, getStrength, hasAbility)

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
    AgingCard Normal { title = "very tired", strength = 0, specialAbility = Just StopDrawing }


stupid : AgingCard
stupid =
    AgingCard Normal { title = "stupid", strength = -2, specialAbility = Nothing }


hungry : AgingCard
hungry =
    AgingCard Normal { title = "hungry", strength = 0, specialAbility = Just MinusOneLife }


scared : AgingCard
scared =
    AgingCard Normal { title = "scared", strength = 0, specialAbility = Just HighestCardNull }


distracted : AgingCard
distracted =
    AgingCard Normal { title = "distracted", strength = -1, specialAbility = Nothing }


veryStupid : AgingCard
veryStupid =
    AgingCard Normal { title = "very stupid", strength = -3, specialAbility = Nothing }


moronic : AgingCard
moronic =
    AgingCard Difficult { title = "moronic", strength = -4, specialAbility = Nothing }


suicidal : AgingCard
suicidal =
    AgingCard Difficult { title = "suicidal", strength = -5, specialAbility = Nothing }


veryHungry : AgingCard
veryHungry =
    AgingCard Difficult { title = "very hungry", strength = 0, specialAbility = Just MinusTwoLife }


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


getStrength : AgingCard -> Int
getStrength =
    getFightStats >> .strength
