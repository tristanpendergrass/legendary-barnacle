module RobinsonCard exposing (RobinsonCard, getAbility, getFightingValue, getInitial, hasAbility)

import FightStats exposing (FightStats, SpecialAbility(..))
import Maybe.Extra


type RobinsonCard
    = RobinsonCard FightStats



-- Robinson Card data


distractedRobinson : RobinsonCard
distractedRobinson =
    RobinsonCard { title = "distracted", fightingValue = -1, specialAbility = Nothing }


focused : RobinsonCard
focused =
    RobinsonCard { title = "focused", fightingValue = 1, specialAbility = Nothing }


weak : RobinsonCard
weak =
    RobinsonCard { title = "weak", fightingValue = 0, specialAbility = Nothing }


genius : RobinsonCard
genius =
    RobinsonCard { title = "genius", fightingValue = 2, specialAbility = Nothing }


eating : RobinsonCard
eating =
    RobinsonCard { title = "eating", fightingValue = 0, specialAbility = Just PlusOneLife }


getInitial : List RobinsonCard
getInitial =
    List.concat
        [ List.repeat 5 distractedRobinson
        , List.repeat 3 focused
        , List.repeat 7 weak
        , [ genius, eating ]
        ]


getFightStats : RobinsonCard -> FightStats
getFightStats (RobinsonCard fightStats) =
    fightStats


hasAbility : RobinsonCard -> Bool
hasAbility =
    getFightStats
        >> .specialAbility
        >> Maybe.Extra.isJust


getAbility : RobinsonCard -> Maybe SpecialAbility
getAbility =
    getFightStats >> .specialAbility


getFightingValue : RobinsonCard -> Int
getFightingValue =
    getFightStats >> .fightingValue
