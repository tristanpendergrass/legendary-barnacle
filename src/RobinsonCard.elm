module RobinsonCard exposing (RobinsonCard, getAbility, getInitial, getStrength, getTitle, hasAbility)

import FightStats exposing (FightStats, SpecialAbility(..))
import Maybe.Extra


type RobinsonCard
    = RobinsonCard FightStats



-- Robinson Card data


distractedRobinson : RobinsonCard
distractedRobinson =
    RobinsonCard { title = "distracted", strength = -1, specialAbility = Nothing }


focused : RobinsonCard
focused =
    RobinsonCard { title = "focused", strength = 1, specialAbility = Nothing }


weak : RobinsonCard
weak =
    RobinsonCard { title = "weak", strength = 0, specialAbility = Nothing }


genius : RobinsonCard
genius =
    RobinsonCard { title = "genius", strength = 2, specialAbility = Nothing }


eating : RobinsonCard
eating =
    RobinsonCard { title = "eating", strength = 0, specialAbility = Just PlusOneLife }


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


getStrength : RobinsonCard -> Int
getStrength =
    getFightStats >> .strength


getTitle : RobinsonCard -> String
getTitle =
    getFightStats >> .title
