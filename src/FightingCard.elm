module FightingCard exposing (FightingCard)


type FightingCard
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
    | DrawOneCard
    | DrawTwoCards
    | Destroy
    | Double
    | Copy
    | PhaseMinusOne
    | SortThreeCards
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
    , destructionCost : Int
    }


type alias HazardStats =
    { title : String
    , freeCards : Int
    , hazardPhaseValues : HazardPhaseValues
    }


type AgingType
    = Normal
    | Difficult


veryTired : FightingCard
veryTired =
    AgingCard Normal { title = "very tired", fightingValue = 0, specialAbility = Just StopDrawing, destructionCost = 2 }


stupid : FightingCard
stupid =
    AgingCard Normal { title = "stupid", fightingValue = -2, specialAbility = Nothing, destructionCost = 2 }


stupid2 : FightingCard
stupid2 =
    AgingCard Normal { title = "stupid", fightingValue = -2, specialAbility = Nothing, destructionCost = 2 }


hungry : FightingCard
hungry =
    AgingCard Normal { title = "hungry", fightingValue = 0, specialAbility = Just MinusOneLife, destructionCost = 2 }


scared : FightingCard
scared =
    AgingCard Normal { title = "scared", fightingValue = 0, specialAbility = Just HighestCardNull, destructionCost = 2 }


scared2 : FightingCard
scared2 =
    AgingCard Normal { title = "scared", fightingValue = 0, specialAbility = Just HighestCardNull, destructionCost = 2 }


distracted : FightingCard
distracted =
    AgingCard Normal { title = "distracted", fightingValue = -1, specialAbility = Nothing, destructionCost = 2 }


veryStupid : FightingCard
veryStupid =
    AgingCard Normal { title = "very stupid", fightingValue = -3, specialAbility = Nothing, destructionCost = 2 }


moronic : FightingCard
moronic =
    AgingCard Difficult { title = "moronic", fightingValue = -4, specialAbility = Nothing, destructionCost = -2 }


suicidal : FightingCard
suicidal =
    AgingCard Difficult { title = "suicidal", fightingValue = -5, specialAbility = Nothing, destructionCost = -2 }


veryHungry : FightingCard
veryHungry =
    AgingCard Difficult { title = "very hungry", fightingValue = 0, specialAbility = Just MinusTwoLife, destructionCost = -2 }


getInitialAgingCards : { normalAgingCards : List FightingCard, difficultAgingCards : List FightingCard }
getInitialAgingCards =
    { normalAgingCards =
        [ veryTired
        , stupid
        , stupid2
        , hungry
        , scared
        , scared2
        , distracted
        ]
    , difficultAgingCards = [ moronic, suicidal, veryHungry ]
    }
