module FightStats exposing (FightStats, SpecialAbility(..))


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
    , strength : Int
    , specialAbility : Maybe SpecialAbility
    }
