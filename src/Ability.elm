module Ability exposing (Ability)


type Ability
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


type AbilityMsg
    = UseAbility Ability
