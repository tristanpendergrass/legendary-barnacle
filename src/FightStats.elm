module FightStats exposing (FightStats, SpecialAbility(..), getAbilityLabel)


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


getAbilityLabel : SpecialAbility -> String
getAbilityLabel ability =
    case ability of
        PlusOneLife ->
            "+1 life"

        PlusTwoLife ->
            "+2 life"

        DrawOne ->
            "draw"

        DrawTwo ->
            "draw 2"

        Destroy ->
            "destroy"

        Double ->
            "double"

        Copy ->
            "copy"

        PhaseMinusOne ->
            "phase -1"

        SortThree ->
            "sort 3"

        ExchangeOne ->
            "exchange"

        ExchangeTwo ->
            "exchange 2"

        BelowTheStack ->
            "below the stack"

        -- Aging abilities
        MinusOneLife ->
            "-1 life"

        MinusTwoLife ->
            "-2 life"

        HighestCardNull ->
            "highest card = 0"

        StopDrawing ->
            "stop free draws"
