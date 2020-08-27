module PirateCard exposing (PirateCard)


type alias NormalPirateStats =
    { hazardValue : Int
    , freeCards : Int
    }


type PirateCard
    = Normal NormalPirateStats
    | DrawCostsLife NormalPirateStats
    | HalfCardsAreNull NormalPirateStats
    | CardsGetPlusOneValue NormalPirateStats
    | FightAllRemainingHazards
    | PlusTwoHazardPointsPerAgingCard NormalPirateStats
