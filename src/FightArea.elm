module FightArea exposing
    ( FightArea
    , canDrawFreeCard
    , createFightArea
    , getHazard
    , getHazardStrength
    , getLeftCards
    , getPlayerStrength
    , getRightCards
    , playOnLeft
    , playOnRight
    )

import HazardCard exposing (HazardCard)
import Phase exposing (Phase(..))
import PlayerCard exposing (PlayerCard)


type PlayedCard
    = NormalCard PlayerCard
    | AbilityCard PlayerCard Bool


type FightArea
    = FightArea HazardCard (List PlayedCard) (List PlayedCard)


toPlayedCard : PlayerCard -> PlayedCard
toPlayedCard card =
    if PlayerCard.hasAbility card then
        AbilityCard card False

    else
        NormalCard card


fromPlayedCard : PlayedCard -> PlayerCard
fromPlayedCard playedCard =
    case playedCard of
        AbilityCard card _ ->
            card

        NormalCard card ->
            card


createFightArea : HazardCard -> FightArea
createFightArea hazardCard =
    FightArea hazardCard [] []


canDrawFreeCard : FightArea -> Bool
canDrawFreeCard (FightArea hazard playedCardsLeft _) =
    List.length playedCardsLeft < HazardCard.getFreeCards hazard


playOnLeft : PlayerCard -> FightArea -> FightArea
playOnLeft card (FightArea hazardCard cardsPlayedLeft cardsPlayedRight) =
    FightArea hazardCard (toPlayedCard card :: cardsPlayedLeft) cardsPlayedRight


playOnRight : PlayerCard -> FightArea -> FightArea
playOnRight card (FightArea hazardCard cardsPlayedLeft cardsPlayedRight) =
    FightArea hazardCard cardsPlayedLeft (toPlayedCard card :: cardsPlayedRight)


getPlayerStrength : FightArea -> Int
getPlayerStrength (FightArea _ playedCardsLeft playedCardsRight) =
    let
        getPlayedCardStrength : PlayedCard -> Int
        getPlayedCardStrength playedCard =
            playedCard
                |> fromPlayedCard
                |> PlayerCard.getFightingValue
    in
    List.concat [ playedCardsLeft, playedCardsRight ]
        |> List.map getPlayedCardStrength
        |> List.foldl (+) 0


getHazard : FightArea -> HazardCard
getHazard (FightArea hazard _ _) =
    hazard


getHazardStrength : Phase -> FightArea -> Int
getHazardStrength phase (FightArea hazard _ _) =
    case phase of
        PhaseGreen ->
            HazardCard.getGreenValue hazard

        PhaseYellow ->
            HazardCard.getYellowValue hazard

        PhaseRed ->
            HazardCard.getRedValue hazard


getLeftCards : FightArea -> List PlayerCard
getLeftCards (FightArea _ cardsPlayedLeft _) =
    List.map fromPlayedCard cardsPlayedLeft


getRightCards : FightArea -> List PlayerCard
getRightCards (FightArea _ _ cardsPlayedRight) =
    List.map fromPlayedCard cardsPlayedRight
