module FightArea exposing
    ( FightArea
    , FightAreaIndex
    , canDrawFreeCard
    , cardCanBeActivated
    , createFightArea
    , getCard
    , getHazard
    , getHazardStrength
    , getLeftCards
    , getPlayerStrength
    , getRightCards
    , playOnLeft
    , playOnRight
    , setCardUsed
    , setInUseToUsed
    )

import HazardCard exposing (HazardCard)
import List.Extra
import Phase exposing (Phase(..))
import PlayerCard exposing (PlayerCard)


type FightAreaIndex
    = LeftIndex Int
    | RightIndex Int


type UsedState
    = NotUsed
    | Used
    | InUse


type PlayedCard
    = NormalCard PlayerCard
    | AbilityCard PlayerCard UsedState


type FightArea
    = FightArea HazardCard (List PlayedCard) (List PlayedCard)


toPlayedCard : PlayerCard -> PlayedCard
toPlayedCard card =
    if PlayerCard.hasAbility card then
        AbilityCard card NotUsed

    else
        NormalCard card


fromPlayedCard : PlayedCard -> PlayerCard
fromPlayedCard playedCard =
    case playedCard of
        AbilityCard card _ ->
            card

        NormalCard card ->
            card


playedCardCanBeUsed : PlayedCard -> Bool
playedCardCanBeUsed playedCard =
    case playedCard of
        AbilityCard _ NotUsed ->
            True

        _ ->
            False


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


getSideAndIndex : FightAreaIndex -> FightArea -> ( Int, List PlayedCard )
getSideAndIndex fightAreaIndex (FightArea _ cardsPlayedLeft cardsPlayedRight) =
    case fightAreaIndex of
        LeftIndex index ->
            ( index, cardsPlayedLeft )

        RightIndex index ->
            ( index, cardsPlayedRight )


getCard : FightAreaIndex -> FightArea -> Maybe PlayerCard
getCard fightAreaIndex fightArea =
    let
        ( index, cards ) =
            getSideAndIndex fightAreaIndex fightArea
    in
    cards
        |> List.Extra.getAt index
        |> Maybe.map fromPlayedCard


setCardUsed : FightAreaIndex -> FightArea -> FightArea
setCardUsed fightAreaIndex (FightArea hazard cardsPlayedLeft cardsPlayedRight) =
    let
        useCard : PlayedCard -> PlayedCard
        useCard playedCard =
            case playedCard of
                NormalCard _ ->
                    playedCard

                AbilityCard card _ ->
                    AbilityCard card Used

        setUsedIfIndexMatches : Int -> Int -> PlayedCard -> PlayedCard
        setUsedIfIndexMatches passedIndex i playedCard =
            if i == passedIndex then
                useCard playedCard

            else
                playedCard
    in
    case fightAreaIndex of
        LeftIndex index ->
            FightArea
                hazard
                (List.indexedMap (setUsedIfIndexMatches index) cardsPlayedLeft)
                cardsPlayedRight

        RightIndex index ->
            FightArea
                hazard
                cardsPlayedLeft
                (List.indexedMap (setUsedIfIndexMatches index) cardsPlayedRight)


cardCanBeActivated : FightAreaIndex -> FightArea -> Bool
cardCanBeActivated fightAreaIndex fightArea =
    let
        ( index, cards ) =
            getSideAndIndex fightAreaIndex fightArea
    in
    cards
        |> List.Extra.getAt index
        |> Maybe.map playedCardCanBeUsed
        |> Maybe.withDefault False


setInUseToUsed : FightArea -> FightArea
setInUseToUsed (FightArea hazard cardsPlayedLeft cardsPlayedRight) =
    let
        setInUseCardToUsed : PlayedCard -> PlayedCard
        setInUseCardToUsed playedCard =
            case playedCard of
                AbilityCard card InUse ->
                    AbilityCard card Used

                _ ->
                    playedCard
    in
    FightArea
        hazard
        (List.map setInUseCardToUsed cardsPlayedLeft)
        (List.map setInUseCardToUsed cardsPlayedRight)
