module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import LifePoints


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Phase
    = PhaseGreen
    | PhaseYellow
    | PhaseRed


type alias Model =
    { lifePoints : LifePoints.Counter, phase : Phase }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialModel : Model
        initialModel =
            { lifePoints = LifePoints.createCounter 20, phase = PhaseGreen }
    in
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = HandleThingInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "m-6 text-xl" ] [ text "Why hello there." ]
