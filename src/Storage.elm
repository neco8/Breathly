module Storage exposing (Model, StorageAction(..), init, update)

import Expression exposing (Breathing(..), Duration(..), Exhale(..), Hold(..), Inhale(..), Number(..), NumberExpr(..), TimeUnit(..))
import Type exposing (BreathingId, BreathingResult, BreathingTechnique)


type alias Model =
    { breathingTechniques : List BreathingTechnique
    , sessions : List BreathingResult
    }


type StorageAction
    = AddBreathingTechnique BreathingTechnique
    | RemoveBreathingTechnique BreathingId
    | AddBreathingResult BreathingResult
    | RemoveBreathingResult BreathingId


breathingTechniquesMock : List BreathingTechnique
breathingTechniquesMock =
    [ { title = "4-7-8 Breathing"
      , breathing =
            Breathing (Inhale (NumberDuration (Number (StaticNumber 4)) Seconds))
                (Just (Hold (NumberDuration (Number (StaticNumber 7)) Seconds)))
                (Exhale (NumberDuration (Number (StaticNumber 8)) Seconds))
                Nothing
      }
    ]


init : Model
init =
    { breathingTechniques = breathingTechniquesMock
    , sessions = []
    }


update : StorageAction -> Model -> Model
update action model =
    case action of
        AddBreathingTechnique newTechnique ->
            { model | breathingTechniques = newTechnique :: model.breathingTechniques }

        RemoveBreathingTechnique breathingId ->
            { model | breathingTechniques = List.filter (\technique -> technique.breathing /= breathingId) model.breathingTechniques }

        AddBreathingResult newResult ->
            { model | sessions = newResult :: model.sessions }

        RemoveBreathingResult breathingId ->
            { model | sessions = List.filter (\session -> session.breathingId /= breathingId) model.sessions }
