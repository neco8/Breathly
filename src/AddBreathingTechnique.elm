module AddBreathingTechnique exposing (Model, Msg, init, update, view)

import Browser.Navigation exposing (pushUrl)
import Expression as E
import ExpressionForm
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Routes
import Storage as S
import Browser.Navigation as Navigation


type alias Model =
    { expressionFormModel : ExpressionForm.Model
    , title : String
    }


init : Model
init =
    { expressionFormModel =
        ExpressionForm.init <|
            E.Breathing
                (E.Inhale (E.NumberDuration (E.Number (E.StaticNumber 4)) E.Seconds))
                (Just (E.Hold (E.NumberDuration (E.Number (E.StaticNumber 4)) E.Seconds)))
                (E.Exhale (E.NumberDuration (E.Number (E.StaticNumber 4)) E.Seconds))
                (Just (E.Hold (E.NumberDuration (E.Number (E.StaticNumber 4)) E.Seconds)))
    , title = ""
    }


type Msg
    = ExpressionFormMsg ExpressionForm.Msg
    | ChangeTitleInput String
    | AddBreathingTechnique


update : Navigation.Key -> Msg -> Model -> ( Model, List S.StorageAction, Cmd Msg )
update key msg model =
    case msg of
        ExpressionFormMsg expressionFormMsg ->
            ( { model | expressionFormModel = ExpressionForm.update expressionFormMsg model.expressionFormModel }
            , []
            , Cmd.none
            )

        ChangeTitleInput title ->
            ( { model | title = title }
            , []
            , Cmd.none
            )

        AddBreathingTechnique ->
            ( model
            , [ S.AddBreathingTechnique
                    { breathing = model.expressionFormModel.breathing
                    , title = model.title
                    }
              ]
            , pushUrl key <|
                Routes.toString
                    Routes.HomeRoute
            )


view : Model -> Html Msg
view model =
    div []
        [ Html.map ExpressionFormMsg <| ExpressionForm.breathingInputView model.expressionFormModel
        , input
            [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
            , value model.title
            , onInput ChangeTitleInput
            ]
            []
        , button
            [ onClick AddBreathingTechnique
            , class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
            ]
            [ text "追加" ]
        ]
