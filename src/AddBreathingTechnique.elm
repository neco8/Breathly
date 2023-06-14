module AddBreathingTechnique exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation exposing (pushUrl)
import DropdownInput exposing (dropdownInput)
import Expression as E
import ExpressionForm
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, disabled, value)
import Html.Events exposing (onClick, onInput)
import InputEvent exposing (onEnter)
import Routes
import Storage as S


type alias Model =
    { expressionFormModel : ExpressionForm.Model
    , title : String
    , formType : FormType
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
    , formType = Simple
    }


type Msg
    = ExpressionFormMsg ExpressionForm.Msg
    | ChangeTitleInput String
    | AddBreathingTechnique E.Breathing
    | SelectFormType FormType
    | GoBackHome
    | NoOp


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

        AddBreathingTechnique breathing ->
            ( model
            , [ S.AddBreathingTechnique
                    { breathing = breathing
                    , title = model.title
                    }
              ]
            , pushUrl key <|
                Routes.toString
                    Routes.HomeRoute
            )

        GoBackHome ->
            ( model
            , []
            , pushUrl key <|
                Routes.toString Routes.HomeRoute
            )

        SelectFormType formType ->
            ( { model | formType = formType }, [], Cmd.none )

        NoOp ->
            ( model, [], Cmd.none )


type FormType
    = Advanced
    | Simple


formTypeToString : FormType -> String
formTypeToString =
    \x ->
        case x of
            Advanced ->
                "Advanced"

            Simple ->
                "Simple"


stringToFormType : String -> Maybe FormType
stringToFormType s =
    case s of
        "Advanced" ->
            Just Advanced

        "Simple" ->
            Just Simple

        _ ->
            Nothing


formTypeAll : List FormType
formTypeAll =
    let
        formTypeNext acc =
            case List.head acc of
                Nothing ->
                    [ Advanced ] |> formTypeNext

                Just Advanced ->
                    Simple :: acc |> formTypeNext

                Just Simple ->
                    acc
    in
    formTypeNext []
        |> Debug.log "formTypeAll"


getSubmitArgs : { record | expressionFormModel : ExpressionForm.Model, title : String } -> Maybe ( E.Breathing, String )
getSubmitArgs { expressionFormModel, title } =
    let
        mbreathing =
            ExpressionForm.extractFinishedBreathing expressionFormModel

        mtitle =
            if title == "" then
                Nothing

            else
                Just title
    in
    case ( mbreathing, mtitle ) of
        ( Just b, Just t ) ->
            Just ( b, t )

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    div []
        [ dropdownInput
            { class = Just <| class ""
            , toString = formTypeToString
            , encode = formTypeToString
            , decode = stringToFormType
            , choices = formTypeAll
            , onSelect = SelectFormType
            , selected = model.formType
            }
        , Html.map ExpressionFormMsg <|
            (case model.formType of
                Simple ->
                    ExpressionForm.breathingInputView

                Advanced ->
                    ExpressionForm.advancedBreathingInputView
            )
                model.expressionFormModel
        , input
            [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
            , value model.title
            , onInput ChangeTitleInput
            , onEnter
                { noOp = NoOp
                , msg =
                    case getSubmitArgs model of
                        Just ( breathing, _ ) ->
                            AddBreathingTechnique breathing

                        Nothing ->
                            NoOp
                }
            ]
            []
        , button [ class "bg-slate-300 text-slate-600 py-2 px-4 rounded", onClick GoBackHome ] [ text "キャンセル" ]
        , button
            (List.concat
                [ case getSubmitArgs model of
                    Just ( breathing, _ ) ->
                        [ onClick <| AddBreathingTechnique breathing ]

                    Nothing ->
                        [ disabled True ]
                , [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded disabled:bg-slate-100 disabled:text-gray-400 disabled:cursor-not-allowed" ]
                ]
            )
            [ text "追加" ]
        ]
