module Home exposing (Model, Msg, init, subscriptions, update, view)

import Breathing exposing (BreathingAction(..), GoToHome(..))
import Browser.Navigation as Navigation
import Color exposing (Color)
import Expression exposing (BooleanExpr(..), Breathing(..), ComparisonOperator(..), Duration(..), Exhale(..), Hold(..), Inhale(..), Number(..), NumberExpr(..), TimeUnit(..))
import Html exposing (Attribute, Html, a, button, div, h1, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode
import MaterialIcon exposing (materialIconView)
import MinSecInput
import Routes
import Storage exposing (StorageAction)
import Tab exposing (tabView)
import Type exposing (BreathingTechnique)


type Msg
    = NoOp
    | BreathingMsg Breathing.Msg
    | MinSecInputMsg MinSecInput.Msg
    | BreathingStart Breathing
    | OpenBreathingTimeModal Breathing
    | CloseBreathingTimeModal


update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg, List StorageAction )
update key msg model =
    case ( msg, model.screen ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none, [] )

        ( BreathingMsg breathingMsg, PracticeScreen breathingModel ) ->
            let
                ( newBreathingModel, breathingCmd, BreathingAction storageActions mGoToHome ) =
                    Breathing.update key breathingMsg breathingModel
            in
            ( { model
                | screen =
                    case mGoToHome of
                        Just GoToHome ->
                            ListScreen

                        Nothing ->
                            PracticeScreen newBreathingModel
              }
            , Cmd.map BreathingMsg breathingCmd
            , storageActions
            )

        ( BreathingMsg _, _ ) ->
            update key NoOp model

        ( MinSecInputMsg minSecInputMsg, ModalScreen { minSecInputModel, breathing } ) ->
            case MinSecInput.update minSecInputMsg minSecInputModel of
                ( newMinSecInputModel, minSecInputCmd ) ->
                    ( { model | screen = ModalScreen { minSecInputModel = newMinSecInputModel, breathing = breathing } }
                    , Cmd.map MinSecInputMsg minSecInputCmd
                    , []
                    )

        ( MinSecInputMsg _, _ ) ->
            update key NoOp model

        ( OpenBreathingTimeModal breathing, ListScreen ) ->
            ( { model | screen = ModalScreen { breathing = breathing, minSecInputModel = MinSecInput.init 0 } }
            , Cmd.none
            , []
            )

        ( OpenBreathingTimeModal _, _ ) ->
            update key NoOp model

        ( CloseBreathingTimeModal, ModalScreen _ ) ->
            ( { model | screen = ListScreen }, Cmd.none, [] )

        ( CloseBreathingTimeModal, _ ) ->
            update key NoOp model

        ( BreathingStart breathing, ModalScreen { minSecInputModel } ) ->
            let
                ( breathingModel, breathingCmd ) =
                    Breathing.init { breathing = breathing, breathingTime = 1000 * MinSecInput.toSeconds minSecInputModel }
            in
            ( { model | screen = PracticeScreen breathingModel }, Cmd.map BreathingMsg breathingCmd, [] )

        ( BreathingStart _, _ ) ->
            update key NoOp model


type Screen
    = ListScreen
    | ModalScreen
        { minSecInputModel : MinSecInput.Model
        , breathing : Breathing
        }
    | PracticeScreen Breathing.Model


type alias Model =
    { screen : Screen
    }


initialModel : Model
initialModel =
    { screen = ListScreen
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch []
    )


breathingTimeModal : { minSecInputModel : MinSecInput.Model, breathing : Breathing } -> Html Msg
breathingTimeModal { minSecInputModel, breathing } =
    let
        closeButtonView =
            button
                [ class "p-2 -mr-2 -mt-2 place-self-end text-gray-500 hover:text-gray-700 focus:outline-none"
                , onClick CloseBreathingTimeModal
                ]
                [ materialIconView { iconType = "close", class = class "text-base" }
                ]

        minSecInputView =
            MinSecInput.view { tagger = MinSecInputMsg, onFinish = Just <| BreathingStart breathing } minSecInputModel
    in
    div
        [ class "fixed inset-0 z-50 flex items-center justify-center bg-gray-900 bg-opacity-50"
        , onClick CloseBreathingTimeModal
        ]
        [ div
            [ class "relative grid bg-white rounded-lg shadow-lg p-6 max-w-md w-full mx-4 my-4"
            , stopPropagationOn "click" <| Json.Decode.succeed ( NoOp, True )
            ]
            [ closeButtonView
            , minSecInputView
            , button
                [ onClick (BreathingStart breathing)
                , class "shadow-md bg-teal-600 p-2 text-white font-bold w-full mt-3 rounded-sm text-sm"
                ]
                [ text "スタート" ]
            ]
        ]


cardClass : Attribute Msg
cardClass =
    class "bg-gray-100 p-4 my-2 rounded-lg max-w-md mx-auto w-full aspect-[3/4] shadow-lg min-w-0 hover:cursor-pointer hover:-translate-y-1 transition-all"


breathingTechniqueView : BreathingTechnique -> Html Msg
breathingTechniqueView technique =
    div
        [ cardClass
        , onClick (OpenBreathingTimeModal technique.breathing)
        ]
        [ h1 [ class "text-xl font-bold" ] [ text technique.title ]
        ]


addBreathingTechniqueCardView : Html Msg
addBreathingTechniqueCardView =
    a [ cardClass, Routes.href Routes.AddBreathingTechniqueRoute, class "grid place-items-center" ]
        [ materialIconView
            { class = class "text-3xl text-gray-500"
            , iconType = "add"
            }
        ]


listScreenView : List BreathingTechnique -> Maybe { minSecInputModel : MinSecInput.Model, breathing : Breathing } -> Html Msg
listScreenView breathingTechniques minSecInputModel =
    div []
        [ div [ class "grid grid-cols-[1fr] sm:grid-cols-[1fr,1fr] lg:grid-cols-[1fr,1fr,1fr] gap-4 font-main p-4" ]
            (List.map breathingTechniqueView breathingTechniques
                ++ [ addBreathingTechniqueCardView
                   , case Maybe.map breathingTimeModal minSecInputModel of
                        Just modal ->
                            modal

                        Nothing ->
                            text ""
                   ]
            )
        , tabView
        ]


view : { record | foregroundColor : Color, backgroundColor : Color } -> { storage | breathingTechniques : List BreathingTechnique } -> Model -> Html Msg
view colors storage model =
    case model.screen of
        ListScreen ->
            listScreenView storage.breathingTechniques Nothing

        ModalScreen modal ->
            listScreenView storage.breathingTechniques <| Just modal

        PracticeScreen breathingModel ->
            Breathing.view colors breathingModel |> Html.map BreathingMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screen of
        PracticeScreen breathingModel ->
            Sub.map BreathingMsg <|
                Breathing.subscriptions breathingModel

        ListScreen ->
            Sub.none

        ModalScreen _ ->
            Sub.none
