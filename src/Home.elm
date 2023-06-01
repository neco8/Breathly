module Home exposing (Model, Msg, init, subscriptions, update, view)

import Breathing
import Color exposing (Color)
import Expression exposing (BooleanExpr(..), Breathing(..), ComparisonOperator(..), Duration(..), Exhale(..), Hold(..), Inhale(..), Number(..), NumberExpr(..), TimeUnit(..))
import Html exposing (Attribute, Html, a, button, div, h1, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode
import MinSecInput
import Routes
import Type exposing (BreathingTechnique)


type Msg
    = NoOp
    | BreathingMsg Breathing.Msg
    | MinSecInputMsg MinSecInput.Msg
    | BreathingStart Breathing
    | OpenBreathingTimeModal Breathing
    | CloseBreathingTimeModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.screen ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( BreathingMsg breathingMsg, PracticeScreen breathingModel ) ->
            let
                ( newBreathingModel, breathingCmd ) =
                    Breathing.update breathingMsg breathingModel
            in
            ( { model | screen = PracticeScreen newBreathingModel }
            , Cmd.map BreathingMsg breathingCmd
            )

        ( BreathingMsg _, _ ) ->
            update NoOp model

        ( MinSecInputMsg minSecInputMsg, ListScreen ) ->
            case Maybe.map (\{ minSecInputModel, breathing } -> ( MinSecInput.update minSecInputMsg minSecInputModel, breathing )) model.breathingTimeModalModel of
                Just ( ( newMinSecInputModel, minSecInputCmd ), breathing ) ->
                    ( { model | breathingTimeModalModel = Just { minSecInputModel = newMinSecInputModel, breathing = breathing } }
                    , Cmd.map MinSecInputMsg minSecInputCmd
                    )

                Nothing ->
                    update NoOp model

        ( MinSecInputMsg _, _ ) ->
            update NoOp model

        ( OpenBreathingTimeModal breathing, ListScreen ) ->
            ( { model | breathingTimeModalModel = Just { breathing = breathing, minSecInputModel = MinSecInput.init 0 } }
            , Cmd.none
            )

        ( OpenBreathingTimeModal _, _ ) ->
            update NoOp model

        ( CloseBreathingTimeModal, ListScreen ) ->
            ( { model | breathingTimeModalModel = Nothing }, Cmd.none )

        ( CloseBreathingTimeModal, _ ) ->
            update NoOp model

        ( BreathingStart breathing, ListScreen ) ->
            case model.breathingTimeModalModel of
                Just { minSecInputModel } ->
                    let
                        ( breathingModel, breathingCmd ) =
                            Breathing.init { breathing = breathing, breathingTime = 1000 * MinSecInput.toSeconds minSecInputModel }
                    in
                    ( { model | screen = PracticeScreen breathingModel }, Cmd.map BreathingMsg breathingCmd )

                Nothing ->
                    update NoOp model

        ( BreathingStart _, _ ) ->
            update NoOp model


type Screen
    = ListScreen
    | PracticeScreen Breathing.Model


type alias Model =
    { screen : Screen
    , breathingTimeModalModel :
        Maybe
            { minSecInputModel : MinSecInput.Model
            , breathing : Breathing
            }
    }


initialModel : Model
initialModel =
    { screen = ListScreen
    , breathingTimeModalModel = Nothing
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
                [ span [ class "material-symbols-outlined text-base" ] [ text "close" ] ]

        minSecInputView =
            MinSecInput.view minSecInputModel |> Html.map MinSecInputMsg
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
        [ span [ class "material-symbols-outlined text-3xl text-gray-500" ] [ text "add" ] ]


listScreenView : List BreathingTechnique -> Maybe { minSecInputModel : MinSecInput.Model, breathing : Breathing } -> Html Msg
listScreenView breathingTechniques minSecInputModel =
    div [ class "grid grid-cols-[1fr] sm:grid-cols-[1fr,1fr] lg:grid-cols-[1fr,1fr,1fr] gap-4 font-main p-4" ]
        (List.map breathingTechniqueView breathingTechniques
            ++ [ addBreathingTechniqueCardView
               , case Maybe.map breathingTimeModal minSecInputModel of
                    Just modal ->
                        modal

                    Nothing ->
                        text ""
               ]
        )


view : { record | foregroundColor : Color, backgroundColor : Color } -> { storage | breathingTechniques : List BreathingTechnique } -> Model -> Html Msg
view colors storage model =
    case model.screen of
        ListScreen ->
            listScreenView storage.breathingTechniques model.breathingTimeModalModel

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
