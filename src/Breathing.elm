module Breathing exposing (BreathingAction(..), GoToHome(..), Model, Msg, init, subscriptions, update, view)

import Animator exposing (Timeline)
import Animator.Css
import BreathStep exposing (BreathStep(..))
import Browser.Navigation as Navigation
import CircleIndicator exposing (circleIndicator)
import Color
import Expression exposing (ArithmeticOperator(..), BooleanExpr(..), BooleanOperator(..), Breathing(..), ComparisonExpr(..), ComparisonOperator(..), Duration(..), Exhale(..), Hold(..), Inhale(..), MinMaxOperator(..), Number(..), NumberExpr(..), TimeUnit(..))
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Routes
import Storage exposing (StorageAction)
import Task
import Time
import Type exposing (BreathingResult)


flip f a b =
    f b a


getNextBreathStep : BreathStep -> BreathStep
getNextBreathStep breathStep =
    case breathStep of
        Inhaling ->
            InHolding

        InHolding ->
            Exhaling

        Exhaling ->
            ExHolding

        ExHolding ->
            Inhaling


getBreathingAction : BreathStep -> String
getBreathingAction breathStep =
    case breathStep of
        Inhaling ->
            "吸う"

        InHolding ->
            "止める"

        Exhaling ->
            "吐く"

        ExHolding ->
            "止める"


getInhaleDuration : Inhale -> Duration
getInhaleDuration inhale =
    case inhale of
        Inhale duration ->
            duration


getExhaleDuration : Exhale -> Duration
getExhaleDuration exhale =
    case exhale of
        Exhale duration ->
            duration


getHoldDuration : Hold -> Duration
getHoldDuration hold =
    case hold of
        Hold duration ->
            duration


type UserDuration
    = IsUserInteractionNeeded
    | IsNotUserInteractionNeeded Float
    | EmptyDuration


evaluateNumberExpr : NumberExpr -> Int -> Float
evaluateNumberExpr numberExpr phase =
    let
        getNumberValue number phaseNumber =
            case number of
                StaticNumber value ->
                    value

                PhaseNumber ->
                    phaseNumber
    in
    case numberExpr of
        Number number ->
            getNumberValue number phase |> toFloat

        ArithmeticExpr number op expr2 ->
            let
                value1 =
                    getNumberValue number phase
                        |> toFloat

                value2 =
                    evaluateNumberExpr expr2 phase
            in
            case op of
                Plus ->
                    value1 + value2

                Minus ->
                    value1 - value2

                Times ->
                    value1 * value2

                Divide ->
                    value1 / value2

        MinMaxExpr minMaxOp number expr2 ->
            let
                value1 =
                    getNumberValue number phase
                        |> toFloat

                value2 =
                    evaluateNumberExpr expr2 phase
            in
            case minMaxOp of
                Min ->
                    min value1 value2

                Max ->
                    max value1 value2


evaluateComparisonOperator : ComparisonOperator -> Float -> Float -> Bool
evaluateComparisonOperator comparisonOperator =
    case comparisonOperator of
        GreaterThanOrEqual ->
            (>=)

        LessThanOrEqual ->
            (<=)

        GreaterThan ->
            (>)

        LessThan ->
            (<)

        EqualTo ->
            (==)

        NotEqualTo ->
            (/=)


evaluateBooleanOperator : BooleanOperator -> Bool -> Bool -> Bool
evaluateBooleanOperator booleanOperator =
    case booleanOperator of
        And ->
            (&&)

        Or ->
            (||)


evaluateComparisonExpr : ComparisonExpr -> Int -> Bool
evaluateComparisonExpr comparisonExpr phase =
    case comparisonExpr of
        Comparison numberExpr1 comparisonOperator numberExpr2 ->
            evaluateComparisonOperator comparisonOperator
                (evaluateNumberExpr numberExpr1 phase)
                (evaluateNumberExpr numberExpr2 phase)


evaluateBooleanExpr : BooleanExpr -> Int -> Bool
evaluateBooleanExpr booleanExpr phase =
    case booleanExpr of
        ComparisonExpr comparisonExpr ->
            evaluateComparisonExpr comparisonExpr phase

        BooleanOperator leftComparisonExpr booleanOperator rightBooleanExpr ->
            evaluateBooleanOperator booleanOperator
                (evaluateComparisonExpr leftComparisonExpr phase)
                (evaluateBooleanExpr rightBooleanExpr phase)


durationToMilliseconds : Duration -> Int -> UserDuration
durationToMilliseconds duration phase =
    case duration of
        Naturally ->
            IsUserInteractionNeeded

        AsLongAsPossible ->
            IsUserInteractionNeeded

        NumberDuration numberExpr timeUnit ->
            let
                number =
                    evaluateNumberExpr numberExpr phase
            in
            IsNotUserInteractionNeeded <|
                case timeUnit of
                    Seconds ->
                        number * 1000

                    Minutes ->
                        number * 60 * 1000

        ConditionalDuration booleanExpr thenDuration elseDuration ->
            if evaluateBooleanExpr booleanExpr phase then
                durationToMilliseconds thenDuration phase

            else
                durationToMilliseconds elseDuration phase


millisecondFromBreathStep : Breathing -> Int -> BreathStep -> UserDuration
millisecondFromBreathStep breathing currentPhase breathStep =
    case breathing of
        Breathing inhale maybeInhold exhale maybeExhold ->
            case breathStep of
                Inhaling ->
                    let
                        duration =
                            getInhaleDuration inhale
                    in
                    durationToMilliseconds duration currentPhase

                InHolding ->
                    maybeInhold
                        |> Maybe.map
                            (getHoldDuration
                                >> flip durationToMilliseconds currentPhase
                            )
                        |> Maybe.withDefault EmptyDuration

                Exhaling ->
                    let
                        duration =
                            getExhaleDuration exhale
                    in
                    durationToMilliseconds duration currentPhase

                ExHolding ->
                    maybeExhold
                        |> Maybe.map
                            (getHoldDuration
                                >> flip durationToMilliseconds currentPhase
                            )
                        |> Maybe.withDefault EmptyDuration


getNextTransitionTime : Breathing -> BreathStep -> Int -> Time.Posix -> Time.Posix
getNextTransitionTime breathing breathStep currentPhase currentTime =
    millisecondFromBreathStep breathing currentPhase breathStep
        |> (\m ->
                case m of
                    IsNotUserInteractionNeeded millisecond ->
                        millisecond
                            + toFloat (Time.posixToMillis currentTime)
                            |> round
                            |> Time.millisToPosix

                    IsUserInteractionNeeded ->
                        currentTime

                    EmptyDuration ->
                        currentTime
           )


type Msg
    = Tick Time.Posix
    | NextBreathStep
    | AutoNextBreathStep
    | InitTimer Int Time.Posix
    | GoToHomeMsg


type Screen
    = PracticeScreen
    | PracticeResultScreen BreathingResult


type alias Model =
    { breathing : Breathing
    , currentBreathStep : Timeline BreathStep
    , currentPhase : Int
    , timer :
        Maybe
            { now : Time.Posix
            , nextTransitionAt : Time.Posix
            , phaseStartedAt : Time.Posix
            , breathingStartedAt : Time.Posix
            , breathingTime : Int
            }
    , screen : Screen
    }


initialBreathStep : BreathStep
initialBreathStep =
    ExHolding


initialPhase : Int
initialPhase =
    -1


init : { breathing : Breathing, breathingTime : Int } -> ( Model, Cmd Msg )
init { breathing, breathingTime } =
    ( { breathing = breathing
      , currentBreathStep = Animator.init initialBreathStep
      , currentPhase = initialPhase
      , timer = Nothing
      , screen = PracticeScreen
      }
    , Task.perform (InitTimer breathingTime) Time.now
    )


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.Css.watching .currentBreathStep
            (\newBreathStep model ->
                { model | currentBreathStep = newBreathStep }
            )


type IsBreathingFinished
    = BreathingFinished
    | BreathingNotFinished


isBreathingFinished : Time.Posix -> { timer | breathingStartedAt : Time.Posix, breathingTime : Int } -> IsBreathingFinished
isBreathingFinished now timer =
    let
        breathingWillFinishAt =
            Time.posixToMillis timer.breathingStartedAt
                + timer.breathingTime
                |> Time.millisToPosix
    in
    if Time.posixToMillis now >= Time.posixToMillis breathingWillFinishAt then
        BreathingFinished

    else
        BreathingNotFinished


andThen : (Model -> ( Model, Cmd Msg, BreathingAction )) -> ( Model, Cmd Msg, BreathingAction ) -> ( Model, Cmd Msg, BreathingAction )
andThen f ( model, cmd, BreathingAction list goToHome ) =
    let
        ( nextModel, nextCmd, BreathingAction nextList nextGoToHome ) =
            f model
    in
    ( nextModel
    , Cmd.batch [ cmd, nextCmd ]
    , BreathingAction (list ++ nextList) <|
        case goToHome of
            Just g ->
                Just g

            Nothing ->
                nextGoToHome
    )


type GoToHome
    = GoToHome


type BreathingAction
    = BreathingAction (List StorageAction) (Maybe GoToHome)


calculateNextPhase : BreathStep -> Breathing -> Int -> Int
calculateNextPhase finishedBreathStep breathing prev =
    let
        isUserDurationEmpty u =
            List.member u [ IsNotUserInteractionNeeded 0, EmptyDuration ]
    in
    case finishedBreathStep of
        ExHolding ->
            prev + 1

        Exhaling ->
            if isUserDurationEmpty (millisecondFromBreathStep breathing prev ExHolding) then
                prev + 1

            else
                prev

        InHolding ->
            if
                [ Exhaling, ExHolding ]
                    |> List.all (millisecondFromBreathStep breathing prev >> isUserDurationEmpty)
            then
                prev + 1

            else
                prev

        Inhaling ->
            if
                [ InHolding, Exhaling, ExHolding ]
                    |> List.all (millisecondFromBreathStep breathing prev >> isUserDurationEmpty)
            then
                prev + 1

            else
                prev


update : Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg, BreathingAction )
update key msg model =
    case msg of
        InitTimer time currentTime ->
            ( model, Cmd.none, BreathingAction [] Nothing )
                |> andThen
                    (\prev ->
                        ( { prev
                            | timer =
                                Just
                                    { now = currentTime
                                    , nextTransitionAt = getNextTransitionTime model.breathing initialBreathStep initialPhase currentTime
                                    , phaseStartedAt = currentTime
                                    , breathingStartedAt = currentTime
                                    , breathingTime = time
                                    }
                          }
                        , Cmd.none
                        , BreathingAction [] Nothing
                        )
                    )
                |> andThen (update key AutoNextBreathStep)

        _ ->
            case model.timer of
                Nothing ->
                    -- init後必ずtimerが存在するので、ここに来ることはない
                    ( model, Cmd.none, BreathingAction [] Nothing )

                Just timer ->
                    case msg of
                        InitTimer _ _ ->
                            ( model, Cmd.none, BreathingAction [] Nothing )

                        Tick newTime ->
                            let
                                isAutoNextBreathStep m =
                                    not (millisecondFromBreathStep m.breathing m.currentPhase (Animator.current m.currentBreathStep) == IsUserInteractionNeeded)
                                        && Time.posixToMillis newTime
                                        >= Time.posixToMillis timer.nextTransitionAt

                                autoProceedStepWhenNeeded m =
                                    if isAutoNextBreathStep m then
                                        update key AutoNextBreathStep m

                                    else
                                        ( m, Cmd.none, BreathingAction [] Nothing )
                            in
                            case isBreathingFinished newTime timer of
                                BreathingFinished ->
                                    ( model, Cmd.none, BreathingAction [] Nothing )
                                        |> andThen (\prev -> ( { prev | timer = Just { timer | now = newTime } }, Cmd.none, BreathingAction [] Nothing ))
                                        |> andThen
                                            (\prev ->
                                                if
                                                    not (millisecondFromBreathStep prev.breathing prev.currentPhase (Animator.current prev.currentBreathStep) == IsUserInteractionNeeded)
                                                        && ((toFloat <| Time.posixToMillis newTime - Time.posixToMillis timer.nextTransitionAt) / 100 |> round |> (\s -> s == 0))
                                                then
                                                    update key AutoNextBreathStep prev

                                                else
                                                    ( prev, Cmd.none, BreathingAction [] Nothing )
                                            )
                                        |> andThen
                                            (\prev ->
                                                let
                                                    result =
                                                        { breathingId = prev.breathing
                                                        , seconds =
                                                            (toFloat <| Time.posixToMillis newTime - Time.posixToMillis timer.breathingStartedAt)
                                                                / 1000
                                                        , phases = prev.currentPhase
                                                        }
                                                in
                                                ( { prev
                                                    | screen =
                                                        PracticeResultScreen
                                                            result
                                                  }
                                                , Cmd.none
                                                , BreathingAction [ Storage.AddBreathingResult result ] Nothing
                                                )
                                            )

                                BreathingNotFinished ->
                                    ( model, Cmd.none, BreathingAction [] Nothing )
                                        |> andThen (\prev -> ( { prev | timer = Just { timer | now = newTime } }, Cmd.none, BreathingAction [] Nothing ))
                                        |> andThen (\prev -> ( Animator.update newTime animator prev, Cmd.none, BreathingAction [] Nothing ))
                                        |> andThen autoProceedStepWhenNeeded

                        NextBreathStep ->
                            let
                                nextBreathStep =
                                    getNextBreathStep <| Animator.current model.currentBreathStep

                                nextPhase =
                                    calculateNextPhase (Animator.current model.currentBreathStep) model.breathing model.currentPhase

                                nextTransition =
                                    getNextTransitionTime model.breathing nextBreathStep model.currentPhase timer.now
                            in
                            ( { model
                                | currentBreathStep =
                                    model.currentBreathStep
                                        |> Animator.go
                                            (Animator.millis <|
                                                toFloat <|
                                                    Time.posixToMillis nextTransition
                                                        - Time.posixToMillis timer.now
                                            )
                                            nextBreathStep
                                , currentPhase = nextPhase
                                , timer = Just { timer | nextTransitionAt = nextTransition, phaseStartedAt = timer.now }
                              }
                            , Cmd.none
                            , BreathingAction [] Nothing
                            )

                        AutoNextBreathStep ->
                            update key NextBreathStep model

                        GoToHomeMsg ->
                            ( model
                            , Cmd.none
                            , BreathingAction [] <| Just GoToHome
                            )


practiceScreenView :
    { colors | backgroundColor : Color.Color, foregroundColor : Color.Color }
    ->
        { model
            | breathing : Breathing
            , currentBreathStep : Timeline BreathStep
            , timer : Maybe { timer | now : Time.Posix, nextTransitionAt : Time.Posix, phaseStartedAt : Time.Posix }
            , currentPhase : Int
        }
    -> Html Msg
practiceScreenView colors model =
    div
        [ style "background-color" <| Color.toCssString colors.backgroundColor
        , class "grid grid-flow-col gap-4 place-items-center font-main h-full"
        ]
    <|
        List.filterMap identity
            [ if millisecondFromBreathStep model.breathing model.currentPhase (Animator.current model.currentBreathStep) == IsUserInteractionNeeded then
                Just <| button [ onClick NextBreathStep ] [ text "Next Breath Step" ]

              else
                Nothing
            , model.timer
                |> Maybe.map (circleIndicator colors model.currentBreathStep)
            ]


practiceResultScreenView : BreathingResult -> Html Msg
practiceResultScreenView { seconds, phases } =
    let
        textElement : String -> Html Msg
        textElement content =
            p [ class "text-2xl" ] [ Html.text content ]

        numberElement : Float -> Html Msg
        numberElement number =
            p [ class "text-2xl font-bold text-blue-600" ] [ Html.text (String.fromFloat number) ]

        resultElement : String -> Float -> Html Msg
        resultElement label value =
            div [ class "flex justify-center items-center mt-4" ]
                [ textElement (label ++ "：")
                , numberElement value
                ]
    in
    div [ class "container mx-auto p-4 font-main" ]
        [ h1 [ class "text-4xl font-bold text-center" ] [ Html.text "練習結果" ]
        , resultElement "何秒呼吸をしたか" seconds
        , resultElement "何フェーズ呼吸をしたか" <| toFloat phases
        , button
            [ onClick GoToHomeMsg
            , class "bg-blue-500 text-white px-4 py-2 rounded-md shadow font-bold font-main"
            ]
            [ text "戻る" ]
        ]


view : { record | foregroundColor : Color.Color, backgroundColor : Color.Color } -> Model -> Html Msg
view colors model =
    case model.screen of
        PracticeScreen ->
            practiceScreenView colors model

        PracticeResultScreen result ->
            practiceResultScreenView result


type ShouldEndSubscription
    = ShouldEndSubscription
    | ShouldNotEndSubscription


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        shouldEndSubscription =
            case model.timer of
                Just timer ->
                    case isBreathingFinished timer.now timer of
                        BreathingFinished ->
                            ShouldEndSubscription

                        BreathingNotFinished ->
                            ShouldNotEndSubscription

                Nothing ->
                    ShouldNotEndSubscription
    in
    Sub.batch <|
        [ animator
            |> Animator.toSubscription Tick model
            |> (case shouldEndSubscription of
                    ShouldEndSubscription ->
                        always Sub.none

                    ShouldNotEndSubscription ->
                        identity
               )
        , Time.every 10 Tick
            |> (case shouldEndSubscription of
                    ShouldEndSubscription ->
                        always Sub.none

                    ShouldNotEndSubscription ->
                        identity
               )
        ]
