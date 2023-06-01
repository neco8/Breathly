module Breathing exposing (Model, Msg, init, subscriptions, update, view)

import Animator exposing (Timeline)
import Animator.Css
import BreathStep exposing (BreathStep(..))
import CircleIndicator exposing (circleIndicator)
import Color
import Expression exposing (ArithmeticOperator(..), BooleanExpr(..), BooleanOperator(..), Breathing(..), ComparisonExpr(..), ComparisonOperator(..), Duration(..), Exhale(..), Hold(..), Inhale(..), MinMaxOperator(..), Number(..), NumberExpr(..), TimeUnit(..))
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import String exposing (fromFloat)
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


isUserInteractionNeeded : Breathing -> BreathStep -> Bool
isUserInteractionNeeded breathing breathStep =
    case breathing of
        Breathing inhale maybeInhold exhale maybeExhold ->
            case breathStep of
                Inhaling ->
                    let
                        duration =
                            getInhaleDuration inhale
                    in
                    duration == Naturally || duration == AsLongAsPossible

                InHolding ->
                    case Maybe.map getHoldDuration maybeInhold of
                        Nothing ->
                            False

                        Just duration ->
                            duration == Naturally || duration == AsLongAsPossible

                Exhaling ->
                    let
                        duration =
                            getExhaleDuration exhale
                    in
                    duration == Naturally || duration == AsLongAsPossible

                ExHolding ->
                    case Maybe.map getHoldDuration maybeExhold of
                        Nothing ->
                            False

                        Just duration ->
                            duration == Naturally || duration == AsLongAsPossible


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


durationToMilliseconds : Duration -> Int -> Float
durationToMilliseconds duration phase =
    case duration of
        Naturally ->
            0

        AsLongAsPossible ->
            0

        NumberDuration numberExpr timeUnit ->
            let
                number =
                    evaluateNumberExpr numberExpr phase
            in
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


getNextTransitionTime : Breathing -> BreathStep -> Int -> Time.Posix -> Time.Posix
getNextTransitionTime breathing breathStep currentPhase currentTime =
    case breathing of
        Breathing inhale maybeInhold exhale maybeExhold ->
            case breathStep of
                Inhaling ->
                    let
                        duration =
                            getInhaleDuration inhale
                    in
                    durationToMilliseconds duration currentPhase
                        + toFloat (Time.posixToMillis currentTime)
                        |> round
                        |> Time.millisToPosix

                InHolding ->
                    maybeInhold
                        |> Maybe.map
                            (getHoldDuration
                                >> flip durationToMilliseconds currentPhase
                                >> (+) (toFloat (Time.posixToMillis currentTime))
                                >> round
                                >> Time.millisToPosix
                            )
                        |> Maybe.withDefault currentTime

                Exhaling ->
                    let
                        duration =
                            getExhaleDuration exhale
                    in
                    durationToMilliseconds duration currentPhase
                        + toFloat (Time.posixToMillis currentTime)
                        |> round
                        |> Time.millisToPosix

                ExHolding ->
                    maybeExhold
                        |> Maybe.map
                            (getHoldDuration
                                >> flip durationToMilliseconds currentPhase
                                >> (+) (toFloat (Time.posixToMillis currentTime))
                                >> round
                                >> Time.millisToPosix
                            )
                        |> Maybe.withDefault currentTime


type Msg
    = Tick Time.Posix
    | NextBreathStep
    | AutoNextBreathStep
    | InitTimer Int Time.Posix


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



-- 構造学習→国語の構造的読解力


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


andThen : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen f ( model, cmd ) =
    let
        ( nextModel, nextCmd ) =
            f model
    in
    ( nextModel, Cmd.batch [ cmd, nextCmd ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitTimer time currentTime ->
            ( model, Cmd.none )
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
                        )
                    )
                |> andThen (update AutoNextBreathStep)

        _ ->
            case model.timer of
                Nothing ->
                    -- init後必ずtimerが存在するので、ここに来ることはない
                    ( model, Cmd.none )

                Just timer ->
                    case msg of
                        InitTimer _ _ ->
                            ( model, Cmd.none )

                        Tick newTime ->
                            case isBreathingFinished newTime timer of
                                BreathingFinished ->
                                    ( { model
                                        | screen =
                                            PracticeResultScreen
                                                { breathingId = model.breathing
                                                , seconds =
                                                    (toFloat <| Time.posixToMillis newTime - Time.posixToMillis timer.breathingStartedAt)
                                                        / 1000
                                                , phases = model.currentPhase
                                                }
                                        , timer = Just { timer | now = newTime }
                                      }
                                    , Cmd.none
                                    )

                                BreathingNotFinished ->
                                    ( model, Cmd.none )
                                        |> andThen (\prev -> ( { prev | timer = Just { timer | now = newTime } }, Cmd.none ))
                                        |> andThen (\prev -> ( Animator.update newTime animator prev, Cmd.none ))
                                        |> andThen
                                            (if
                                                not (isUserInteractionNeeded model.breathing (Animator.current model.currentBreathStep))
                                                    && Time.posixToMillis newTime
                                                    >= Time.posixToMillis timer.nextTransitionAt
                                             then
                                                update AutoNextBreathStep

                                             else
                                                \m -> ( m, Cmd.none )
                                            )

                        NextBreathStep ->
                            let
                                nextBreathStep =
                                    getNextBreathStep <| Animator.current model.currentBreathStep

                                nextPhase =
                                    case Animator.current model.currentBreathStep of
                                        ExHolding ->
                                            model.currentPhase + 1

                                        _ ->
                                            model.currentPhase

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
                            )

                        AutoNextBreathStep ->
                            update NextBreathStep model


practiceScreenView :
    { colors | backgroundColor : Color.Color, foregroundColor : Color.Color }
    ->
        { model
            | breathing : Breathing
            , currentBreathStep : Timeline BreathStep
            , timer : Maybe { timer | now : Time.Posix, nextTransitionAt : Time.Posix, phaseStartedAt : Time.Posix }
        }
    -> Html Msg
practiceScreenView colors model =
    div
        [ style "background-color" <| Color.toCssString colors.backgroundColor
        , class "grid grid-flow-col gap-4 place-items-center font-main h-full"
        ]
    <|
        List.filterMap identity
            [ if isUserInteractionNeeded model.breathing (Animator.current model.currentBreathStep) then
                Just <| button [ onClick NextBreathStep ] [ text "Next Breath Step" ]

              else
                Nothing
            , model.timer
                |> Maybe.map (circleIndicator colors model.currentBreathStep)
            ]


practiceResultScreenView : BreathingResult -> Html msg
practiceResultScreenView { seconds, phases } =
    let
        -- テキストを表示する関数を定義しました。
        textElement : String -> Html msg
        textElement content =
            p [ class "text-2xl" ] [ Html.text content ]

        -- 数値を表示する関数を定義しました。
        numberElement : Float -> Html msg
        numberElement number =
            p [ class "text-2xl font-bold text-blue-600" ] [ Html.text (String.fromFloat number) ]

        -- 結果を表示する関数を定義しました。
        resultElement : String -> Float -> Html msg
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
