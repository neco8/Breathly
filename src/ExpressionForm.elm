module ExpressionForm exposing (Model, Msg, breathingInputView, init, update, advancedBreathingInputView)

import Dict exposing (Dict)
import Expression exposing (ArithmeticOperator(..), BooleanExpr(..), BooleanOperator(..), Breathing(..), ComparisonExpr(..), ComparisonOperator(..), Duration(..), Exhale(..), Hold(..), Inhale(..), MinMaxOperator(..), Number(..), NumberExpr(..), TimeUnit(..))
import ExpressionParser as P
import ExpressionSerializer as S
import Html exposing (Attribute, Html, button, div, h1, input, label, text)
import Html.Attributes exposing (class, disabled, selected, type_, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Decode
import Parser


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


type InputState
    = Inputting String
    | Ready


type alias Model =
    { breathing : Breathing
    , textInputState : Dict String InputState
    }


type Msg
    = ChangeBreathing Breathing
    | ChangeTextInput { key : String, value : String }
    | FinishTextInput { key : String, value : Breathing }


init : Breathing -> Model
init breathing =
    { breathing = breathing, textInputState = Dict.empty }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeBreathing breathing ->
            { model | breathing = breathing }

        ChangeTextInput { key, value } ->
            { model | textInputState = Dict.insert key (Inputting value) model.textInputState }

        FinishTextInput { key, value } ->
            { model | breathing = value, textInputState = Dict.remove key model.textInputState }


dropdownInput : { class : Maybe (Attribute Msg), toString : value -> String, encode : value -> String, decode : String -> Maybe value, onSelect : value -> Msg, choices : List value, selected : value } -> Html Msg
dropdownInput props =
    let
        optionElements =
            List.map
                (\choice ->
                    Html.option
                        [ value <| props.encode choice
                        , if choice == props.selected then
                            selected True

                          else
                            selected False
                        ]
                        [ Html.text <| props.toString choice ]
                )
                props.choices
    in
    Html.div [ Maybe.withDefault (class "") props.class ]
        [ Html.select
            [ on "change" <|
                (targetValue
                    |> Decode.andThen
                        (\s ->
                            case props.decode s of
                                Just value ->
                                    Decode.succeed <| props.onSelect value

                                Nothing ->
                                    Decode.fail "invalid value"
                        )
                )
            , class "border py-2 pl-2 pr-3 rounded text-sm"
            ]
            optionElements
        ]


textInput : { class : Maybe (Attribute Msg), value : String, textInputState : Maybe String, onInput : String -> Msg, onFinish : String -> Maybe Msg } -> Html Msg
textInput props =
    div [ class "flex", Maybe.withDefault (class "") props.class ] <|
        List.filterMap identity
            [ Just <|
                input
                    [ onInput props.onInput
                    , value <| Maybe.withDefault props.value props.textInputState
                    , class "border px-3 py-2 rounded border-blue-100"
                    ]
                    []
            , Maybe.map
                (always <|
                    button
                        (List.filterMap identity
                            [ Maybe.map onClick <| Maybe.andThen props.onFinish props.textInputState
                            , Just <| class "bg-blue-500 shadow-md shadow-blue-200 disabled:shadow-none disabled:bg-blue-200 disabled:cursor-not-allowed text-white px-4 py-2 rounded ml-2"
                            , Just <| type_ "button"
                            , case Maybe.andThen props.onFinish props.textInputState of
                                Nothing ->
                                    Just <| disabled True

                                Just _ ->
                                    Nothing
                            ]
                        )
                        [ text "完了" ]
                )
                props.textInputState
            ]


numberInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> Number -> (Number -> Breathing) -> Html Msg
numberInput key textInputState class_ value toBreathing =
    let
        defaultNumberStaticNumber =
            StaticNumber 0

        choiceNext acc =
            case List.head acc of
                Nothing ->
                    defaultNumberStaticNumber :: acc |> choiceNext

                Just (StaticNumber _) ->
                    PhaseNumber :: acc |> choiceNext

                Just PhaseNumber ->
                    acc

        choices =
            choiceNext []
                |> List.map
                    (\v ->
                        case ( value, v ) of
                            ( StaticNumber _, StaticNumber _ ) ->
                                value

                            ( PhaseNumber, PhaseNumber ) ->
                                value

                            _ ->
                                v
                    )
    in
    div [ cardClass, Maybe.withDefault (class "") class_, class "grid grid-cols-[auto,1fr] gap-4" ] <|
        dropdownInput
            { selected = value
            , onSelect = toBreathing >> ChangeBreathing
            , choices = choices
            , toString =
                \v ->
                    case v of
                        StaticNumber _ ->
                            "数値"

                        PhaseNumber ->
                            "何呼吸目"
            , encode = S.numberToString
            , decode = Parser.run P.number >> Result.toMaybe
            , class = Just <| class "h-full"
            }
            :: (case value of
                    StaticNumber number ->
                        let
                            numberKey =
                                key ++ "staticnumber"
                        in
                        [ textInput
                            { value = String.fromInt number
                            , textInputState =
                                Dict.get numberKey textInputState
                                    |> Maybe.map
                                        (\x ->
                                            case x of
                                                Inputting s ->
                                                    s

                                                Ready ->
                                                    ""
                                        )
                            , onInput = \s -> ChangeTextInput { key = numberKey, value = s }
                            , onFinish =
                                String.toInt
                                    >> Maybe.map
                                        (StaticNumber
                                            >> toBreathing
                                            >> (\v ->
                                                    FinishTextInput
                                                        { key = numberKey
                                                        , value = v
                                                        }
                                               )
                                        )
                            , class = Just <| class ""
                            }
                        ]

                    PhaseNumber ->
                        []
               )


arithmeticExprInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> Number -> ArithmeticOperator -> NumberExpr -> (( Number, ArithmeticOperator, NumberExpr ) -> Breathing) -> Html Msg
arithmeticExprInput key textInputState class_ left operator right toBreathing =
    let
        operatorDropdown c =
            dropdownInput
                { selected = operator
                , onSelect = \op -> toBreathing ( left, op, right ) |> ChangeBreathing
                , choices = [ Plus, Minus, Times, Divide ]
                , toString =
                    \op ->
                        case op of
                            Plus ->
                                "＋"

                            Minus ->
                                "－"

                            Times ->
                                "×"

                            Divide ->
                                "÷"
                , decode =
                    \encoded ->
                        case encoded of
                            "Plus" ->
                                Just Plus

                            "Minus" ->
                                Just Minus

                            "Times" ->
                                Just Times

                            "Divide" ->
                                Just Divide

                            _ ->
                                Nothing
                , encode =
                    \op ->
                        case op of
                            Plus ->
                                "Plus"

                            Minus ->
                                "Minus"

                            Times ->
                                "Times"

                            Divide ->
                                "Divide"
                , class = c
                }
    in
    div [ cardClass, Maybe.withDefault (class "") class_ ]
        [ numberInput (key ++ "left") textInputState Nothing left (\l -> toBreathing ( l, operator, right ))
        , operatorDropdown <| Just <| class "mt-4"
        , numberExprInput (key ++ "right") textInputState (Just <| class "mt-4") right (\r -> toBreathing ( left, operator, r ))
        ]


minMaxExprInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> MinMaxOperator -> Number -> NumberExpr -> (( MinMaxOperator, Number, NumberExpr ) -> Breathing) -> Html Msg
minMaxExprInput key textInputState class_ operator left right toBreathing =
    let
        operatorDropdown c =
            dropdownInput
                { selected = operator
                , onSelect = \op -> toBreathing ( op, left, right ) |> ChangeBreathing
                , choices = [ Min, Max ]
                , toString =
                    \op ->
                        case op of
                            Min ->
                                "min"

                            Max ->
                                "max"
                , encode =
                    \op ->
                        case op of
                            Min ->
                                "min"

                            Max ->
                                "max"
                , decode =
                    \s ->
                        case s of
                            "min" ->
                                Just Min

                            "max" ->
                                Just Max

                            _ ->
                                Nothing
                , class = c
                }
    in
    div [ cardClass, Maybe.withDefault (class "") class_ ]
        [ label [ class "font-bold text-sm" ] [ text "最大・最小" ]
        , operatorDropdown (Just <| class "mt-2")
        , numberInput (key ++ "left") textInputState (Just <| class "mt-4") left (\l -> toBreathing ( operator, l, right ))
        , numberExprInput (key ++ "right") textInputState (Just <| class "mt-4") right (\r -> toBreathing ( operator, left, r ))
        ]


numberExprInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> NumberExpr -> (NumberExpr -> Breathing) -> Html Msg
numberExprInput key textInputState class_ value toBreathing =
    let
        defaultNumber =
            StaticNumber 0

        defaultNumberExprNumber =
            Number defaultNumber

        defaultNumberExprArithmeticExpr =
            ArithmeticExpr defaultNumber Plus defaultNumberExprNumber

        defaultNumberExprMinMaxExpr =
            MinMaxExpr Min defaultNumber defaultNumberExprNumber

        choiceNext acc =
            case List.head acc of
                Nothing ->
                    defaultNumberExprNumber :: acc |> choiceNext

                Just (Number _) ->
                    defaultNumberExprArithmeticExpr :: acc |> choiceNext

                Just (ArithmeticExpr _ _ _) ->
                    defaultNumberExprMinMaxExpr :: acc |> choiceNext

                Just (MinMaxExpr _ _ _) ->
                    acc

        choices =
            choiceNext []
                |> List.map
                    (\v ->
                        case ( value, v ) of
                            ( Number _, Number _ ) ->
                                value

                            ( ArithmeticExpr _ _ _, ArithmeticExpr _ _ _ ) ->
                                value

                            ( MinMaxExpr _ _ _, MinMaxExpr _ _ _ ) ->
                                value

                            _ ->
                                v
                    )
    in
    div [ cardClass, Maybe.withDefault (class "") class_ ]
        [ label [ class "font-bold text-sm" ] [ text "数値式" ]
        , dropdownInput
            { selected = value
            , onSelect = toBreathing >> ChangeBreathing
            , choices = choices
            , toString =
                \v ->
                    case v of
                        Number _ ->
                            "数値"

                        ArithmeticExpr _ _ _ ->
                            "計算式"

                        MinMaxExpr _ _ _ ->
                            "最大最小"
            , encode =
                S.numberExprToString
            , decode =
                Parser.run P.numberExpr >> Result.toMaybe
            , class = Just <| class "mt-2"
            }
        , case value of
            Number number ->
                numberInput (key ++ "number") textInputState (Just <| class "mt-4") number (Number >> toBreathing)

            ArithmeticExpr left operator right ->
                arithmeticExprInput (key ++ "arithmeticexpr") textInputState (Just <| class "mt-4") left operator right ((\f ( a, b, c ) -> f a b c) ArithmeticExpr >> toBreathing)

            MinMaxExpr operator left right ->
                minMaxExprInput (key ++ "minmaxexpr") textInputState (Just <| class "mt-4") operator left right ((\f ( a, b, c ) -> f a b c) MinMaxExpr >> toBreathing)
        ]


timeUnitAll : List TimeUnit
timeUnitAll =
    let
        timeUnitNext acc =
            case List.head acc of
                Nothing ->
                    Seconds :: acc |> timeUnitNext

                Just Seconds ->
                    Minutes :: acc |> timeUnitNext

                Just Minutes ->
                    acc
    in
    timeUnitNext []


timeUnitInput : Maybe (Attribute Msg) -> TimeUnit -> (TimeUnit -> Breathing) -> Html Msg
timeUnitInput class_ value toBreathing =
    dropdownInput
        { toString =
            \v ->
                case v of
                    Seconds ->
                        "秒"

                    Minutes ->
                        "分"
        , onSelect = toBreathing >> ChangeBreathing
        , choices = timeUnitAll
        , selected = value
        , encode = S.timeUnitToString
        , decode = Parser.run P.timeUnit >> Result.toMaybe
        , class = class_
        }


inhaleInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> Inhale -> (Inhale -> Breathing) -> Html Msg
inhaleInput key textInputState class_ value toBreathing =
    div [ cardClass, Maybe.withDefault (class "") class_ ]
        [ label [ class "font-bold text-sm" ] [ text "Inhale" ]
        , case value of
            Inhale duration ->
                durationInput
                    (key ++ "duration")
                    textInputState
                    (Just <| class "mt-4")
                    duration
                    (\d -> toBreathing (Inhale d))
        ]


exhaleInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> Exhale -> (Exhale -> Breathing) -> Html Msg
exhaleInput key textInputState class_ value toBreathing =
    div [ cardClass, Maybe.withDefault (class "") class_ ]
        [ label [ class "font-bold text-sm" ] [ text "Exhale" ]
        , case value of
            Exhale duration ->
                durationInput (key ++ "duration")
                    textInputState
                    (Just <| class "mt-2")
                    duration
                    (\d -> toBreathing (Exhale d))
        ]


holdInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> Hold -> (Hold -> Breathing) -> Html Msg
holdInput key textInputState class_ value toBreathing =
    let
        holdDuration =
            case value of
                Hold duration ->
                    duration
    in
    durationInput (key ++ "duration") textInputState class_ holdDuration (\d -> toBreathing (Hold d))


durationInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> Duration -> (Duration -> Breathing) -> Html Msg
durationInput key textInputState class_ value toBreathing =
    let
        durationDropdown =
            dropdownInput
                { selected = value
                , onSelect = toBreathing >> ChangeBreathing
                , choices =
                    [ NumberDuration (Number (StaticNumber 0)) Seconds
                    , AsLongAsPossible
                    , Naturally
                    , ConditionalDuration (ComparisonExpr (Comparison (Number (StaticNumber 1)) EqualTo (Number (StaticNumber 1)))) (NumberDuration (Number (StaticNumber 1)) Seconds) (NumberDuration (Number (StaticNumber 1)) Seconds)
                    ]
                , toString =
                    \v ->
                        case v of
                            NumberDuration _ _ ->
                                "数値"

                            AsLongAsPossible ->
                                "できるだけ長く"

                            Naturally ->
                                "自然に"

                            ConditionalDuration _ _ _ ->
                                "条件式"
                , encode = S.durationToString
                , decode = Parser.run P.duration >> Result.toMaybe
                , class = Just <| class "mt-2"
                }
    in
    div [ cardClass, Maybe.withDefault (class "") class_ ]
        [ label [ class "font-bold text-sm" ] [ text "時間" ]
        , durationDropdown
        , case value of
            NumberDuration numberExpr timeUnit ->
                div [ class "mt-4" ]
                    [ numberExprInput (key ++ "numberexpr") textInputState Nothing numberExpr (\ne -> toBreathing (NumberDuration ne timeUnit))
                    , timeUnitInput (Just <| class "mt-4") timeUnit (\tu -> toBreathing (NumberDuration numberExpr tu))
                    ]

            AsLongAsPossible ->
                text ""

            Naturally ->
                text ""

            ConditionalDuration booleanExpr duration1 duration2 ->
                conditionalDurationInput (key ++ "conditionalduration") textInputState (Just <| class "mt-4") booleanExpr duration1 duration2 (\b d1 d2 -> toBreathing (ConditionalDuration b d1 d2))
        ]


booleanOperatorInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> BooleanOperator -> ComparisonExpr -> BooleanExpr -> (BooleanOperator -> ComparisonExpr -> BooleanExpr -> Breathing) -> Html Msg
booleanOperatorInput key textInputState class_ booleanOperator comparisonExpr booleanExpr2 toBreathing =
    let
        operatorDropdown =
            dropdownInput
                { selected = booleanOperator
                , onSelect = \op -> toBreathing op comparisonExpr booleanExpr2 |> ChangeBreathing
                , choices = [ And, Or ]
                , toString =
                    \op ->
                        case op of
                            And ->
                                "かつ"

                            Or ->
                                "または"
                , encode = S.booleanOperatorToString
                , decode = Parser.run P.booleanOperator >> Result.toMaybe
                , class = Just <| class "mt-4"
                }
    in
    div [ cardClass, Maybe.withDefault (class "") class_ ]
        [ comparisionExprInput (key ++ "comparisonExpr1") textInputState Nothing comparisonExpr (\be1 -> toBreathing booleanOperator be1 booleanExpr2)
        , operatorDropdown
        , booleanExprInput (key ++ "booleanexpr2") textInputState (Just <| class "mt-4") booleanExpr2 (\be2 -> toBreathing booleanOperator comparisonExpr be2)
        ]


comparisionExprInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> ComparisonExpr -> (ComparisonExpr -> Breathing) -> Html Msg
comparisionExprInput key textInputState class_ value toBreathing =
    case value of
        Comparison numberExpr1 comparisonOperator numberExpr2 ->
            let
                operatorDropdown =
                    dropdownInput
                        { selected = comparisonOperator
                        , onSelect = \op -> toBreathing (Comparison numberExpr1 op numberExpr2) |> ChangeBreathing
                        , choices = [ GreaterThanOrEqual, LessThanOrEqual, GreaterThan, LessThan, EqualTo, NotEqualTo ]
                        , toString =
                            \op ->
                                case op of
                                    GreaterThanOrEqual ->
                                        "≥"

                                    LessThanOrEqual ->
                                        "≤"

                                    GreaterThan ->
                                        ">"

                                    LessThan ->
                                        "<"

                                    EqualTo ->
                                        "="

                                    NotEqualTo ->
                                        "≠"
                        , encode =
                            \op ->
                                case op of
                                    GreaterThanOrEqual ->
                                        "≥"

                                    LessThanOrEqual ->
                                        "≤"

                                    GreaterThan ->
                                        ">"

                                    LessThan ->
                                        "<"

                                    EqualTo ->
                                        "="

                                    NotEqualTo ->
                                        "≠"
                        , decode =
                            \s ->
                                case s of
                                    "≥" ->
                                        Just GreaterThanOrEqual

                                    "≤" ->
                                        Just LessThanOrEqual

                                    ">" ->
                                        Just GreaterThan

                                    "<" ->
                                        Just LessThan

                                    "=" ->
                                        Just EqualTo

                                    "≠" ->
                                        Just NotEqualTo

                                    _ ->
                                        Nothing
                        , class = Just <| class "mt-4"
                        }
            in
            div [ cardClass, Maybe.withDefault (class "") class_ ]
                [ label [ class "text-sm font-bold" ] [ text "比較" ]
                , numberExprInput (key ++ "numberexpr1") textInputState (Just <| class "mt-2") numberExpr1 (\ne1 -> toBreathing (Comparison ne1 comparisonOperator numberExpr2))
                , operatorDropdown
                , numberExprInput (key ++ "numberexpr2") textInputState (Just <| class "mt-4") numberExpr2 (\ne2 -> toBreathing (Comparison numberExpr1 comparisonOperator ne2))
                ]


booleanExprInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> BooleanExpr -> (BooleanExpr -> Breathing) -> Html Msg
booleanExprInput key textInputState class_ value toBreathing =
    case value of
        ComparisonExpr comparisonExpr ->
            comparisionExprInput (key ++ "comparisonexpr") textInputState class_ comparisonExpr (\ce -> toBreathing (ComparisonExpr ce))

        BooleanOperator comparisonExpr booleanOperator booleanExpr2 ->
            booleanOperatorInput (key ++ "booleanoperator") textInputState class_ booleanOperator comparisonExpr booleanExpr2 (\op be1 be2 -> toBreathing (BooleanOperator be1 op be2))


conditionalDurationInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> BooleanExpr -> Duration -> Duration -> (BooleanExpr -> Duration -> Duration -> Breathing) -> Html Msg
conditionalDurationInput key textInputState class_ booleanExpr duration1 duration2 toBreathing =
    div [ cardClass, Maybe.withDefault (class "") class_ ]
        [ label [ class "text-sm font-bold" ] [ text "if" ]
        , booleanExprInput (key ++ "booleanexpr") textInputState Nothing booleanExpr (\be -> toBreathing be duration1 duration2)
        , label [ class "text-sm font-bold mt-4" ] [ text "then" ]
        , durationInput (key ++ "duration1") textInputState (Just <| class "mt-2") duration1 (\d1 -> toBreathing booleanExpr d1 duration2)
        , label [ class "text-sm font-bold mt-4" ] [ text "else" ]
        , durationInput (key ++ "duration2") textInputState (Just <| class "mt-2") duration2 (\d2 -> toBreathing booleanExpr duration1 d2)
        ]


maybeHoldInput : String -> Dict String InputState -> Maybe (Attribute Msg) -> Maybe Hold -> (Maybe Hold -> Breathing) -> Html Msg
maybeHoldInput key textInputState class_ maybeHold toBreathing =
    let
        holdChoices =
            [ Nothing
            , Just (Hold (NumberDuration (Number (StaticNumber 0)) Seconds))
            ]
                |> List.map
                    (\mHold ->
                        case ( maybeHold, mHold ) of
                            ( Nothing, Nothing ) ->
                                maybeHold

                            ( Just _, Just _ ) ->
                                maybeHold

                            _ ->
                                mHold
                    )

        holdDropdown =
            dropdownInput
                { selected = maybeHold
                , onSelect = toBreathing >> ChangeBreathing
                , choices = holdChoices
                , toString =
                    \mHold ->
                        case mHold of
                            Nothing ->
                                "止めない"

                            Just _ ->
                                "止める"
                , encode = Maybe.map S.holdToString >> Maybe.withDefault "Nothing"
                , decode =
                    \s ->
                        case s of
                            "Nothing" ->
                                Just Nothing

                            _ ->
                                s
                                    |> Parser.run P.hold
                                    >> Result.toMaybe
                                    >> Maybe.map Just
                , class = Just <| class "mt-2"
                }

        holdInputWrapper =
            case maybeHold of
                Just hold ->
                    holdInput key textInputState (Just <| class "mt-4") hold (Just >> toBreathing)

                Nothing ->
                    text ""
    in
    div [ cardClass, Maybe.withDefault (class "") class_ ]
        [ label [ class "text-sm font-bold" ] [ text "Hold?" ]
        , holdDropdown
        , holdInputWrapper
        ]


advancedBreathingInput : String -> Dict String InputState -> Breathing -> Html Msg
advancedBreathingInput key textInputState breathing =
    case breathing of
        Breathing inhale inhaleHold exhale exhaleHold ->
            let
                updateInhale newInhale =
                    Breathing newInhale inhaleHold exhale exhaleHold

                updateExhale newExhale =
                    Breathing inhale inhaleHold newExhale exhaleHold

                updateInhaleHold newInhaleHold =
                    Breathing inhale newInhaleHold exhale exhaleHold

                updateExhaleHold newExhaleHold =
                    Breathing inhale inhaleHold exhale newExhaleHold
            in
            div [ cardClass, class "grid grid-rows-[auto,auto,auto,auto] gap-4" ]
                [ inhaleInput
                    (key ++ "inhale")
                    textInputState
                    (Just <| class "")
                    inhale
                    updateInhale
                , maybeHoldInput
                    (key ++ "inhalehold")
                    textInputState
                    (Just <| class "")
                    inhaleHold
                    updateInhaleHold
                , exhaleInput
                    (key ++ "exhale")
                    textInputState
                    (Just <| class "")
                    exhale
                    updateExhale
                , maybeHoldInput
                    (key ++ "exhalehold")
                    textInputState
                    (Just <| class "")
                    exhaleHold
                    updateExhaleHold
                ]


breathingInput : String -> Dict String InputState -> Breathing -> Html Msg
breathingInput key textInputState breathing =
    case breathing of
        Breathing inhale inhaleHold exhale exhaleHold ->
            let
                updateInhale duration =
                    Breathing (Inhale duration) inhaleHold exhale exhaleHold

                updateExhale duration =
                    Breathing inhale inhaleHold (Exhale duration) exhaleHold

                updateInhaleHold duration =
                    Breathing inhale (Just (Hold duration)) exhale exhaleHold

                updateExhaleHold duration =
                    Breathing inhale inhaleHold exhale (Just (Hold duration))

                inputView : { label : String, updater : Duration -> Breathing, value : Maybe Duration, textInputState : Dict String InputState, key : String } -> Html Msg
                inputView props =
                    div [ cardClass ]
                        [ label [ class "text-sm font-bold" ] [ text props.label ]
                        , textInput
                            { class = Just <| class "mt-2"
                            , value =
                                case props.value of
                                    Just (NumberDuration (Number (StaticNumber n)) _) ->
                                        String.fromInt n

                                    _ ->
                                        ""
                            , textInputState =
                                Dict.get props.key props.textInputState
                                    |> Maybe.map
                                        (\x ->
                                            case x of
                                                Inputting s ->
                                                    s

                                                Ready ->
                                                    ""
                                        )
                            , onInput = \s -> ChangeTextInput { key = props.key, value = s }
                            , onFinish =
                                String.toInt
                                    >> Maybe.map
                                        (StaticNumber
                                            >> Number
                                            >> flip NumberDuration Seconds
                                            >> (\duration -> props.updater duration)
                                            >> (\v ->
                                                    FinishTextInput
                                                        { key = props.key
                                                        , value = v
                                                        }
                                               )
                                        )
                            }
                        ]
            in
            div [ cardClass, class "grid grid-rows-[auto,auto,auto,auto] gap-4" ]
                [ case inhale of
                    Inhale duration ->
                        inputView { label = "Inhale", value = Just duration, updater = updateInhale, textInputState = textInputState, key = key ++ "inhale" }
                , inputView
                    { label = "Inhale Hold"
                    , value =
                        inhaleHold
                            |> Maybe.map
                                (\(Hold duration) ->
                                    duration
                                )
                    , updater = updateInhaleHold
                    , textInputState = textInputState
                    , key = key ++ "inhalehold"
                    }
                , case exhale of
                    Exhale duration ->
                        inputView { label = "Exhale", value = Just duration, updater = updateExhale, textInputState = textInputState, key = key ++ "exhale" }
                , inputView
                    { label = "Exhale Hold"
                    , value =
                        exhaleHold
                            |> Maybe.map
                                (\(Hold duration) ->
                                    duration
                                )
                    , updater = updateExhaleHold
                    , textInputState = textInputState
                    , key = key ++ "exhalehold"
                    }
                ]


cardClass : Attribute Msg
cardClass =
    class "shadow-md shadow-blue-100 px-5 py-4 rounded-lg bg-[#bfe9fa20] border-[#12156b20] border"


breathingInputView : Model -> Html Msg
breathingInputView model =
    breathingInput "breathing" model.textInputState model.breathing


advancedBreathingInputView : Model -> Html Msg
advancedBreathingInputView model =
    advancedBreathingInput "breathing" model.textInputState model.breathing
