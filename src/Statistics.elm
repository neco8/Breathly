module Statistics exposing (view)

import Dict exposing (Dict)
import ExpressionSerializer exposing (breathingToString)
import Html exposing (Html, div, label, span, text)
import Html.Attributes exposing (class)
import Time
import Type exposing (BreathingResult, BreathingTechnique)


type alias Model model =
    { model
        | sessions : List BreathingResult
        , breathingTechniques : List BreathingTechnique
    }


statisticsView : { totalSeconds : Int, consecutivePracticeDays : Int, mostFrequentBreathingTechnique : BreathingTechnique, practiceTrend : Dict Int Int } -> Html msg
statisticsView { consecutivePracticeDays, mostFrequentBreathingTechnique, practiceTrend, totalSeconds } =
    let
        cardView props =
            div [ class "rounded-md bg-slate-100 shadow-md px-4 py-3 grid grid-rows-[auto,auto] items-start justify-start", props.class ]
                [ label [ class "text-xs text-slate-500" ] [ text props.label ]
                , span [ class "mt-0.5 text-3xl font-bold text-slate-800" ] [ text props.text ]
                ]
    in
    div []
        [ cardView
            { class = class ""
            , text = String.fromInt totalSeconds
            , label = "合計時間"
            }
        , cardView
            { class = class "mt-4"
            , text = String.fromInt consecutivePracticeDays
            , label = "連続練習日数"
            }
        , cardView
            { class = class "mt-4"
            , label = "一番頻繁にやった呼吸法 title"
            , text = mostFrequentBreathingTechnique.title
            }
        , cardView
            { class = class "mt-4"
            , label = "一番頻繁にやった呼吸法 breathing"
            , text = breathingToString mostFrequentBreathingTechnique.breathing
            }
        , cardView
            { class = class "mt-4"
            , label = "トレンド"
            , text =
                String.concat <|
                    List.map (\( a, b ) -> String.fromInt a ++ String.fromInt b) <|
                        Dict.toList practiceTrend
            }
        ]


view : Model model -> Html msg
view { sessions, breathingTechniques } =
    div [] <|
        List.map
            (\bt ->
                statisticsView
                    { totalSeconds = 10 * 60 * 10
                    , consecutivePracticeDays = 10
                    , mostFrequentBreathingTechnique = bt
                    , practiceTrend = Dict.fromList [ ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ), ( 1, 1 ) ]
                    }
            )
            breathingTechniques
