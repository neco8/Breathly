module CircleIndicator exposing (circleIndicator)

import Animator exposing (Timeline)
import Animator.Css
import BreathStep exposing (BreathStep(..))
import Color
import Color.Manipulate
import Html exposing (..)
import Html.Attributes exposing (class)
import Time


type alias TimelineModel =
    { size : Float
    , color : Color.Color
    }


formatTime : { timer | now : Time.Posix, nextTransitionAt : Time.Posix } -> String
formatTime timer =
    let
        remainingTime =
            Time.posixToMillis timer.nextTransitionAt - Time.posixToMillis timer.now

        remainingSeconds =
            toFloat remainingTime / 1000

        rounded =
            round <| remainingSeconds * 10

        ten =
            floor <| toFloat rounded / 10

        one =
            modBy 10 rounded
    in
    String.fromInt ten ++ "." ++ String.fromInt one


circleIndicator :
    { record | foregroundColor : Color.Color, backgroundColor : Color.Color }
    -> Timeline BreathStep
    ->
        { timer
            | now : Time.Posix
            , nextTransitionAt : Time.Posix
            , phaseStartedAt : Time.Posix
        }
    -> Html msg
circleIndicator colors timeline timer =
    let
        toDiameter size =
            size * 200

        formattedRemainingSeconds =
            formatTime timer
    in
    Animator.Css.div timeline
        [ Animator.Css.backgroundColor <|
            \breathStep ->
                case breathStepToTimelineModel colors breathStep of
                    { color } ->
                        color
        , Animator.Css.width <|
            \breathStep ->
                case breathStepToTimelineModel colors breathStep of
                    { size } ->
                        Animator.at <| toDiameter size
        -- , Animator.Css.height <|
        --     \breathStep ->
        --         case breathStepToTimelineModel colors breathStep of
        --             { size } ->
        --                 Animator.at <| toDiameter size
        ]
        [ class "rounded-full flex items-center justify-center text-3xl font-bold text-white aspect-square font-bodoni-moda"
        ]
        [ text <| formattedRemainingSeconds ++ "s" ]


breathStepToTimelineModel : { record | foregroundColor : Color.Color, backgroundColor : Color.Color } -> BreathStep -> TimelineModel
breathStepToTimelineModel { backgroundColor, foregroundColor } breathStep =
    case breathStep of
        Inhaling ->
            { color = Color.Manipulate.weightedMix foregroundColor backgroundColor 1
            , size = 1
            }

        InHolding ->
            { size = 1
            , color = Color.Manipulate.weightedMix foregroundColor backgroundColor 1
            }

        Exhaling ->
            { size = 0.5
            , color = Color.Manipulate.weightedMix foregroundColor backgroundColor 0.25
            }

        ExHolding ->
            { size = 0.5
            , color = Color.Manipulate.weightedMix foregroundColor backgroundColor 0.25
            }
