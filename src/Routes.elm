module Routes exposing (Route(..), fromUrl, href, toString)

import Expression exposing (Number(..))
import Html
import Html.Attributes as A
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


type Route
    = HomeRoute
    | StatisticsRoute
    | AddBreathingTechniqueRoute


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse routeParser url


toString : Route -> String
toString route =
    case route of
        HomeRoute ->
            "/"

        StatisticsRoute ->
            "/statistics"

        AddBreathingTechniqueRoute ->
            "/add"


href : Route -> Html.Attribute msg
href route =
    A.href <| toString route


routeParser : Parser (Route -> a) a
routeParser =
    let
        routeNext routes =
            case List.head routes of
                Nothing ->
                    routeNext <| HomeRoute :: routes

                Just HomeRoute ->
                    routeNext <| StatisticsRoute :: routes

                Just StatisticsRoute ->
                    routeNext <| AddBreathingTechniqueRoute :: routes

                Just AddBreathingTechniqueRoute ->
                    routes

        routeAll =
            routeNext []
    in
    Parser.oneOf <|
        List.map
            (\route ->
                case route of
                    HomeRoute ->
                        Parser.map route Parser.top

                    StatisticsRoute ->
                        Parser.map route (s "statistics")

                    AddBreathingTechniqueRoute ->
                        Parser.map route (s "add")
            )
            routeAll
