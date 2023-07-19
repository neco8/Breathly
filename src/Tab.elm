module Tab exposing (tabView)

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class)
import MaterialIcon exposing (materialIconView)
import Routes


tabView : Html msg
tabView =
    div [ class "grid grid-cols-[auto,auto] gap-4 max-w-screen-sm" ] <|
        List.map
            (\( route, iconType ) ->
                a [ Routes.href route, class "bg-slate-50 rounded-lg place-content-center grid py-4" ]
                    [ materialIconView
                        { class = class "text-slate-700", iconType = iconType }
                    ]
            )
            [ ( Routes.HomeRoute, "home" )
            , ( Routes.StatisticsRoute, "insights" )
            ]
