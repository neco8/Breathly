module NotFound exposing (document)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)


document : { title : String, body : List (Html msg) }
document =
    let
        status =
            "404"

        description =
            "This page could not be found."

        fourOFourView =
            span [ class "font-semibold text-3xl py-2" ] [ text status ]

        dashView =
            div [ class "w-px bg-slate-400 h-full" ] []

        textView =
            span [ class "font-light text-base" ] [ text description ]
    in
    { title = status ++ ": " ++ description
    , body =
        [ div [ class "items-center h-full w-full grid place-content-center" ]
            [ div [ class "grid grid-flow-col gap-6 items-center" ]
                [ fourOFourView
                , dashView
                , textView
                ]
            ]
        ]
    }
