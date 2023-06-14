module MaterialIcon exposing (materialIconView)

import Html exposing (Attribute, Html, span, text)
import Html.Attributes exposing (class)


materialIconView : { iconType : String, class : Attribute msg } -> Html msg
materialIconView props =
    span [ class "material-symbols-outlined", props.class ]
        [ text props.iconType ]
