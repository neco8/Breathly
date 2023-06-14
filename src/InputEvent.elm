module InputEvent exposing (onEnter)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Decode


onEnter : { noOp : msg, msg : msg } -> Attribute msg
onEnter { noOp, msg } =
    on "keydown" <|
        Decode.map
            (\key ->
                if key == "Enter" then
                    msg

                else
                    noOp
            )
            (Decode.field "key" Decode.string)
