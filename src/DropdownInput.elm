module DropdownInput exposing (..)

import Html exposing (Attribute)
import Html exposing (Html)
import Html.Attributes exposing (value)
import Html.Attributes exposing (selected)
import Html.Attributes exposing (class)
import Html.Events exposing (on)
import Html.Events exposing (targetValue)
import Json.Decode as Decode


dropdownInput :
    { class : Maybe (Attribute msg)
    , toString : value -> String
    , encode : value -> String
    , decode : String -> Maybe value
    , onSelect : value -> msg
    , choices : List value
    , selected : value
    }
    -> Html msg
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
