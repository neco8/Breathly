port module MinSecInput exposing (Model, Msg, init, toSeconds, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode


type Model
    = Model
        { time : InputState Int
        , key : Int
        }


type InputState a
    = Ready
    | Selected a
    | Inputting a


inputStateToMaybe : InputState a -> Maybe a
inputStateToMaybe i =
    case i of
        Ready ->
            Nothing

        Selected a ->
            Just a

        Inputting a ->
            Just a


toSeconds : Model -> Int
toSeconds (Model model) =
    case inputStateToMaybe model.time of
        Just time ->
            let
                seconds =
                    modBy 100 time

                minutes =
                    time // 100
            in
            minutes * 60 + seconds

        Nothing ->
            0


init : Int -> Model
init key =
    Model
        { time = Ready
        , key = key
        }


type Msg
    = UpdateTime String
    | SelectInput String
    | BlurInput
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        UpdateTime newTime ->
            ( Model <|
                if String.length newTime <= 5 then
                    case String.toInt newTime of
                        Just val ->
                            { model | time = Inputting val }

                        Nothing ->
                            if String.isEmpty newTime then
                                { model | time = Ready }

                            else
                                model

                else
                    model
            , Cmd.none
            )

        SelectInput id ->
            ( Model
                { model
                    | time =
                        case model.time of
                            Ready ->
                                Selected 0

                            Inputting t ->
                                Selected t

                            Selected t ->
                                Selected t
                }
            , selectInput id
            )

        NoOp ->
            ( Model model, Cmd.none )

        BlurInput ->
            ( Model
                { model
                    | time =
                        case model.time of
                            Ready ->
                                Ready

                            Selected t ->
                                Inputting t

                            Inputting t ->
                                Inputting t
                }
            , Cmd.none
            )


view : Model -> Html Msg
view (Model model) =
    let
        minute =
            model.time |> inputStateToMaybe |> Maybe.map (\time -> time // 100)

        second =
            model.time
                |> inputStateToMaybe
                |> Maybe.andThen
                    (\time ->
                        minute
                            |> Maybe.map
                                (\m ->
                                    time - m * 100
                                )
                    )

        format ms =
            String.padLeft 2 '0' <|
                case ms of
                    Just v ->
                        String.fromInt v

                    Nothing ->
                        "0"

        minSecInputId =
            "minSecInput" ++ String.fromInt model.key
    in
    div [ class "grid items-center bg-gray-50 font-main" ]
        [ input
            [ id minSecInputId
            , class "opacity-0 w-0 h-0 p-0"
            , type_ "text"
            , value <| Maybe.withDefault "" (Maybe.map String.fromInt <| inputStateToMaybe model.time)
            , onInput UpdateTime
            , onBlur BlurInput
            , preventDefaultOn "compositionstart" (Json.Decode.succeed ( NoOp, True ))
            ]
            []
        , div
            (List.filterMap identity
                [ Just <| class "text-right rounded-md pl-4 pr-3 py-2 grid grid-cols-[auto,auto,auto,auto] place-content-end items-baseline gap-1"
                , case model.time of
                    Selected _ ->
                        Just <| class "bg-gray-100"

                    Inputting _ ->
                        Nothing

                    Ready ->
                        Nothing
                , Just <| onClick (SelectInput minSecInputId)
                ]
            )
            [ div [ class "text-xl font-bold text-black" ] [ text (format minute) ]
            , div [ class "text-sm text-gray-300" ] [ text "m" ]
            , div [ class "text-xl font-bold text-black" ] [ text (format second) ]
            , div [ class "text-sm text-gray-300" ] [ text "s" ]
            ]
        ]


port selectInput : String -> Cmd msg
