module Main exposing (main)

import AddBreathingTechnique
import Breathing exposing (init)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Color
import Expression exposing (BooleanExpr(..), Breathing(..), ComparisonOperator(..), Duration(..), Exhale(..), Hold(..), Inhale(..), Number(..), NumberExpr(..), TimeUnit(..))
import Home
import Html exposing (b, text)
import MinSecInput
import NotFound
import Routes
import Statistics
import Storage
import Tab exposing (tabView)
import Task
import Url


flip f a b =
    f b a


backgroundColor : Color.Color
backgroundColor =
    Color.rgb255 172 228 248


foregroundColor : Color.Color
foregroundColor =
    Color.rgb255 40 23 90


globalOptions =
    { foregroundColor = foregroundColor
    , backgroundColor = backgroundColor
    }


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | HomeMsg Home.Msg
    | StorageAction Storage.StorageAction
    | MinSecInputMsg MinSecInput.Msg
    | AddBreathingTechniqueMsg AddBreathingTechnique.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        storageActionToCmd =
            StorageAction >> Task.succeed >> Task.perform identity
    in
    case msg of
        UrlChanged url ->
            ( { model | route = Routes.fromUrl url }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        HomeMsg homeMsg ->
            let
                ( homeModel, homeCmd, storageActions ) =
                    Home.update model.key homeMsg model.homeModel
            in
            ( { model | homeModel = homeModel }
            , Cmd.batch
                [ Cmd.map HomeMsg homeCmd
                , storageActions
                    |> List.map storageActionToCmd
                    |> Cmd.batch
                ]
            )

        StorageAction storageAction ->
            ( { model | storage = Storage.update storageAction model.storage }
            , Cmd.none
            )

        MinSecInputMsg minSecInputMsg ->
            let
                ( minSecInputModel, minSecInputCmd ) =
                    MinSecInput.update minSecInputMsg model.minSecInputModel
            in
            ( { model | minSecInputModel = minSecInputModel }
            , Cmd.map MinSecInputMsg minSecInputCmd
            )

        AddBreathingTechniqueMsg addBreathingTechniqueMsg ->
            let
                ( addBreathingTechniqueModel, actions, cmd ) =
                    AddBreathingTechnique.update model.key addBreathingTechniqueMsg model.addBreathingTechniqueModel
            in
            ( { model | addBreathingTechniqueModel = addBreathingTechniqueModel }
            , actions
                |> List.map storageActionToCmd
                |> (\cs -> cs ++ [ Cmd.map AddBreathingTechniqueMsg cmd ])
                |> Cmd.batch
            )


type alias Model =
    { route : Maybe Routes.Route
    , key : Nav.Key
    , homeModel : Home.Model
    , storage : Storage.Model
    , minSecInputModel : MinSecInput.Model
    , addBreathingTechniqueModel : AddBreathingTechnique.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { route = Routes.fromUrl url
      , key = key
      , homeModel = Home.init () |> Tuple.first
      , storage = Storage.init
      , minSecInputModel = MinSecInput.init 0
      , addBreathingTechniqueModel = AddBreathingTechnique.init
      }
    , Cmd.batch []
    )


view : Model -> Document Msg
view model =
    case model.route of
        Nothing ->
            NotFound.document

        Just Routes.HomeRoute ->
            { title = "Home"
            , body =
                [ Home.view globalOptions model.storage model.homeModel |> Html.map HomeMsg
                ]
            }

        Just Routes.StatisticsRoute ->
            { title = "statistics"
            , body =
                [ Statistics.view model.storage
                , tabView
                ]
            }

        Just Routes.AddBreathingTechniqueRoute ->
            { title = "add breathing technique"
            , body =
                [ AddBreathingTechnique.view model.addBreathingTechniqueModel |> Html.map AddBreathingTechniqueMsg ]
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Home.subscriptions model.homeModel |> Sub.map HomeMsg
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
