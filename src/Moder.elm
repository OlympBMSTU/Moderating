module Moder exposing (Flags, Model, Msg(..), init, main, reactor, subscriptions, update, view)

import API
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import List
import List.Extra as List
import RemoteData exposing (RemoteData(..), WebData)
import View.Exercise as Exercise


type alias Flags =
    { token : String }


defaultFlags =
    { token = "1234" }


reactor : Program () Model Msg
reactor =
    Browser.document
        { init = \_ -> init defaultFlags
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { token : String
    , exercise : Exercise.Model
    , navbarState : Navbar.State
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        token =
            flags.token

        ( exerciseModel, exerciseCmd ) =
            Exercise.init token

        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( Model token exerciseModel navbarState, Cmd.batch [ Cmd.map ExerciseMsg exerciseCmd, navbarCmd ] )


type Msg
    = ExerciseMsg Exercise.Msg
    | NavbarMsg Navbar.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExerciseMsg vmsg ->
            let
                ( updatedExercise, cmd ) =
                    Exercise.update vmsg model.exercise
            in
            ( { model | exercise = updatedExercise }, Cmd.map ExerciseMsg cmd )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navbarState NavbarMsg
        , Sub.map ExerciseMsg (Exercise.subscriptions model.exercise)
        ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    Grid.container []
        [ Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.brand [ href "#" ]
                [ text " Модерирование"
                ]
            |> Navbar.items
                [ Navbar.itemLink [href "https://olymp.bmstu.ru/exercises/login/"] [ text "Личный кабинет"]
                , Navbar.itemLink [href "https://olymp.bmstu.ru/exercises/upload/"] [ text "Загрузка задач"]
                ]
            |> Navbar.view model.navbarState
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Модерирование"
    , body =
        [ viewNavbar model
        , Grid.containerFluid []
            [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
            , Html.map ExerciseMsg <| Exercise.view model.exercise
            ]
        ]
    }
