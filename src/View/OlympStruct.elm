module View.OlympStruct exposing (..)

import API
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Spacing as Spacing
import Data.ExerciseFromSubject exposing (..)
import Data.Exercise exposing (..)
import Data.Responce exposing (..)
import Bootstrap.Form as Form
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Table as Table
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import List.Extra as List
import Maybe
import Regex
import RemoteData exposing (RemoteData(..), WebData)
import String
import Task
import Time
import Data.OlympStruct exposing (..)

import Debug

type alias Model =
    { token : API.Token
    , subjects : WebData (List String)
    , struct : WebData OlympStruct
    , modalVisibility : Modal.Visibility
    , subject : String
    , grade : String
    }


init : API.Token -> ( Model, Cmd Msg )
init token =
    ( Model token NotAsked NotAsked Modal.hidden "" "", getSubjects token )


type Msg
    = GetSubjects (WebData (Responce (List String)))
    | GetStruct (WebData (Responce OlympStruct))
    | PostStruct (WebData ())
    | UpdateStruct
    | SubjectSelect String
    | GradeSelect String
    | InputCount Int String
    | InputPoints Int String
    | CloseModal

getSubjects : API.Token -> Cmd Msg
getSubjects token =
    API.getSubjects token
        |> RemoteData.sendRequest
        |> Cmd.map GetSubjects

getOlympStruct : String -> String -> API.Token -> Cmd Msg
getOlympStruct subject grade token =
    API.getOlympStruct subject grade token
        |> RemoteData.sendRequest
        |> Cmd.map GetStruct

postOlympStruct : OlympStruct -> String -> String -> API.Token -> Cmd Msg
postOlympStruct os subject grade token =
    API.postOlympStruct os subject grade token
        |> RemoteData.sendRequest
        |> Cmd.map PostStruct

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Debug.log "update" <| case msg of
        GetSubjects resp ->
            logResponce resp <| ( {model | subjects = mapResponce resp }, Cmd.none )
        GetStruct resp ->
            logResponce resp <| ( {model | struct = mapResponce resp}, Cmd.none )

        SubjectSelect subject ->
            ( {model | struct = NotAsked, subject = subject},
                if model.grade == "" || subject == ""
                    then Cmd.none
                    else getOlympStruct subject model.grade model.token )

        GradeSelect grade ->
            ( {model | struct = NotAsked, grade = grade},
                if grade == "" || model.subject == ""
                    then Cmd.none
                    else getOlympStruct model.subject grade model.token )

        UpdateStruct ->
            case model.struct of
                Success struct ->
                    let
                        check1 = Just (List.sum (List.take 3 struct.exs)) == List.getAt 3 struct.exs
                        check20 = List.zip (List.take 3 struct.exs) (List.take 3 struct.perExs)
                        check21 = List.sum <| List.map (\(a,b) -> a*b) check20
                        check2 = Just check21 == List.getAt 3 struct.perExs
                    in
                    if not check1 || not check2
                        then ( {model|modalVisibility = Modal.shown}, Cmd.none )
                        else ( model, postOlympStruct struct model.subject model.grade model.token )
                _ ->
                    ( model, Cmd.none )

        InputPoints i sp ->
            case model.struct of
                Success struct ->
                    let
                        cp = Maybe.withDefault 0 <| List.getAt i struct.perExs
                        p = Maybe.withDefault cp <| String.toInt sp
                        updatedp = List.setAt i p struct.perExs
                        us = {struct | perExs = updatedp}
                    in
                    ( {model | struct = Success us}, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        InputCount i sp ->
            case model.struct of
                Success struct ->
                    let
                        cp = Maybe.withDefault 0 <| List.getAt i struct.exs
                        p = Maybe.withDefault cp <| String.toInt sp
                        updatedp = List.setAt i p struct.exs
                        us = {struct | exs = updatedp}
                    in
                    ( {model | struct = Success us}, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        CloseModal ->
            ( {model | modalVisibility = Modal.hidden}, Cmd.none )
        _ ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


view : Model -> Html.Html Msg
view model =
   Grid.container
        []
        [ Grid.row
            [ ]
            [ Grid.col [ Col.md2, Col.attrs [Spacing.mt2] ]
                [ viewSubjectSelect model
                , viewGradeSelect model
                ]
            , Grid.col [ Col.md10, Col.attrs [Spacing.mt2] ]
                [ viewWStruct model
                ]
            ]
        , Modal.config CloseModal
            |> Modal.small
            |> Modal.hideOnBackdropClick True
            |> Modal.h3 [] [ text "Ошибка" ]
            |> Modal.body [] [ p [] [ text "Данные некорректны" ] ]
            |> Modal.footer []
                [ Button.button
                    [ Button.outlinePrimary
                    , Button.attrs [ onClick CloseModal ]
                    ]
                    [ text "Закрыть" ]
                ]
            |> Modal.view model.modalVisibility
        ]

viewSubjectSelect : Model -> Html.Html Msg
viewSubjectSelect model =
    let
        list =
            case model.subjects of
                Success x -> x
                _ -> []
        mkSelect s = Select.item [ ] [ text s]
        allSelect = [Select.item [ ] [ text "" ]]
    in
    Form.form []
        [ Form.group []
            [ Select.select [ Select.onChange SubjectSelect ]
                <| allSelect ++ List.map mkSelect list
            ]
        ]

viewGradeSelect : Model -> Html.Html Msg
viewGradeSelect model =
    let
        mkSelect i = Select.item [ ] [ text <| String.fromInt i ++" класс" ]
        allSelect = [Select.item [ ] [ text <| "" ]]
    in
    Form.form []
        [ Form.group []
            [ Select.select [ Select.onChange GradeSelect ]
                <| allSelect ++ (List.map mkSelect <| List.range 5 11)
            ]
        ]

viewWStruct : Model -> Html.Html Msg
viewWStruct model =
    case model.struct of
        NotAsked ->
            text "Выберите категорию"

        Loading ->
            text "Loading."

        Failure err ->
            text "Error."

        Success struct ->
            div [] [viewStruct model struct, Button.button [Button.onClick UpdateStruct] [ text "Отправить"]]


viewStruct : Model -> OlympStruct -> Html.Html Msg
viewStruct model struct =
    let
        mkCellCount i value = Table.td [] [ Input.number [Input.value <| String.fromInt value, Input.onInput (InputCount i)] ]
        mkCellPoint i value = Table.td [] [ Input.number [Input.value <| String.fromInt value, Input.onInput (InputPoints i)] ]
    in
    Table.table
      { options = [  ]
      , thead =  Table.simpleThead
          [ Table.th [] [ text "" ]
          , Table.th [] [ text "Базовая Сложность" ]
          , Table.th [] [ text "Продвинутая Сложность" ]
          , Table.th [] [ text "Сложная Сложность" ]
          , Table.th [] [ text "Сумма" ]
          ]
      , tbody =
          Table.tbody []
              [ Table.tr []
                  <| [ Table.td [ Table.cellAttr <| scope "row"] [ text "Количество заданий" ]]
                    ++ (List.indexedMap mkCellCount struct.exs)
              , Table.tr []
                  <| [ Table.td [Table.cellAttr <| scope "row"] [ text "Баллы за задания раздела" ]]
                    ++ (List.indexedMap mkCellPoint struct.perExs)
              ]
      }
