module View.Exercise exposing (..)

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
import Data.Level exposing (..)

import Debug

type alias Model =
    { token : API.Token
    , subjects : WebData (List String)
    , subject : WebData (List ExerciseFromSubject)
    , exercise : WebData (Exercise)
    , modalVisibility : Modal.Visibility
    , tagString : String
    }


init : API.Token -> ( Model, Cmd Msg )
init token =
    ( Model token NotAsked NotAsked NotAsked Modal.hidden "", getSubjects token )


type Msg
    = GetSubjects (WebData (Responce (List String)))
    | GetSubject (WebData (Responce (List ExerciseFromSubject)))
    | GetExercise (WebData (Responce Exercise))
    | PostExercise (WebData ())
    | SubjectSelect String
    | GradeSelect String
    | ExerciseSelect Int
    | Level Int
    | NumberInVariant String
    | InputTags String
    | Reject Bool
    | UpdateExercise
    | CloseModal


getSubjects : API.Token -> Cmd Msg
getSubjects token =
    API.getSubjects token
        |> RemoteData.sendRequest
        |> Cmd.map GetSubjects

getSubject : String -> String -> API.Token -> Cmd Msg
getSubject grade subject token =
    API.getSubject subject token
        |> RemoteData.sendRequest
        |> Cmd.map GetSubject

getExercise : Int -> API.Token -> Cmd Msg
getExercise id token =
    API.getExercise id token
        |> RemoteData.sendRequest
        |> Cmd.map GetExercise

postExercise : Exercise -> API.Token -> Cmd Msg
postExercise ex token =
    API.postExercise ex token
        |> RemoteData.sendRequest
        |> Cmd.map PostExercise

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Debug.log "update" <| case msg of
        GetSubjects resp ->
            let
                subjects = mapResponce resp
            in
            logResponce resp <| case resp of
                Success (Responce (Just data) _ _) ->
                    let
                        msubject = List.getAt 0 data
                        subject = Maybe.withDefault "" msubject
                    in
                     ( {model | subjects = subjects, exercise = NotAsked }, getSubject "" subject model.token )
                _ -> ( {model | subjects = subjects, exercise = NotAsked }, Cmd.none )
        GetSubject resp ->
            logResponce resp <| ( {model | subject = mapResponce resp}, Cmd.none )
        GetExercise resp ->
            let
                exercise = mapResponce resp
            in
            logResponce resp <| ( {model | exercise = exercise }, Cmd.none )
        SubjectSelect subject ->
            ( {model | exercise = NotAsked}, getSubject "" subject model.token )
        ExerciseSelect id ->
            ( model, getExercise id model.token )
        Level l ->
            case model.exercise of
                Success exercise ->
                    let
                        cleanedExercise = cleanTags exercise
                        temp = if List.last cleanedExercise.tags == Just ""
                            then Maybe.withDefault [] <| List.init cleanedExercise.tags
                            else cleanedExercise.tags
                        tag = String.fromInt l ++ " уровень сложности 2018 года"
                        taggedExercise = {cleanedExercise | tags = temp ++ [tag]}
                    in
                    ( {model | exercise = Success taggedExercise }, Cmd.none)
                _ ->
                    ( model, Cmd.none )
        NumberInVariant sv ->
            case model.exercise of
                Success exercise ->
                    let
                        v = Maybe.withDefault 0 (String.toInt sv)
                        updated = { exercise | level = v }
                    in
                    ( {model | exercise = Success updated }, Cmd.none)
                _ ->
                    ( model, Cmd.none )
        Reject r ->
            case model.exercise of
                Success exercise ->
                    let
                        updated = { exercise | rejected = r }
                    in
                    ( {model | exercise = Success updated }, Cmd.none)
                _ ->
                    ( model, Cmd.none )
        UpdateExercise ->
            case model.exercise of
                Success exercise ->
                    ( model, postExercise exercise model.token )
                _ ->
                    ( model, Cmd.none )
        InputTags stags ->
            case model.exercise of
                Success exercise ->
                    let
                        temp = if List.last exercise.tags == Just "" && String.endsWith "," stags
                            then String.dropRight 1 stags
                            else stags
                        tags = String.split "," temp
                        updated = { exercise | tags = tags }
                    in
                    ( {model | exercise = Success updated }, Cmd.none)
                _ ->
                    ( model, Cmd.none )
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
                , viewExerciseList model
                ]
            , Grid.col [ Col.md10, Col.attrs [Spacing.mt2] ]
                [ viewWExercise model
                ]
            ]
        , Modal.config CloseModal
            |> Modal.small
            |> Modal.hideOnBackdropClick True
            |> Modal.h3 [] [ text "Ошибка" ]
            |> Modal.body [] [ p [] [ text "Некорректный формат некоторых полей" ] ]
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
    in
    Form.form []
        [ Form.group []
            [ Select.select [ Select.onChange SubjectSelect ]
                <| List.map mkSelect list
            ]
        ]

viewGradeSelect : Model -> Html.Html Msg
viewGradeSelect model =
    let
        list =
            case model.subject of
                Success x -> x
                _ -> []
        mkSelect i = Select.item [ ] [ text <| String.fromInt i ++" класс" ]
        allSelect = [Select.item [ ] [ text <| "Все задачи" ]]
    in
    Form.form []
        [ Form.group []
            [ Select.select [ Select.onChange GradeSelect ]
                <| allSelect ++ (List.map mkSelect <| List.range 5 11)
            ]
        ]

viewExerciseList : Model -> Html.Html Msg
viewExerciseList model =
    let
        list =
            case model.subject of
                Success x -> x
                _ -> []
        currentExId =
            case model.exercise of
                Success ex -> Just ex.id
                _ -> Nothing
        mkListItem ex =
            let
                attrs =
                    if Just ex.id == currentExId
                        then [ ListGroup.info ]
                        else [ ListGroup.attrs [ onClick (ExerciseSelect ex.id)] ]
                    ++
                    if blocked ex
                        then [ListGroup.danger]
                        else if hasLevel ex
                            then [ListGroup.success]
                            else []

            in
            ListGroup.anchor attrs [ text <| "#" ++ String.fromInt ex.id ]
    in
    ListGroup.custom
        <| List.map mkListItem list

viewWExercise : Model -> Html.Html Msg
viewWExercise model =
    case model.exercise of
        NotAsked ->
            text "Выберите задание"

        Loading ->
            text "Loading."

        Failure err ->
            text "Error."

        Success exercise ->
            viewExercise model exercise


viewExercise : Model -> Exercise -> Html.Html Msg
viewExercise model exercise =
    Grid.container
        []
        [ Grid.row
            [ ]
            [ Grid.col [ Col.md3 ]
                [ viewControls model exercise ]
            , Grid.col [ Col.md9 ]
                [ viewPdf exercise.filename ]
            ]
        , Grid.row
            [ ]
            [ Grid.col [ Col.md12 ]
                [ viewTags exercise.tags ]
            ]
        , Grid.row
            [ ]
            [ Grid.col [ Col.md12 ]
                [ Button.button [Button.onClick UpdateExercise] [ text "Отправить"] ]
            ]
        ]

viewControls : Model -> Exercise -> Html.Html Msg
viewControls model ex =
    let
        find x lst = List.filter (\y -> y == x) lst /= []
    in
    Form.form []
        [ Form.group []
            [ Form.label [] [ text "Уровень сложности" ]
            , Fieldset.config
                |> Fieldset.children
                    ( Radio.radioList "customradiogroup"
                        [ Radio.createCustom [ Radio.onClick (Level 1), Radio.id "rdi1", Radio.checked (find level1_2018 ex.tags) ] level1_2018
                        , Radio.createCustom [ Radio.onClick (Level 2), Radio.id "rdi2", Radio.checked (find level2_2018 ex.tags) ] level2_2018
                        , Radio.createCustom [ Radio.onClick (Level 3), Radio.id "rdi3", Radio.checked (find level3_2018 ex.tags) ] level3_2018
                        ]
                    )
                |> Fieldset.view
            ]
        , Form.group []
            [ Checkbox.custom [ Checkbox.onCheck Reject, Checkbox.id "block", Checkbox.checked ex.rejected ] "Заблокировать"
            ]
        , Form.group []
            [ Form.label [] [ text "№ в билете" ]
            , Input.number [ Input.onInput NumberInVariant, Input.value <| String.fromInt ex.level ]
            ]
        ]

viewPdf : String -> Html.Html Msg
viewPdf path =
    let
        url = "https://olymp.bmstu.ru/exercises/files/" ++ path
    in
    iframe
        [ style "height" "400px"
        , style "width" "100%"
        , src url
        ]
        []

viewTags : List String -> Html.Html Msg
viewTags tags =
    let
        cleaned = cleanTags_ tags
    in
    Form.form []
        [ Form.group []
            [ Form.label [] [ text "Тэги" ]
            , Input.text [Input.onInput InputTags, Input.value <| String.join "," <| List.map (\x -> if String.startsWith " " x then x else " " ++ x) cleaned]
            , Form.help [] [ text "Перечислите тэги через запятую" ]
            ]
        ]
