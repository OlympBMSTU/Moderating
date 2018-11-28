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
import Http
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
    , level : Maybe Int
    , s : String
    , g : String
    , modalMessage : String
    }


init : API.Token -> ( Model, Cmd Msg )
init token =
    ( Model token NotAsked NotAsked NotAsked Modal.hidden "" Nothing "" "" "", getSubjects token )


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
    API.getSubject subject grade token
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
                     ( {model | subjects = subjects, exercise = NotAsked, s = subject }, getSubject model.g subject model.token )
                _ -> ( {model | subjects = subjects, exercise = NotAsked}, Cmd.none )
        GetSubject resp ->
            logResponce resp <| ( {model | subject = mapResponce resp}, Cmd.none )
        GetExercise resp ->
            let
                exercise = mapResponce resp
                cleanedExercise = RemoteData.map cleanTags exercise
                level =
                    case exercise of
                        Success ex ->
                            if find level1_2018 ex.tags
                            then Just 1
                            else if find level2_2018 ex.tags
                            then Just 2
                            else if find level3_2018 ex.tags
                            then Just 3
                            else Nothing
                        _ -> Nothing
                tagString =
                    case cleanedExercise of
                        Success ex -> String.join ", "  ex.tags
                        _ -> ""
            in
            logResponce resp <| ( {model | exercise = cleanedExercise, level = level, tagString = tagString }, Cmd.none )
        SubjectSelect subject ->
            ( {model | exercise = NotAsked, s = subject}, getSubject model.g subject model.token )
        GradeSelect grade ->
            let
                temp = if grade == "Все задачи" then "" else grade
            in
            ( {model | exercise = NotAsked, g = temp}, getSubject temp model.s model.token )
        ExerciseSelect id ->
            ( model, getExercise id model.token )
        Level l ->
            case model.exercise of
                Success exercise ->
                    ( {model | level = Just l }, Cmd.none)
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
                    let
                        tags = List.map String.trim <| String.split "," model.tagString
                        cleanedTags = List.filter (\x -> x /= "") <| cleanTags_ tags
                        tag = case model.level of
                            Just l  -> [levelSelector l]
                            Nothing -> []
                        taggedExercise = {exercise | tags = tags ++ tag}
                    in
                    ( model, postExercise taggedExercise model.token )
                _ ->
                    ( model, Cmd.none )
        InputTags stags ->
            ( {model | tagString = stags }, Cmd.none)
        PostExercise (Success _) ->
            ( { model | modalMessage = "Ok", modalVisibility = Modal.shown}, getSubject model.g model.s model.token)
        CloseModal ->
            ( {model | modalVisibility = Modal.hidden}, Cmd.none)
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
            |> Modal.h3 [] [ text "Сообщение" ]
            |> Modal.body [] [ p [] [ text model.modalMessage ] ]
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
    div [style "height" "600px", style "overflow" "hidden", style "overflow-y" "scroll"]
    [ ListGroup.custom
        <| List.map mkListItem list ]

viewWExercise : Model -> Html.Html Msg
viewWExercise model =
    case model.exercise of
        NotAsked ->
            text "Выберите задание"

        Loading ->
            text "Loading."

        Failure (Http.BadStatus resp) ->
            if resp.status.code == 403
            then
                text "У вас нет доступа к этим задачам"
            else
                text "error."

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
                [ viewTags model.tagString ]
            ]
        , Grid.row
            [ ]
            [ Grid.col [ Col.md12 ]
                [ Button.button [Button.onClick UpdateExercise] [ text "Отправить"] ]
            ]
        ]

viewControls : Model -> Exercise -> Html.Html Msg
viewControls model ex =
    Form.form []
        [ Form.group []
            [ Form.label [] [ text "Уровень сложности" ]
            , Fieldset.config
                |> Fieldset.children
                    ( Radio.radioList "customradiogroup"
                        [ Radio.createCustom [ Radio.onClick (Level 1), Radio.id "rdi1", Radio.checked (model.level == Just 1) ] level1_2018
                        , Radio.createCustom [ Radio.onClick (Level 2), Radio.id "rdi2", Radio.checked (model.level == Just 2) ] level2_2018
                        , Radio.createCustom [ Radio.onClick (Level 3), Radio.id "rdi3", Radio.checked (model.level == Just 3) ] level3_2018
                        ]
                    )
                |> Fieldset.view
            ]
        , Form.group []
            [ Checkbox.custom [ Checkbox.onCheck Reject, Checkbox.id "block", Checkbox.checked ex.rejected ] "Заблокировать"
            ]
        , Form.group []
            [ Form.label [] [ text "Тип задачи" ]
            , Input.number [ Input.onInput NumberInVariant, Input.attrs [Html.Attributes.min "1", Html.Attributes.max "100"], Input.value <| String.fromInt ex.level ]
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

viewTags : String -> Html.Html Msg
viewTags tags =
    Form.form []
        [ Form.group []
            [ Form.label [] [ text "Тэги" ]
            , Input.text [Input.onInput InputTags, Input.value tags]
            , Form.help [] [ text "Перечислите тэги через запятую" ]
            ]
        ]



find x lst = List.filter (\y -> y == x) lst /= []


