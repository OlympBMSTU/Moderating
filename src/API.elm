module API exposing (..)

import Data.Exercise exposing (..)
import Data.ExerciseFromSubject exposing (..)
import Data.Responce exposing (..)
import Data.OlympStruct exposing (..)
import Http
import Json.Encode as E
import Json.Decode as D


type alias Token =
    String

encodeToken : Token -> E.Value
encodeToken =
    E.string


getSubjects : Token -> Http.Request (Responce (List String))
getSubjects token =
    Http.request
        { method = "GET"
        , headers = mkTokenHeader token
        , url = "https://olymp.bmstu.ru/api/exercises/subjects/"
        , body = Http.emptyBody
        , expect = Http.expectJson (decodeResponce (D.list (D.string)))
        , timeout = Nothing
        , withCredentials = False
        }

getSubject : String -> Token -> Http.Request (Responce (List ExerciseFromSubject))
getSubject subject token =
    Http.request
        { method = "GET"
        , headers = mkTokenHeader token
        , url = "https://olymp.bmstu.ru/api/exercises/list/" ++ subject
        , body = Http.emptyBody
        , expect = Http.expectJson (decodeResponce (D.list decodeExerciseFromSubject))
        , timeout = Nothing
        , withCredentials = False
        }

getExercise : Int -> Token -> Http.Request (Responce Exercise)
getExercise id token =
    Http.request
        { method = "GET"
        , headers = mkTokenHeader token
        , url = "https://olymp.bmstu.ru/api/exercises/get/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectJson (decodeResponce decodeExercise)
        , timeout = Nothing
        , withCredentials = False
        }

postExercise : Exercise -> Token -> Http.Request ()
postExercise ex token =
    Http.request
        { method = "POST"
        , headers = mkTokenHeader token
        , url = "https://olymp.bmstu.ru/api/exercises/get/" ++ String.fromInt ex.id
        , body = Http.jsonBody (encodeExercise ex)
        , expect = Http.expectStringResponse << always <| Ok ()
        , timeout = Nothing
        , withCredentials = False
        }

getOlympStruct : String -> String -> Token -> Http.Request (Responce OlympStruct)
getOlympStruct subject grade token =
    Http.request
        { method = "GET"
        , headers = mkTokenHeader token
        , url = "https://olymp.bmstu.ru/api/exercises/olymp_conf/" ++ grade
        , body = Http.emptyBody
        , expect = Http.expectJson (decodeResponce decodeOlympStruct)
        , timeout = Nothing
        , withCredentials = False
        }

postOlympStruct : OlympStruct -> String -> String -> Token -> Http.Request ()
postOlympStruct os subject grade token =
    Http.request
        { method = "POST"
        , headers = mkTokenHeader token
        , url = "https://olymp.bmstu.ru/api/exercises/olymp_conf/" ++ grade
        , body = Http.jsonBody (encodeOlympStruct os)
        , expect = Http.expectStringResponse << always <| Ok ()
        , timeout = Nothing
        , withCredentials = False
        }

mkTokenHeader token =
    []
