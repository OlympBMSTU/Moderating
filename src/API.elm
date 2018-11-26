module API exposing (..)

import Data.Exercise exposing (..)
import Data.ExerciseFromSubject exposing (..)
import Data.Responce exposing (..)
import Data.OlympStruct exposing (..)
import Http
import Html
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
        , withCredentials = True
        }

getSubject : String -> String -> Token -> Http.Request (Responce (List ExerciseFromSubject))
getSubject subject grade token =
    Http.request
        { method = "GET"
        , headers = mkTokenHeader token
        , url = "https://olymp.bmstu.ru/api/exercises/list/" ++ subject ++ "/" ++ grade
        , body = Http.emptyBody
        , expect = Http.expectJson (decodeResponce (D.list decodeExerciseFromSubject))
        , timeout = Nothing
        , withCredentials = True
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
        , withCredentials = True
        }

postExercise : Exercise -> Token -> Http.Request ()
postExercise ex token =
    Http.request
        { method = "POST"
        , headers = mkTokenHeader token
        , url = "https://olymp.bmstu.ru/api/exercises/update"
        , body = Http.multipartBody
            [   Http.stringPart "id" (String.fromInt ex.id),
                Http.stringPart "level" (String.fromInt ex.level),
                Http.stringPart "tags" (E.encode 0 (E.list E.string ex.tags)),
                Http.stringPart "is_broken" (boolToStr ex.rejected)
            ]
        , expect = Http.expectStringResponse << always <| Ok ()
        , timeout = Nothing
        , withCredentials = True
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
        , withCredentials = True
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
        , withCredentials = True
        }

mkTokenHeader token =
    []

boolToStr val =
    if val then
        "true"
    else
        "false"