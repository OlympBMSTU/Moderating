module Data.Exercise exposing (..)

import Json.Decode as D
import Json.Encode as E
import Regex

type alias Exercise =
    { id : Int
    , authorId : Int
    , filename : String
    , tags : List String
    , level : Int
    , subject : String
    , rejected : Bool
    }

decodeExercise : D.Decoder Exercise
decodeExercise =
    D.map7 Exercise
        (D.field "id" D.int)
        (D.field "author" D.int)
        (D.field "file_name" D.string)
        (D.field "tags" (D.list D.string))
        (D.field "level" D.int)
        (D.field "subject" D.string)
        (D.field "rejected" D.bool)

encodeExercise : Exercise -> E.Value
encodeExercise ex =
    E.object
        [ ( "id", E.int ex.id )
        , ( "author", E.int ex.authorId )
        , ( "file_name", E.string ex.filename )
        , ( "tags", (E.list E.string) ex.tags )
        , ( "level", E.int ex.level )
        , ( "subject", E.string ex.subject )
        , ( "rejected", E.bool ex.rejected )
        ]

levelRegex : Regex.Regex
levelRegex =
    Maybe.withDefault Regex.never <| Regex.fromString "[123] уровень сложности 2018 года?$"

cleanTags ex = {ex | tags = List.filter cleanTag ex.tags}

cleanTag x = not <| Regex.contains levelRegex <| String.trim x
