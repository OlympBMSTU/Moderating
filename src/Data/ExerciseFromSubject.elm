module Data.ExerciseFromSubject exposing (..)

import Json.Decode as D
import Data.Level exposing (..)
import Regex

type alias ExerciseFromSubject =
    { id : Int
    , authorId : Int
    , filename : String
    , tags : List String
    , level : Int
    , subject : String
    , isBroken : Bool
    }

decodeExerciseFromSubject : D.Decoder ExerciseFromSubject
decodeExerciseFromSubject =
    D.map7 ExerciseFromSubject
        (D.field "Id" D.int)
        (D.field "AuthorId" D.int)
        (D.field "FileName" D.string)
        (D.field "Tags" (D.list D.string))
        (D.field "Level" D.int)
        (D.field "Subject" D.string)
        (D.field "IsBroken" D.bool)


hasLevel ex = List.any (Regex.contains levelRegex << String.trim) ex.tags
blocked ex = ex.isBroken
