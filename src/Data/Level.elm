module Data.Level exposing (..)

import Regex

levelRegex : Regex.Regex
levelRegex =
    Maybe.withDefault Regex.never <| Regex.fromString "[123] уровень сложности 2018 года?$"

level1_2018 = "1 уровень сложности 2018 года"
level2_2018 = "2 уровень сложности 2018 года"
level3_2018 = "3 уровень сложности 2018 года"
