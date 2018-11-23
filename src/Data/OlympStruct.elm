module Data.OlympStruct exposing (..)

import Json.Decode as D
import Json.Encode as E
import Regex

type alias OlympStruct =
    { exs : List Int
    , perExs : List Int
    }

decodeOlympStruct : D.Decoder OlympStruct
decodeOlympStruct =
    D.map2 OlympStruct
        -- (D.field "grade" D.string)
        -- (D.field "subject" D.string)
        (D.field "exs" (D.list D.int))
        (D.field "per_exs" (D.list D.int))

encodeOlympStruct : OlympStruct -> E.Value
encodeOlympStruct ex =
    E.object
        [ ( "exs", (E.list E.int) ex.exs )
        , ( "per_exs", (E.list E.int) ex.perExs )
        -- , ( "grade", E.string ex.grade )
        -- , ( "subject", E.string ex.subject )
        ]
