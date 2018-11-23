module Data.Responce exposing (..)

import Json.Decode as D
import RemoteData exposing (RemoteData(..), WebData)
import Http exposing (Error(..))

type Responce a = Responce (Maybe a) String String

decodeResponce : D.Decoder a -> D.Decoder (Responce a)
decodeResponce decoder =
    D.map3 Responce
        (D.field "Data" (D.maybe decoder))
        (D.field "Message" D.string)
        (D.field "Status" D.string)

mapResponce : WebData (Responce a) -> WebData a
mapResponce resp =
    let
        f (Responce x _ _) = x
        mdata = RemoteData.map f resp
        data =
            case mdata of
                Success (Just x) -> Success x
                Success Nothing -> Failure (Http.NetworkError)
                NotAsked -> NotAsked
                Failure e -> Failure e
                Loading -> Loading
    in data


logResponce :  WebData (Responce a) -> b -> b
logResponce resp x =
    case resp of
        Success (Responce _ msg status) -> Debug.log msg x
        Failure err -> Debug.log "elm error" x
        _ -> x
