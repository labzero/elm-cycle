module Decoders exposing (..)

import Types exposing (..)
import Json.Decode as Decode exposing (Decoder, list, int, string, float, (:=))
import Json.Decode.Pipeline as Decode exposing (decode, required, custom, requiredAt)
import Date
import Time exposing (Time)


networksDecoder : Decoder (List Network)
networksDecoder =
    "networks"
        := (Decode.list networkDecoder)


networkDecoder : Decoder Network
networkDecoder =
    decode Network
        |> Decode.required "id" string
        |> Decode.required "name" string
        |> Decode.required "href" string
        |> Decode.required "location" (networkLocationDecoder)
        |> Decode.required "company" companyDecoder


networkLocationDecoder : Decoder NetworkLocation
networkLocationDecoder =
    decode NetworkLocation
        |> Decode.required "city" string
        |> Decode.required "country" string
        |> Decode.custom
            (decode Coordinates
                |> Decode.required "latitude" float
                |> Decode.required "longitude" float
            )


companyDecoder : Decoder Company
companyDecoder =
    Decode.oneOf
        [ Decode.map CompanyName string
        , Decode.map MultipleCompanyNames (Decode.list string)
        , Decode.null NoCompany
        ]


stationsDecoder : Decoder (List Station)
stationsDecoder =
    Decode.at [ "network", "stations" ] (Decode.list stationDecoder)


stationDecoder : Decoder Station
stationDecoder =
    decode Station
        |> Decode.required "id" string
        |> Decode.required "name" string
        |> Decode.custom
            (decode Coordinates
                |> Decode.required "latitude" float
                |> Decode.required "longitude" float
            )
        |> Decode.required "empty_slots" int
        |> Decode.required "free_bikes" int
        |> Decode.required "timestamp"
            (Decode.map (jsDateToTime 0) string)
        |> Decode.hardcoded (Just 0.0)



-- Helpers


jsDateToTime : Time -> String -> Time
jsDateToTime default =
    Date.fromString
        >> Result.map Date.toTime
        >> Result.toMaybe
        >> Maybe.withDefault default
