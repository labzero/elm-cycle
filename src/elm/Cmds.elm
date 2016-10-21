module Cmds exposing (..)

import Types exposing (..)
import Msg exposing (Msg(..))
import Decoders exposing (..)
import Task exposing (Task)
import Http
import Process
import Geocoding as G
import Geolocation
import Basics.Extra as B


-- Cmds


googleKey : String
googleKey =
    "AIzaSyAji7Gm0r66d9QfW1aPYGSocFlawSXNLMw"


geocodeLocation : String -> Cmd Msg
geocodeLocation str =
    G.requestForAddress googleKey str
        |> G.send
        |> Task.perform GeocodingError GeocodingSuccess


getCurrentLocation : Cmd Msg
getCurrentLocation =
    let
        toCoordinates loc =
            Coordinates loc.latitude loc.longitude
    in
        Task.map toCoordinates Geolocation.now
            |> Task.perform GeolocationError GeolocationSuccess


getNetworks : Cmd Msg
getNetworks =
    let
        task =
            Http.get networksDecoder networksUrl
    in
        task |> Task.perform LoadNetworksError LoadNetworksSuccess


getStations : (List Station -> Msg) -> Network -> Cmd Msg
getStations successMsg network =
    let
        task =
            Http.get stationsDecoder <| stationsUrl network
    in
        task |> Task.perform LoadStationsError successMsg



-- HTTP


apiUrl : String
apiUrl =
    "https://api.citybik.es"


buildUrl : String -> String
buildUrl =
    (++) apiUrl


networksUrl : String
networksUrl =
    buildUrl "/v2/networks"


stationsUrl : Network -> String
stationsUrl =
    buildUrl << .href


clearUpdatedStations : Cmd Msg
clearUpdatedStations =
    Process.sleep 2000 |> Task.perform B.never (\_ -> ClearUpdatedStations)
