module Msg exposing (Msg(..))

import Types exposing (..)
import Geocoding as G
import Geolocation
import Http


type Msg
    = UpdateLocationField String
    | SubmitLocation
    | GeocodingError Http.Error
    | GeocodingSuccess G.Response
    | LoadNetworksError Http.Error
    | LoadNetworksSuccess (List Network)
    | LoadStationsError Http.Error
    | LoadStationsSuccess (List Station)
    | UseCurrentLocation
    | GeolocationError Geolocation.Error
    | GeolocationSuccess Coordinates
    | UpdateStations Network
    | UpdateStationsSuccess (List Station)
    | ClearUpdatedStations
    | StationClicked Station
