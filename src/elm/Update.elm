module Update exposing (update)

import Types exposing (..)
import Msg exposing (..)
import Model exposing (Model)
import Ports exposing (..)
import Cmds exposing (..)
import MapHelpers exposing (..)
import Maybe.Extra as Maybe
import Dict


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLocationField str ->
            { model | locationField = str } ! []

        SubmitLocation ->
            { model | locationField = "" } ! [ geocodeLocation model.locationField ]

        GeocodingError err ->
            { model | errorMessage = Just <| toString err } ! []

        GeocodingSuccess data ->
            { model | geocodingData = mapSpecForResponse data } ! [ getNetworks ]

        LoadNetworksError err ->
            { model | errorMessage = Just <| toString err } ! []

        LoadNetworksSuccess networks ->
            let
                nearestNetwork =
                    Maybe.map (findNearestNetwork networks << .center) model.geocodingData |> Maybe.join

                maybeLoadStations =
                    Maybe.map (getStations LoadStationsSuccess) nearestNetwork |> Maybe.withDefault Cmd.none
            in
                { model | bikeNetworks = Just networks, nearestNetwork = nearestNetwork } ! [ maybeLoadStations ]

        LoadStationsError err ->
            { model | errorMessage = Just <| toString err } ! []

        LoadStationsSuccess stations ->
            let
                markers =
                    List.map markerSpecForStation stations

                bounds =
                    Maybe.map .bounds model.geocodingData |> calculateMapBounds stations

                mapSpec =
                    MapSpec (boundsCenter bounds) bounds markers

                stationsWithDistance =
                    List.map (withDistanceFrom mapSpec.center) stations
            in
                { model | stations = sortByDistance stationsWithDistance, mapSpec = Just mapSpec } ! [ createMapForLocation <| Just mapSpec ]

        UseCurrentLocation ->
            model ! [ getCurrentLocation ]

        GeolocationError err ->
            { model | errorMessage = Just <| toString err } ! []

        GeolocationSuccess coords ->
            { model | geocodingData = Just <| mapSpecForCurrentLocation coords } ! [ getNetworks ]

        UpdateStations network ->
            model ! [ getStations UpdateStationsSuccess network ]

        UpdateStationsSuccess stations ->
            let
                bounds =
                    Maybe.map .bounds model.geocodingData |> calculateMapBounds stations

                stationsWithDistance =
                    List.map (withDistanceFrom <| boundsCenter bounds) stations
            in
                { model | stations = sortByDistance stationsWithDistance, updatedStations = model.updatedStations ++ updatedStations model.stations stations } ! [ clearUpdatedStations ]

        ClearUpdatedStations ->
            { model | updatedStations = [] } ! []

        StationClicked station ->
            { model | selectedStation = Just station } ! [ zoomMap station.coordinates ]


updatedStations : List Station -> List Station -> List String
updatedStations before after =
    let
        beforeMap =
            (List.map (\x -> ( x.id, x )) before) |> Dict.fromList

        changed station =
            case Dict.get station.id beforeMap of
                Just s ->
                    s.freeBikes /= station.freeBikes || s.emptySlots /= station.emptySlots

                Nothing ->
                    True
    in
        List.map .id (List.filter changed after)
