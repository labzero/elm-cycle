port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Http
import String
import Dict
import Time exposing (Time)
import Date exposing (Date)
import Basics as B
import Process
import Basics.Extra as B
import Geocoding as G
import Geolocation
import Task exposing (Task)
import Maybe.Extra as Maybe
import Json.Decode as Decode exposing (Decoder, list, int, string, float, (:=))
import Json.Decode.Pipeline as Decode exposing (decode, required, custom, requiredAt)
import Geodesy as Geod


-- app


main : Program Never
main =
    App.program
        { init = initialModel ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { locationField : String
    , geocodingData : Maybe MapSpec
    , mapSpec : Maybe MapSpec
    , errorMessage : Maybe String
    , bikeNetworks : Maybe (List Network)
    , nearestNetwork : Maybe Network
    , stations : List Station
    , updatedStations : List String
    }


initialModel : Model
initialModel =
    { locationField = ""
    , geocodingData = Nothing
    , mapSpec = Nothing
    , errorMessage = Nothing
    , bikeNetworks = Nothing
    , nearestNetwork = Nothing
    , stations = []
    , updatedStations = []
    }



-- UPDATE


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
            in
                { model | stations = sortByDistanceFrom mapSpec.center stations, mapSpec = Just mapSpec } ! [ createMapForLocation <| Just mapSpec ]

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
            in
                { model | stations = sortByDistanceFrom (boundsCenter bounds) stations, updatedStations = model.updatedStations ++ updatedStations model.stations stations } ! [ clearUpdatedStations ]

        ClearUpdatedStations ->
            { model | updatedStations = [] } ! []


clearUpdatedStations : Cmd Msg
clearUpdatedStations =
    Process.sleep 2000 |> Task.perform B.never (\_ -> ClearUpdatedStations)


updatedStations : List Station -> List Station -> List String
updatedStations before after =
    let
        beforeMap =
            (List.map (\x -> ( x.id, x )) before) |> Dict.fromList

        changed station =
            case Dict.get station.id beforeMap of
                Just s ->
                    s.freeBikes /= station.freeBikes || s.emptySlots /= station.emptySlots

                _ ->
                    True
    in
        List.map .id (List.filter changed after)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "body pure-g" ]
        <| [ Html.form [ class "pure-form pure-u-2-3", onSubmit SubmitLocation ]
                [ fieldset []
                    [ legend [] [ text "Find a Bike Share" ]
                    , input [ class "pure-input-1-3", type' "text", placeholder "Address, Location Name or Postal Code", value model.locationField, onInput UpdateLocationField ] []
                    , button [ class "pure-button pure-button-primary", type' "submit" ] [ text "Search" ]
                    , span [ class "pure-button", onClick UseCurrentLocation ] [ text "Use Current Location" ]
                    ]
                ]
           ]
        ++ maybeNode errorDiv model.errorMessage
        ++ [ mapDiv ]
        ++ maybeNode networkCard model.nearestNetwork
        ++ [ stationsView model.stations model.updatedStations ]


errorDiv : String -> Html Msg
errorDiv str =
    div [ class "error pure-u-2-3" ] [ text str ]


mapDiv : Html Msg
mapDiv =
    div [ class "map pure-u-1-1", id "map" ] []


networkCard : Network -> Html Msg
networkCard network =
    div [ class "pure-u-11-24 network-card" ]
        [ div [ class "network-name" ] [ text network.name ]
        , div [ class "network-company" ] [ text <| companyHelper network.company ]
        , div [ class "network-location" ] [ text <| network.location.city ++ ", " ++ network.location.country ]
        ]


companyHelper : Company -> String
companyHelper company =
    case company of
        CompanyName name ->
            name

        MultipleCompanyNames names ->
            String.join ", " names

        NoCompany ->
            ""


stationsView : List Station -> List String -> Html Msg
stationsView stations updated =
    div [ class "pure-u-1-1" ]
        <| if List.isEmpty stations then
            []
           else
            [ table [ class "pure-u-1-4 pure-table pure-table-horizontal" ]
                [ thead []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Free Bikes" ]
                    , th [] [ text "Empty Slots" ]
                    ]
                , tbody [] <| List.map (stationRow updated) stations
                ]
            ]


stationRow : List String -> Station -> Html Msg
stationRow ids station =
    let
        classes =
            if List.member station.id ids then
                "station-row, station-row-updated"
            else
                "station-row"
    in
        tr [ class classes ]
            [ td [] [ text <| station.name ]
            , td [] [ text <| toString station.freeBikes ]
            , td [] [ text <| toString station.emptySlots ]
            ]


networksView : List Network -> Html Msg
networksView networks =
    table [ class "pure-u-1-2 pure-table pure-table-horizontal" ]
        [ thead []
            [ th [] [ text "Name" ]
            , th [] [ text "City" ]
            , th [] [ text "Country" ]
            , th [] [ text "Latitude" ]
            , th [] [ text "Longitude" ]
            ]
        , tbody [] <| List.map networkRow networks
        ]


networkRow : Network -> Html Msg
networkRow network =
    tr []
        [ td [] [ text network.name ]
        , td [] [ text network.location.city ]
        , td [] [ text network.location.country ]
        , td [] [ text <| toString network.location.coordinates.lat ]
        , td [] [ text <| toString network.location.coordinates.lng ]
        ]


maybeNode : (a -> b) -> Maybe a -> List b
maybeNode f maybeVal =
    maybeVal
        |> Maybe.map f
        |> Maybe.maybeToList



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



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.nearestNetwork of
        Just network ->
            Time.every (15 * Time.second) (\_ -> UpdateStations network)

        _ ->
            Sub.none



-- Ports


createMapForLocation : Maybe MapSpec -> Cmd msg
createMapForLocation mapSpec =
    case mapSpec of
        Just spec ->
            createMap spec

        _ ->
            Cmd.none


port createMap : MapSpec -> Cmd msg



-- Location Helpers


sortByDistanceFrom : Coordinates -> List Station -> List Station
sortByDistanceFrom coords stations =
    List.sortBy (\s -> distance coords s.coordinates) stations


distance : Coordinates -> Coordinates -> Float
distance x y =
    Geod.distance ( x.lat, x.lng ) ( y.lat, y.lng ) Geod.Meters


findNearestNetwork : List Network -> Coordinates -> Maybe Network
findNearestNetwork networks coordinates =
    let
        networkDistance coords net =
            ( net, distance coords net.location.coordinates )

        distances =
            List.map (networkDistance coordinates) networks
    in
        List.sortBy snd distances |> List.head |> Maybe.map fst


mapSpecForCurrentLocation : Coordinates -> MapSpec
mapSpecForCurrentLocation coords =
    MapSpec coords (boundsForCoordinates [ coords ]) []


mapSpecForResponse : G.Response -> Maybe MapSpec
mapSpecForResponse data =
    let
        geometry =
            data.results |> List.head |> Maybe.map .geometry
    in
        case geometry of
            Just geom ->
                Just <| makeMapSpec geom.location geom.viewport

            _ ->
                Nothing


makeMapSpec : { latitude : Float, longitude : Float } -> G.Viewport -> MapSpec
makeMapSpec loc viewport =
    MapSpec (coordinates loc) (bounds viewport) []


coordinates : { latitude : Float, longitude : Float } -> Coordinates
coordinates loc =
    Coordinates loc.latitude loc.longitude


bounds : G.Viewport -> Bounds
bounds v =
    Bounds v.northeast.longitude v.northeast.latitude v.southwest.latitude v.northeast.longitude


markerSpecForStation : Station -> MarkerSpec
markerSpecForStation s =
    MarkerSpec s.coordinates s.name



-- calculate a viewport for a list of coordinates


boundsForCoordinates : List Coordinates -> Bounds
boundsForCoordinates coords =
    let
        east =
            List.map .lng coords |> List.maximum |> Maybe.withDefault 0

        north =
            List.map .lat coords |> List.maximum |> Maybe.withDefault 0

        south =
            List.map .lat coords |> List.minimum |> Maybe.withDefault 0

        west =
            List.map .lng coords |> List.minimum |> Maybe.withDefault 0
    in
        Bounds east north south west


boundsCenter : Bounds -> Coordinates
boundsCenter b =
    Coordinates (b.south + (boundsHeight b / 2)) (b.west + (boundsWidth b / 2))


boundsHeight : Bounds -> Float
boundsHeight x =
    x.north - x.south


boundsWidth : Bounds -> Float
boundsWidth x =
    x.east - x.west


boundsContains : Bounds -> Coordinates -> Bool
boundsContains b c =
    c.lng >= b.west && c.lng <= b.east && c.lat >= b.south && c.lat <= b.north



-- completely unprincipled fudge factor


minMapWidth : Float
minMapWidth =
    0.07


minMapHeight : Float
minMapHeight =
    0.03


ensureMinimumSize : Bounds -> Bounds
ensureMinimumSize b =
    let
        ( north, south ) =
            if b.north - b.south < minMapHeight then
                ( b.north + (minMapHeight / 2.0), b.south - (minMapHeight / 2.0) )
            else
                ( north, south )

        ( east, west ) =
            if b.east - b.west < minMapWidth then
                ( b.east + (minMapWidth / 2.0), b.west - (minMapWidth / 2.0) )
            else
                ( east, west )
    in
        Bounds east north south west


containsStations : Bounds -> List Station -> Bool
containsStations b =
    List.any (boundsContains b << .coordinates)



-- if a minimally sized map containing your geocoding results contains any stations
-- use that, otherwise use a viewport that contains all the stations in the network


calculateMapBounds : List Station -> Maybe Bounds -> Bounds
calculateMapBounds stations bounds =
    let
        stationBounds =
            List.map .coordinates stations |> boundsForCoordinates
    in
        case bounds of
            Just b ->
                if containsStations (ensureMinimumSize b) stations then
                    ensureMinimumSize b
                else
                    stationBounds

            _ ->
                stationBounds


type alias MapSpec =
    { center : Coordinates
    , bounds : Bounds
    , markers : List MarkerSpec
    }


type alias Coordinates =
    { lat : Float
    , lng : Float
    }


type alias MarkerSpec =
    { location : Coordinates
    , title : String
    }


type alias Bounds =
    { east : Float
    , north : Float
    , south : Float
    , west : Float
    }


type alias Network =
    { id : String
    , name : String
    , href : String
    , location : NetworkLocation
    , company : Company
    }


type Company
    = CompanyName String
    | MultipleCompanyNames (List String)
    | NoCompany


type alias NetworkLocation =
    { city : String
    , country : String
    , coordinates : Coordinates
    }


type alias Station =
    { id : String
    , name : String
    , coordinates : Coordinates
    , emptySlots : Int
    , freeBikes : Int
    , timestamp : Time
    }



-- JSON decoders


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



-- Helpers


jsDateToTime : Time -> String -> Time
jsDateToTime default =
    Date.fromString
        >> Result.map Date.toTime
        >> Result.toMaybe
        >> Maybe.withDefault default
