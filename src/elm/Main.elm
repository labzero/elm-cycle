port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Http
import String
import Time exposing (Time)
import Date exposing (Date)
import Basics as B
import Geocoding as G
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
        , subscriptions = \_ -> Sub.none
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
            { model | geocodingData = mapSpecForResponse (Debug.log (toString data) data) } ! [ getNetworks ]

        LoadNetworksError err ->
            { model | errorMessage = Just <| toString err } ! []

        LoadNetworksSuccess networks ->
            let
                nearestNetwork =
                    Maybe.map (findNearestNetwork networks << .center) model.geocodingData |> Maybe.join

                maybeLoadStations =
                    Maybe.map getStations nearestNetwork |> Maybe.withDefault Cmd.none
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
                { model | stations = stations, mapSpec = Just mapSpec } ! [ createMapForLocation <| Just mapSpec ]


findNearestNetwork : List Network -> Coordinates -> Maybe Network
findNearestNetwork networks coordinates =
    let
        distance coords net =
            ( net, Geod.distance ( coords.lat, coords.lng ) ( net.location.coordinates.lat, net.location.coordinates.lng ) Geod.Meters )

        distances =
            List.map (distance coordinates) networks
    in
        List.sortBy snd distances |> List.head |> Maybe.map fst



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "body pure-g" ]
        <| [ Html.form [ class "pure-form pure-u-2-3", onSubmit SubmitLocation ]
                [ fieldset []
                    [ legend [] [ text "Find a Bike Share" ]
                    , input [ class "pure-input-1-3", type' "text", placeholder "Address, Location Name or Postal Code", value model.locationField, onInput UpdateLocationField ] []
                    , button [ class "pure-button pure-button-primary", type' "submit" ] [ text "Search" ]
                    ]
                ]
           ]
        ++ maybeNode errorDiv model.errorMessage
        ++ [ mapDiv ]
        ++ maybeNode networkCard model.nearestNetwork


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


getNetworks : Cmd Msg
getNetworks =
    let
        task =
            Http.get networksDecoder networksUrl
    in
        task |> Task.perform LoadNetworksError LoadNetworksSuccess


getStations : Network -> Cmd Msg
getStations network =
    let
        task =
            Http.get stationsDecoder <| stationsUrl network
    in
        task |> Task.perform LoadStationsError LoadStationsSuccess



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
