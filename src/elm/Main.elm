port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Http
import String
import Time exposing (Time)
import Date exposing (Date)
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
    , errorMessage : Maybe String
    , bikeNetworks : Maybe (List Network)
    , nearestNetwork : Maybe Network
    , stations : List Station
    }


initialModel : Model
initialModel =
    { locationField = ""
    , geocodingData = Nothing
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
            let
                mapSpec =
                    mapSpecForResponse data
            in
                { model | geocodingData = mapSpec } ! [ createMapForLocation mapSpec, getNetworks ]

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
            { model | stations = Debug.log (toString stations) stations } ! []


findNearestNetwork : List Network -> Coordinates -> Maybe Network
findNearestNetwork networks coordinates =
    let
        distance coords net =
            ( net, Geod.distance ( coords.lat, coords.lng ) ( net.location.latitude, net.location.longitude ) Geod.Meters )

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
        , td [] [ text <| toString network.location.latitude ]
        , td [] [ text <| toString network.location.longitude ]
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
    MapSpec (coordinates loc) (bounds viewport)


coordinates : { latitude : Float, longitude : Float } -> Coordinates
coordinates loc =
    Coordinates loc.latitude loc.longitude


bounds : G.Viewport -> Bounds
bounds v =
    Bounds v.northeast.longitude v.northeast.latitude v.southwest.latitude v.northeast.longitude



-- Types


type alias MapSpec =
    { center : Coordinates
    , bounds : Bounds
    }


type alias Coordinates =
    { lat : Float
    , lng : Float
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
    , latitude : Float
    , longitude : Float
    }


type alias Station =
    { id : String
    , name : String
    , latitude : Float
    , longitude : Float
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
        |> Decode.required "latitude" float
        |> Decode.required "longitude" float


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
        |> Decode.required "latitude" float
        |> Decode.required "longitude" float
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
