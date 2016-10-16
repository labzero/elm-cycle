port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Http
import Geocoding as G
import Task exposing (Task)


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
    , geocodingData : Maybe G.Response
    , errorMessage : Maybe String
    }


initialModel : Model
initialModel =
    { locationField = ""
    , geocodingData = Nothing
    , errorMessage = Nothing
    }



-- UPDATE


type Msg
    = UpdateLocationField String
    | SubmitLocation
    | GeocodingError Http.Error
    | GeocodingSuccess G.Response


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
            { model | geocodingData = Just data } ! [ createMapForLocation <| mapSpecForResponse data ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "body pure-g" ]
        [ Html.form [ class "pure-form pure-u-2-3", onSubmit SubmitLocation ]
            [ fieldset []
                [ legend [] [ text "Find a Bike Share" ]
                , input [ class "pure-input-1-3", type' "text", placeholder "Address, Location Name or Postal Code", value model.locationField, onInput UpdateLocationField ] []
                , button [ class "pure-button pure-button-primary", type' "submit" ] [ text "Search" ]
                ]
            ]
        , errorDiv model.errorMessage
        , mapDiv
        , div [ class "pure-u-2-3" ] [ model.geocodingData |> Maybe.map toString |> Maybe.withDefault "" |> text ]
        ]


errorDiv : Maybe String -> Html Msg
errorDiv errorMsg =
    case errorMsg of
        Just str ->
            div [ class "error pure-u-2-3" ] [ text str ]

        _ ->
            div [ class "noerror " ] []


mapDiv : Html Msg
mapDiv =
    div [ class "pure-u-1-1", id "map" ] []



-- Cmds


googleKey : String
googleKey =
    "AIzaSyAji7Gm0r66d9QfW1aPYGSocFlawSXNLMw"


geocodeLocation : String -> Cmd Msg
geocodeLocation str =
    G.requestForAddress googleKey str
        |> G.send
        |> Task.perform GeocodingError GeocodingSuccess



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
