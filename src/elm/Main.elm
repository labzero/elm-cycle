module Main exposing (..)

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
            { model | geocodingData = Just data } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "body pure-g" ]
        [ Html.form [ class "pure-form pure-u-2-3" ]
            [ fieldset []
                [ legend [] [ text "Find a Bike Share" ]
                , input [ class "pure-input-1-3", type' "text", placeholder "Address, Location Name or Postal Code", value model.locationField, onInput UpdateLocationField ] []
                , button [ class "pure-button pure-button-primary", type' "button", onClick SubmitLocation ] [ text "Search" ]
                ]
            ]
        , errorDiv model.errorMessage
        , div [] [ model.geocodingData |> Maybe.map toString |> Maybe.withDefault "" |> text ]
        ]


errorDiv : Maybe String -> Html Msg
errorDiv errorMsg =
    case errorMsg of
        Just str ->
            div [ class "error pure-u-2-3" ] [ text str ]

        _ ->
            div [ class "noerror " ] []



-- Cmds


googleKey : String
googleKey =
    "AIzaSyAji7Gm0r66d9QfW1aPYGSocFlawSXNLMw"


geocodeLocation : String -> Cmd Msg
geocodeLocation str =
    G.requestForAddress googleKey str
        |> G.send
        |> Task.perform GeocodingError GeocodingSuccess
