module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App


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
    }


initialModel : Model
initialModel =
    { locationField = ""
    }



-- UPDATE


type Msg
    = UpdateLocationField String
    | SubmitLocation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLocationField str ->
            { model | locationField = str } ! []

        SubmitLocation ->
            { model | locationField = "" } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "body" ]
        [ Html.form [ class "pure-form " ]
            [ fieldset []
                [ legend [] [ text "Find a Bike Share" ]
                , input [ class "pure-input-1-3", type' "text", placeholder "Address, Location Name or Postal Code", value model.locationField, onInput UpdateLocationField ] []
                , button [ class "pure-button pure-button-primary", type' "button", onClick SubmitLocation ] [ text "Search" ]
                ]
            ]
        ]
