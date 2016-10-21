module Main exposing (..)

import Html.App as App
import Update exposing (update)
import View exposing (view)
import Subscriptions exposing (subscriptions)
import Model exposing (Model)


-- app


main : Program Never
main =
    App.program
        { init = initialModel ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
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
    , selectedStation = Nothing
    }
