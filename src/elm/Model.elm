module Model exposing (Model)

import Types exposing (..)


type alias Model =
    { locationField : String
    , geocodingData : Maybe MapSpec
    , mapSpec : Maybe MapSpec
    , errorMessage : Maybe String
    , bikeNetworks : Maybe (List Network)
    , nearestNetwork : Maybe Network
    , stations : List Station
    , updatedStations : List String
    , selectedStation : Maybe Station
    }
