port module Ports exposing (createMapForLocation, zoomMap)

import Maybe.Extra as Maybe
import Types exposing (..)


createMapForLocation : Maybe MapSpec -> Cmd msg
createMapForLocation =
    Maybe.unwrap Cmd.none createMap


port zoomMap : Coordinates -> Cmd msg


port createMap : MapSpec -> Cmd msg
