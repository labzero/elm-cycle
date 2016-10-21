module MapHelpers exposing (..)

import Types exposing (..)
import Geocoding as G
import Geodesy as Geod


-- Location Helpers


sortByDistance : List Station -> List Station
sortByDistance =
    List.sortBy (.distance >> Maybe.withDefault 999999999)


withDistanceFrom : Coordinates -> Station -> Station
withDistanceFrom coords station =
    { station | distance = Just <| distance coords station.coordinates }


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

            Nothing ->
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

            Nothing ->
                stationBounds
