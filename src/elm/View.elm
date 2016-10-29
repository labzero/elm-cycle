module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)
import Msg exposing (Msg(..))
import Model exposing (Model)
import String
import Maybe.Extra as Maybe


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
        ++ [ stationsView model.stations model.updatedStations model.selectedStation ]


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


stationsView : List Station -> List String -> Maybe Station -> Html Msg
stationsView stations updated selectedStation =
    div [ class "pure-u-1-1" ]
        <| if List.isEmpty stations then
            []
           else
            [ table [ class "pure-u-1-4 pure-table pure-table-horizontal" ]
                [ thead []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Free Bikes" ]
                    , th [] [ text "Empty Slots" ]
                    , th [] [ text "Distance" ]
                    ]
                , tbody [] <| List.map (stationRow updated selectedStation) stations
                ]
            ]


stationRow : List String -> Maybe Station -> Station -> Html Msg
stationRow updating selected station =
    let
        classes =
            stationClasses updating selected station
    in
        tr [ class classes, onClick <| StationClicked station ]
            [ td [] [ text <| station.name ]
            , td [] [ text <| toString station.freeBikes ]
            , td [] [ text <| toString station.emptySlots ]
            , td [] [ Maybe.map (toString << ceiling) station.distance |> Maybe.withDefault "" |> text ]
            ]


stationClasses : List String -> Maybe Station -> Station -> String
stationClasses updating selected station =
    List.concat
        [ [ "station-row" ]
        , if List.member station.id updating then
            [ "station-row-updated" ]
          else
            []
        , if Maybe.unwrap False ((==) station) selected then
            [ "station-row-focused" ]
          else
            []
        ]
        |> String.join " "


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


companyHelper : Company -> String
companyHelper company =
    case company of
        CompanyName name ->
            name

        MultipleCompanyNames names ->
            String.join ", " names

        NoCompany ->
            ""


maybeNode : (a -> b) -> Maybe a -> List b
maybeNode f x =
    let
        toList y = [y] 
    in
        Maybe.unwrap [] (toList << f) x
