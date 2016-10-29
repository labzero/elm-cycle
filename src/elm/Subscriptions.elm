module Subscriptions exposing (subscriptions)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.nearestNetwork of
        Just network ->
            Time.every (15 * Time.second) (\_ -> UpdateStations network)

        Nothing ->
            Sub.none
