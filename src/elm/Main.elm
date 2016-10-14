import Html exposing (..)
import Html.App as App

-- app
main : Program Never
main = App.program
  {
    init = {} ! []
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }


-- MODEL
type alias Model = {}


-- UPDATE
type Msg = NoOp 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> model ! []



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model = div [] [text "success!"]
 

