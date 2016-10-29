module Types exposing (..)

import Time exposing (Time)


type alias MapSpec =
    { center : Coordinates
    , bounds : Bounds
    , markers : List MarkerSpec
    }


type alias Coordinates =
    { lat : Float
    , lng : Float
    }


type alias MarkerSpec =
    { location : Coordinates
    , title : String
    }


type alias Bounds =
    { east : Float
    , north : Float
    , south : Float
    , west : Float
    }


type alias Network =
    { id : String
    , name : String
    , href : String
    , location : NetworkLocation
    , company : Company
    }


type Company
    = CompanyName String
    | MultipleCompanyNames (List String)
    | NoCompany


type alias NetworkLocation =
    { city : String
    , country : String
    , coordinates : Coordinates
    }


type alias Station =
    { id : String
    , name : String
    , coordinates : Coordinates
    , emptySlots : Int
    , freeBikes : Int
    , timestamp : Time
    , distance : Maybe Float
    }
