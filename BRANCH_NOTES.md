* Step 1
 - Basic Elm Architecture

* Step 2
 - A Simple Form
 - update
 - elm-html

* Step 3
 - "3rd party" library
 - more about Maybe
 ```elm
maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Nothing ->
            []

        Just x ->
            [ x ]
```

 - Task and Cmd

* Step 4
 - JS interop / Ports
 - more Types

* Step 5
  - HTTP and JSON decoding

* Step 6
 - Another library (Geodesy)
 - Calculating distances

* Step 7
 - More HTTP and JSON
 - load stations

* Step 8
 - More JS interop
 - drop map markers

* Step 9
 - Refactor
 - Get fancy with the map definition

* Step 10
 - Geolocation (add option to use current location)

* Step 11
 - Display the stations

* Step 12
 - Sort stations by proximity

* Step 13
 - Subscriptions / Time
 - refresh stations periodically

* Step 14
 - a little visual feedback on update (sleep)

* Step 15
 - display the distance to each station
