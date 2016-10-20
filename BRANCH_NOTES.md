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
