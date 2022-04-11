port module Ports exposing (clearLocalStorage, onDbChange, onLocalStorageChange, toDb, toLocalStorage)

import Json.Encode
import Type.Database
import Type.LocalStorage



{-
   This file holds the Ports that our application will use.
   I have included the necessary ports for working with localStorage, as well as their respective handlers in JS
-}
-- Listener for change to localStorage


port onLocalStorageChange : (Json.Encode.Value -> msg) -> Sub msg


port onDbChange : (Json.Encode.Value -> msg) -> Sub msg



-- Set localStorage


port toLocalStorage : Type.LocalStorage.LocalStorage -> Cmd msg


port toDb : Json.Encode.Value -> Cmd msg



-- Clear localStorage


port clearLocalStorage : () -> Cmd msg
