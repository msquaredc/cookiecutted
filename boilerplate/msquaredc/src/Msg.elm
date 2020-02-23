module Msg exposing
    ( Msg(..)
    , PageOneMsg(..)
    , PageWithSubpageMsg(..)
    , TopMsg(..)
    , ViewerMsg(..)
    , UserMsg(..)
    , AdminMsg(..)
    , DbMsg (..)
    )

import Browser
import Json.Encode
import Url
import Type.IO.Form exposing (UpdateMsg(..))
import Type.IO.Setter as Updater
import Type.Database.TypeMatching exposing (Type)


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | OnWindowResize Int Int
    | OnLocalStorageChange Json.Encode.Value
    | Viewer ViewerMsg
    | Top TopMsg
    | User UserMsg
    | Admin AdminMsg
      -- | NewPageMsg NewPage.Msg
    | PageOne PageOneMsg
    | PageWithSubpage PageWithSubpageMsg
    | Db Updater.Msg
    | OnDbChange Json.Encode.Value


type ViewerMsg
    = OpenDrawer
    | CloseDrawer


type TopMsg
    = NoOp
    | LocalStorageInputFieldChange String
    | SetLocalStorage
    | ClearLocalStorage

type DbMsg
    = Create Type String


type PageOneMsg
    = PageOneNothing


type PageWithSubpageMsg
    = PWSNothing


type UserMsg
    = UserNothing


type AdminMsg
    = AdminForm UpdateMsg
    | AdminDb DbMsg
    | ValueChanged String
