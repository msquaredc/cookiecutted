module Msg exposing
    ( AdminMsg(..)
    , DbMsg(..)
    , Msg(..)
    , PageOneMsg(..)
    , PageWithSubpageMsg(..)
    , TopMsg(..)
    , UserMsg(..)
    , ViewerMsg(..)
    , StudyMsg(..)
    , EventMsg(..)
    , QuestionaryMsg(..)
    , EditableTextMsg(..)
    )

import Browser
import Json.Encode
import Type.Database exposing (Type)
import Type.IO.Form exposing (UpdateMsg(..))
import Type.IO.Setter as Updater
import Time exposing (Posix)
import Url


type Msg a
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | OnWindowResize Int Int
    | OnLocalStorageChange Json.Encode.Value
    | Viewer ViewerMsg
    | Top TopMsg
    | User UserMsg
    | Admin (AdminMsg a)
      -- | NewPageMsg NewPage.Msg
    | PageOne PageOneMsg
    | Study StudyMsg
    | Event EventMsg
    | Questionary QuestionaryMsg
    | PageWithSubpage PageWithSubpageMsg
    | Db Updater.Msg
    | OnDbChange Json.Encode.Value
    | Search String
    | CRUD (DbMsg a)
    | Form UpdateMsg
    | Follow Type String
    | SetUser String
    | Back
    | Tick Posix
    


type ViewerMsg
    = OpenDrawer
    | CloseDrawer
    | NewUsername String


type TopMsg
    = NoOp
    | LocalStorageInputFieldChange String
    | SetLocalStorage
    | ClearLocalStorage
   

type EditableTextMsg
    = GetFocus
    | LooseFocus

type StudyMsg
    = StudyNameEdit EditableTextMsg

type EventMsg
    = EventMsgNothing

type QuestionaryMsg
    = CurrentQuestionSelected (Maybe String)
    | QuestionNameEdit EditableTextMsg
    


type DbMsg a
    = Create Type String (List (String -> Msg a))
    | CreateRandom Type (List (String -> Msg a))
    | Update Updater.Msg
    | SwapAttributes Type (String, String) String (a -> Updater.Msg)


type PageOneMsg
    = PageOneNothing


type PageWithSubpageMsg
    = PWSNothing


type UserMsg
    = UserNothing


type AdminMsg a
    = AdminForm UpdateMsg
    | AdminDb (DbMsg a)
    | ValueChanged String
