module Msg exposing
    ( AdminMsg(..)
    , DbMsg(..)
    , EditableTextMsg(..)
    , EventMsg(..)
    , EventSubPage(..)
    , ListMsg(..)
    , LongMsg(..)
    , Msg(..)
    , PageOneMsg(..)
    , PageWithSubpageMsg(..)
    , QuestionMsg(..)
    , QuestionaryMsg(..)
    , ShortMsg(..)
    , StudyMsg(..)
    , TopMsg(..)
    , UserMsg(..)
    , ViewerMsg(..)
    )

import Browser
import DnDList
import Json.Encode
import Material.Snackbar as Snackbar
import Time exposing (Posix)
import Type.Database as Db exposing (Type)
import Type.Database.InputType as IT
import Type.IO.Form exposing (UpdateMsg(..))
import Type.IO.Internal exposing (Id)
import Type.IO.Setter as Updater
import Url
import Url.Builder


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
    | Study StudyMsg
    | Event EventMsg
    | Questionary QuestionaryMsg
    | Question QuestionMsg
    | PageWithSubpage PageWithSubpageMsg
    | Db Updater.Msg
    | OnDbChange Json.Encode.Value
    | Search String
    | CRUD DbMsg
    | Form UpdateMsg
    | Follow Type String
    | FollowSubpage Type String (List String) (List Url.Builder.QueryParameter)
    | SetUser (Id Db.User String)
    | Back
    | Tick Posix
    | SnackbarClosed Snackbar.MessageId
    | DnDEvent DnDList.Msg


type ViewerMsg
    = OpenDrawer
    | CloseDrawer
    | OpenDialog
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
    | ExportStudy String


type EventMsg
    = EventNameEdit EditableTextMsg
    | AnswerQuestions { questionary : String, test_subject : String, event : String }
    | EventSwitchTo EventSubPage


type EventSubPage
    = EventSettings
    | EventOverview
    | EventPeople


type QuestionaryMsg
    = CurrentQuestionSelected (Maybe String)
    | QuestionNameEdit EditableTextMsg
    | ContextMenu (Maybe String)



{- | OnQuestionDrag DnDList.Msg
   | Tock Posix
-}


type QuestionMsg
    = SetInputType String
    | Short ShortMsg
    | Long LongMsg
    | List ListMsg


type ShortMsg
    = ShortLabel String
    | ShortPlaceholder String


type LongMsg
    = LongLabel String


type ListMsg
    = SingleInput IT.SingleInputType


type DbMsg
    = Create Type String (List (String -> Msg))
    | CreateRandom Type (List (String -> Msg))
    | Update Updater.Msg
    | UpdateAll (List Updater.Msg)
    | Delete Type String
    | Access Type String



--| SwapAttributes Type (String, String) String


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
