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
    , QuestionMsg(..)
    , EditableTextMsg(..)
    , ShortMsg(..)
    , LongMsg(..)
    , ListMsg(..)
    , EventSubPage(..)
    )

import Browser
import Json.Encode
import Type.Database exposing (Type)
import Type.IO.Form exposing (UpdateMsg(..))
import Type.IO.Setter as Updater
import Type.IO.Internal exposing (Id)
import Type.Database as Db
import Type.Database.InputType as IT
import Material.Snackbar as Snackbar
import Time exposing (Posix)
import Url
import Url.Builder
import DnDList


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
    | Question QuestionMsg
    | PageWithSubpage PageWithSubpageMsg
    | Db Updater.Msg
    | OnDbChange Json.Encode.Value
    | Search String
    | CRUD (DbMsg a)
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
    | AnswerQuestions {questionary: String, test_subject: String, event: String}
    | EventSwitchTo EventSubPage

type EventSubPage
    = EventSettings
    | EventOverview
    | EventPeople

type QuestionaryMsg
    = CurrentQuestionSelected (Maybe String)
    | QuestionNameEdit EditableTextMsg
    | ContextMenu (Maybe String)

{-     | OnQuestionDrag DnDList.Msg
    | Tock Posix -}

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
    


type DbMsg a
    = Create Type String (List (String -> Msg a))
    | CreateRandom Type (List (String -> Msg a))
    | Update Updater.Msg
    | UpdateAll (List Updater.Msg)
    | Delete Type (Id a String)
    | Access Type String
    --| SwapAttributes Type (String, String) String


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
