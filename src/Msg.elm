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
import Type.Database.InputType as IT
import Material.Snackbar as Snackbar
import Time exposing (Posix)
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
    | SetUser String
    | Back
    | Tick Posix
    | SnackbarClosed Snackbar.MessageId
    


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
