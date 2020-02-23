module Page exposing (..)

import Session
import Viewer
import Msg
import Browser exposing (Document)
import Session
import Html


type Page a msg=
    Page 
    {   session : Session.Session,
        page : a,
        view : (Page a msg -> Viewer.Details msg),
        toMsg : (msg -> Msg.Msg),
        header : Viewer.Header,
        update : (msg -> Page a msg -> (Page a msg, Cmd msg))
    }

view : Page a msg -> Document Msg.Msg
view (Page model) = 
    Viewer.view model.session model.toMsg (model.view (Page model)) model.header

liftview : (a -> Viewer.Details msg) -> Page a msg -> Viewer.Details msg
liftview pview (Page a)=
    pview a.page

update : msg -> Page a msg -> (Page a msg, Cmd msg)
update msg (Page model) =
    model.update msg (Page model)

liftupdate : (msg -> a -> (a, Cmd msg)) -> msg -> Page a msg -> (Page a msg, Cmd msg)
liftupdate uf msg (Page model) = 
    let
        (newModel, effect) = uf msg model.page
    in 
        (Page {model | page = newModel}, effect)
