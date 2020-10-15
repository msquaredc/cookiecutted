module Page exposing (..)

import Session
import Viewer
import Msg
import Browser exposing (Document)
import Session
import Html
import Time exposing (Posix)


type Page a msg=
    Page 
    {   session : Session.Session,
        page : a,
        view : (Page a msg -> Viewer.Details Msg.Msg),
        toMsg : (msg -> Msg.Msg),
        subscriptions : Sub msg,
        -- header : Viewer.Header,
        update : (msg -> Page a msg -> (Page a msg, Cmd msg))
    }

view : Page a msg -> Viewer.Header -> Maybe Posix -> Document Msg.Msg
view (Page model) header = 
    Viewer.view model.session model.toMsg (model.view (Page model)) header

liftview : (a -> Viewer.Details msg) -> Page a msg -> Viewer.Details msg
liftview pview (Page a)=
    pview a.page

update : msg -> Page a msg -> (Page a msg, Cmd msg)
update msg (Page model) =
    model.update msg (Page model)

-- updateHeader : Msg.ViewerMsg -> Page a msg -> Page a msg
-- updateHeader msg (Page model) =
--     Page {model| header = Viewer.update msg model.header}

liftupdate : (msg -> a -> (a, Cmd msg)) -> msg -> Page a msg -> (Page a msg, Cmd msg)
liftupdate uf msg (Page model) = 
    let
        (newModel, effect) = uf msg model.page
    in 
        (Page {model | page = newModel}, effect)
