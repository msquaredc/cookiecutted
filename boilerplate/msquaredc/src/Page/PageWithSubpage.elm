module Page.PageWithSubpage exposing (Model, init, update, view, page)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Session
import Viewer
import Msg exposing (PageWithSubpageMsg)
import Page



{-
   This is a page with subpages. You can change the behaviour depending on the subpage path!
-}
-- MODEL


type alias Model =
    { subpage : String
    }



-- INIT


init : String -> Model
init subpage =
    Model subpage

page : Session.Session -> String -> (Page.Page Model PageWithSubpageMsg, Cmd PageWithSubpageMsg )
page session subpage =
    let 
        model = 
            {session = session,
            page = init subpage,
            view = Page.liftview view,
            toMsg = Msg.PageWithSubpage,
            header = Viewer.header,
            update = Page.liftupdate update}
    in
        (Page.Page model, Cmd.none )


-- UPDATE



update : PageWithSubpageMsg -> Model -> ( Model, Cmd PageWithSubpageMsg )
update msg model =
    case msg of
        Msg.PWSNothing ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Viewer.Details PageWithSubpageMsg
view model =
    { title = toTitle model
    , body =
        [ h1 [] [ text "elm-spa-boilerplate - Page With Subpage" ]
        , div [ class "content" ]
            [ h3 [] [ text "This is a page that can handle subpaths in its routing." ]
            , h3 [] [ text <| "The current subpath is : /" ++ model.subpage ]
            , div [] [ text "The subpath could be anything, or a specific type, like a string or integer. You can have many levels of subpaths if you wanted!" ]
            , div []
                [ text " This demo accepts a single level subpath that can be any string. For example, "
                , a [ href "/pagewithsubpage/xyz" ] [ text "/pagewithsubpage/xyz" ]
                ]
            , div [] [ a [ href "/pagewithsubpage/a-wonderful-subpath" ] [ text "click here to go to a subpath" ] ]
            , div [] [ a [ href "/pagewithsubpage/i-love-elm" ] [ text "click here to go to another subpath" ] ]
            ]
        ]
    }



-- HELPERS


toTitle model =
    "Page With Subpage - " ++ model.subpage
