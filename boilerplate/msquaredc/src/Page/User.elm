module Page.User exposing (Model, init, update, view, page)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Session
import Viewer
import Msg exposing (UserMsg)
import Page exposing (Page(..))


{-
   This is a page with subpages. You can change the behaviour depending on the subpage path!
-}
-- MODEL


type alias Model =
    { user_id: Maybe Int
    }



-- INIT


init : Maybe Int -> Model
init user_id =
    Model user_id

page : Session.Session -> Maybe Int -> (Page.Page Model UserMsg, Cmd UserMsg )
page session user_id =
    let 
        model = 
            {session = session,
            page = init user_id,
            view = view,
            toMsg = Msg.User,
            header = Viewer.header,
            update = Page.liftupdate update}
    in
        (Page model, Cmd.none )


-- UPDATE



update : UserMsg -> Model -> ( Model, Cmd UserMsg )
update msg model =
    case msg of
        Msg.UserNothing ->
            ( model, Cmd.none )



-- VIEW


view : Page Model UserMsg -> Viewer.Details UserMsg
view (Page model) =
    { title = toTitle model.page
    , body =
        [ h1 [] [ text "elm-spa-boilerplate - Page With Subpage" ]
        , div [ class "content" ]
            [ h3 [] [ text "This is a page that can handle subpaths in its routing." ]
            , h3 [] [ text <| "The current subpath is : /" ++ String.fromInt (Maybe.withDefault -1 model.page.user_id) ]
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
    "Page With Subpage - " ++ String.fromInt (Maybe.withDefault -1 model.user_id)
