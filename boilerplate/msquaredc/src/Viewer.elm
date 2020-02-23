module Viewer exposing (Details, Header, header, notFound, view)

--import Url.Builder

import Browser
import Html exposing (Html, a, div, h1, h3, text)
import Html.Attributes exposing (class, href, style)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.TopAppBar as TopAppBar exposing (topAppBar, topAppBarConfig)
import Material.Drawer as Drawer exposing (modalDrawer, modalDrawerConfig, dismissibleDrawerConfig)
import Msg exposing (ViewerMsg)
import Session
import Utils



{-
   This module builds the view. Every individual page's view function returns a Viewer.Details,
   which is used to generate a Browser.Document msg in this module
-}


type alias Details msg =
    { title : String
    , body : List (Html msg)
    }


type alias Header =
    { drawerOpen : Bool
    }



-- UPDATE
-- VIEW


view : Session.Session -> (a -> Msg.Msg) -> Details a -> Header -> Browser.Document Msg.Msg
view session msg details h =
    { title = details.title ++ Utils.genericTitle
    , body =
        [ viewHeader2 h details.title
            |> Html.map Msg.Viewer

        --, Utils.logo 256
        , 
            div
                [ TopAppBar.fixedAdjust
--                , class "container"
--                , class "main"
--                , style "height" (String.fromInt (session.windowSize.height - headerHeight - footerHeight) ++ "px")
                ]
                    (viewDrawer h (List.map (Html.map msg) details.body))
        , viewFooter
        ]
    }



-- HEADER
-- This header is statically generated. You'd likely want this to be dynamic in some way based on the active page & session
-- You could create an additional field in Viewer.Details for a header, and create this header in Main.elm or in each individual page's view
{- viewHeader : Html msg
   viewHeader =
       div [ class "header", class "container" ]
           [ div [ class "logo" ] [ viewLogo ]
           , div [ class "nav-links" ]
               [ a [ href "/" ] [ text "Home" ]
               , a [ href "/pageone" ] [ text "Page One" ]
               , a [ href "/pagewithsubpage/hello" ] [ text "Page With Subpage" ]

               --    , a [ href "newpage" ] [ text "New Page" ]
               ]
           ]
-}


viewHeader2 : Header -> String -> Html ViewerMsg
viewHeader2 config name =
    let
        toggleDrawer =
            if config.drawerOpen then
                Msg.CloseDrawer

            else
                Msg.OpenDrawer
    in
    topAppBar { topAppBarConfig | fixed = True}
        [ TopAppBar.row []
            [ TopAppBar.section [ TopAppBar.alignStart ]
                [ iconButton
                    { iconButtonConfig
                        | additionalAttributes = [ TopAppBar.navigationIcon ]
                        , onClick = Just toggleDrawer
                    }
                    "menu"
                , Html.span
                    [ TopAppBar.title
                    , Html.Attributes.style "text-transform" "uppercase"
                    , Html.Attributes.style "font-weight" "400"
                    ]
                    [ text name ]
                ]
            ]
        ]



-- FOOTER


viewFooter : Html msg
viewFooter =
    div [ class "footer", class "container" ]
        [ text "A simple, no-frills boilerplate for creating delightful Single Page Applications (SPAs) in Elm."
        , a [ href "https://github.com/jzxhuang/elm-spa-boilerplate" ] [ text "Check it out on Github!" ]
        , text "© 2018 - present Jeffrey Huang."
        ]



-- 404 PAGE (NotFound)


notFound : Details msg
notFound =
    { title = "Page Not Found"
    , body =
        [ div [ class "not-found" ]
            [ div [ style "font-size" "12em" ] [ text "404" ]
            , h1 [ style "font-size" "3.5em" ] [ text "Page Not Found" ]
            , h3 [ style "font-size" "1.5em" ]
                [ text "Oops - Looks like you got lost or clicked a bad link! "
                , a [ href "/" ] [ text "Click here " ]
                , text "to go back to the home page."
                ]
            ]
        ]
    }


header : Header
header =
    { drawerOpen = False
    }

viewDrawer : Header -> List (Html.Html Msg.Msg) -> List (Html.Html Msg.Msg)
viewDrawer config content =
    [Drawer.dismissibleDrawer
        { dismissibleDrawerConfig
            | open = True
            , onClose = Just (Msg.Viewer Msg.CloseDrawer)
        }
        [ Drawer.drawerHeader []
        [ Html.h3 [ Drawer.drawerTitle ] [ text "Mail" ]
        , Html.h6 [ Drawer.drawerSubtitle ] [ text "email@material.io" ]
        ]
    , Drawer.drawerContent [] [] ]
    , Drawer.drawerScrim [] []
    , Html.div [] content
    ]

-- LOGO
-- viewLogo : Html msg
-- viewLogo =
--     a [ href "/", style "text-decoration" "none" ] [ Utils.logo 32 ]
-- STYLING HELPERS (lazy, hard-coded styling)


headerHeight : Int
headerHeight =
    60


footerHeight : Int
footerHeight =
    60
