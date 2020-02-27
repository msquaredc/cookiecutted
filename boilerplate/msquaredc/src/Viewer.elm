module Viewer exposing (Details, Header, update, header, notFound, view)

--import Url.Builder

import Browser
import Html exposing (Html, a, div, h1, h3, text)
import Html.Attributes exposing (class, href, style)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.TopAppBar as TopAppBar exposing (topAppBar, topAppBarConfig)
import Material.Drawer as Drawer exposing (modalDrawer, modalDrawerConfig, dismissibleDrawerConfig, drawerContent, permanentDrawer, permanentDrawerConfig)
import Material.List exposing (list, listConfig, listItem, listItemConfig, listItemGraphic)
import Material.Typography as Typography
import Msg exposing (ViewerMsg(..))
import TestDrawer
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
update : ViewerMsg -> Header -> Header
update msg model =
    case msg of
        OpenDrawer ->
            {model|drawerOpen = True}
        
        CloseDrawer ->
            {model|drawerOpen = False}


-- VIEW


view : Session.Session -> (a -> Msg.Msg) -> Details a -> Header -> Browser.Document Msg.Msg
view session msg details h =
    { title = details.title ++ Utils.genericTitle
    , body =
        [TestDrawer.main]
       --  [Html.map Msg.Viewer (viewAll h details.title)]
        {- [ viewHeader2 h details.title
            |> Html.map Msg.Viewer

        --, Utils.logo 256
        , 
            div
                [ TopAppBar.fixedAdjust
--                , class "container"
--                , class "main"
--                , style "height" (String.fromInt (session.windowSize.height - headerHeight - footerHeight) ++ "px")
                ]
                    [viewDrawer h (List.map (Html.map msg) details.body)]
        , viewFooter
        ] -}
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
    { drawerOpen = True
    }

viewDrawer : Header -> List (Html.Html Msg.Msg) -> (Html.Html Msg.Msg)
viewDrawer config content =
    div demoPanel
    [Drawer.modalDrawer
        { modalDrawerConfig
            | open = True
            , onClose = Just (Msg.Viewer Msg.CloseDrawer)
            , additionalAttributes =
                        [ TopAppBar.fixedAdjust
                        , Html.Attributes.style "z-index" "1"
                        ]
        }
        [ drawerContent [][]
        ]
    , Html.div [] content
    ]

viewAll : Header -> String -> Html Msg.ViewerMsg
viewAll config name =
    let
        toggleCatalogDrawer =
            if config.drawerOpen then
                Msg.CloseDrawer

            else
                Msg.OpenDrawer
    in
    Html.div catalogPageContainer
        [ topAppBar topAppBarConfig
            [ TopAppBar.row []
                [ TopAppBar.section [ TopAppBar.alignStart ]
                    [ iconButton
                        { iconButtonConfig
                            | additionalAttributes = [ TopAppBar.navigationIcon ]
                            , onClick = Just toggleCatalogDrawer
                        }
                        "menu"
                    , Html.span
                        [ TopAppBar.title
                        , Html.Attributes.style "text-transform" "uppercase"
                        , Html.Attributes.style "font-weight" "400"
                        ]
                        [ text "Material Components for Elm" ]
                    ]
                ]
            ]
        , Html.div 
            [ style "display" "flex"
            , style "flex-flow" "row nowrap"
            ]
            [ modalDrawer {modalDrawerConfig | open = True}
                [ drawerContent []
                    [ list listConfig
                        [ listItem listItemConfig
                            [ text "Home" ]
                        , listItem listItemConfig
                            [ text "Log out" ]
                        ]
                    ]
                ]
            , Html.div [] [ text "Main Content" ]
            ]
            -- , 
            --     Html.div (TopAppBar.fixedAdjust :: Drawer.appContent :: demoContent)
            --         [ Html.div demoContentTransition
            --             (Html.h1 [ Typography.headline5 ] [ text "catalogPage.title" ]
            --                 :: Html.p [ Typography.body1 ] [ text "catalogPage.prelude" ]
            --                 --:: Html.div hero catalogPage.hero
            --                 :: Html.h2 (Typography.headline6 :: demoTitle)
            --                     [ text "Resources" ]
            --                 --:: resourcesList catalogPage.resources
            --                 :: Html.h2 (Typography.headline6 :: demoTitle)
            --                     [ text "Demos" ]
            --                 :: [text "catalogPage.content"]
            --             )
            --         ]
            --]
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

catalogPageContainer : List (Html.Attribute msg)
catalogPageContainer =
    [ Html.Attributes.style "position" "relative"
    , Typography.typography
    ]

demoPanel : List (Html.Attribute msg)
demoPanel =
    [ Html.Attributes.style "display" "-ms-flexbox"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "position" "relative"
    , Html.Attributes.style "height" "100vh"
    , Html.Attributes.style "overflow" "hidden"
    ]

demoContent : List (Html.Attribute msg)
demoContent =
    [ Html.Attributes.id "demo-content"
    , Html.Attributes.style "height" "100%"
    , Html.Attributes.style "-webkit-box-sizing" "border-box"
    , Html.Attributes.style "box-sizing" "border-box"
    , Html.Attributes.style "max-width" "100%"
    , Html.Attributes.style "padding-left" "16px"
    , Html.Attributes.style "padding-right" "16px"
    , Html.Attributes.style "padding-bottom" "100px"
    , Html.Attributes.style "width" "100%"
    , Html.Attributes.style "overflow" "auto"
    , Html.Attributes.style "display" "-ms-flexbox"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "-ms-flex-direction" "column"
    , Html.Attributes.style "flex-direction" "column"
    , Html.Attributes.style "-ms-flex-align" "center"
    , Html.Attributes.style "align-items" "center"
    , Html.Attributes.style "-ms-flex-pack" "start"
    , Html.Attributes.style "justify-content" "flex-start"
    ]

demoContentTransition : List (Html.Attribute msg)
demoContentTransition =
    [ Html.Attributes.style "max-width" "900px"
    , Html.Attributes.style "width" "100%"
    ]

demoTitle : List (Html.Attribute msg)
demoTitle =
    [ Html.Attributes.style "border-bottom" "1px solid rgba(0,0,0,.87)"
    ]