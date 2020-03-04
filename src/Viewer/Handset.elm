module Viewer.Handset exposing (..)

import Html exposing (Html,text,div)
import Viewer.Internal as I
import Material.TopAppBar as TopAppBar exposing (shortTopAppBar, topAppBarConfig)
import Material.Drawer as Drawer exposing (modalDrawerConfig, drawerScrim)
import Html.Attributes exposing (style)

viewLandscape : I.ViewerConfig msg -> List (Html msg)
viewLandscape config =
    [
    I.viewDrawer
        {drawer = Drawer.modalDrawer {modalDrawerConfig | open = config.drawerOpen
                                                        , onClose = config.closeDrawer
                                                        }
        , drawerTitle = config.drawerTitle
        , drawerSubtitle = config.drawerSubtitle
        , content = config.drawerContent
        } 
    , drawerScrim [] []
    , div [ Drawer.appContent][
        I.viewTopAppBar
            {topAppBar = shortTopAppBar {topAppBarConfig | dense = True
                                                , fixed = False}
            , navButton = Just {icon = config.navButtonIcon, message = config.navButtonCallback}
            , title = Maybe.withDefault "Landscape Handset" config.title
            , search = Nothing
            , user = Nothing
            }
        ]
    , div [TopAppBar.denseFixedAdjust][config.body]
    ]

viewPortrait : I.ViewerConfig msg -> List (Html msg)
viewPortrait config =
    -- [
    -- Drawer.modalDrawer
    --     { modalDrawerConfig
    --         | open = True
    --         , onClose = config.closeDrawer
    --     }
    --     [ Drawer.drawerContent [] [] ]
    -- , drawerScrim [] []
    -- , Html.div [] [ text "Main Content" ]
    [
    I.viewDrawer
        {drawer = Drawer.modalDrawer {modalDrawerConfig | open = config.drawerOpen
                                                        , onClose = config.closeDrawer
                                                        }
        , drawerTitle = config.drawerTitle
        , drawerSubtitle = config.drawerSubtitle
        , content = config.drawerContent
        } 
    , drawerScrim [][]
    , div [Drawer.appContent][
        I.viewTopAppBar
            {topAppBar = shortTopAppBar {topAppBarConfig | dense = True
                                                , fixed = True}
            , navButton = Just {icon = config.navButtonIcon, message = config.navButtonCallback}
            , title = Maybe.withDefault "Portrait Handset" config.title
            , search = Nothing
            , user = Nothing
            }
        ]
    , div [TopAppBar.denseFixedAdjust][config.body]
    ]