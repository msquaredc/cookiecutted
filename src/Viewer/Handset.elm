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
                                                        , additionalAttributes = [ style "z-index" "1" ]}
        , drawerTitle = config.drawerTitle
        , drawerSubtitle = config.drawerSubtitle
        , content = config.drawerContent
        } 
    , drawerScrim [] []
    , div [TopAppBar.denseFixedAdjust][
        I.viewTopAppBar
            {topAppBar = shortTopAppBar {topAppBarConfig | dense = True
                                                , fixed = False}
            , navButton = Just {icon = "menu", message =  if config.drawerOpen then config.closeDrawer else config.openDrawer}
            , title = "Title"
            , search = Nothing
            , user = Nothing
            }
        , config.body
        ]
    ]

viewPortrait : I.ViewerConfig msg -> List (Html msg)
viewPortrait config =
    [
    I.viewDrawer
        {drawer = Drawer.modalDrawer {modalDrawerConfig | open = config.drawerOpen
                                                        , onClose = config.closeDrawer
                                                        , additionalAttributes = [ style "z-index" "1" ]}
        , drawerTitle = config.drawerTitle
        , drawerSubtitle = config.drawerSubtitle
        , content = config.drawerContent
        } 
    , drawerScrim [][]
    , div [TopAppBar.denseFixedAdjust][
        I.viewTopAppBar
            {topAppBar = shortTopAppBar {topAppBarConfig | dense = True
                                                , fixed = True}
            , navButton = Just {icon = "menu", message =  if config.drawerOpen then config.closeDrawer else config.openDrawer}
            , title = "Title"
            , search = Nothing
            , user = Nothing
            }
        , config.body
        ]
    ]