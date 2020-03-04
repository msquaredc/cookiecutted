module Viewer.Tablet exposing (..)

import Html exposing (Html,text,div)
import Html.Attributes exposing (style)
import Viewer.Internal as I
import Material.TopAppBar as TopAppBar exposing (topAppBar, topAppBarConfig)
import Material.Drawer as Drawer exposing (dismissibleDrawerConfig, modalDrawerConfig,drawerScrim)

viewLandscape : I.ViewerConfig msg -> List (Html msg)
viewLandscape config =
    [
        I.viewTopAppBar
            {topAppBar = topAppBar {topAppBarConfig | dense = True
                                                , fixed = True}
            , navButton = Just {icon = "menu", message =  if config.drawerOpen then config.closeDrawer else config.openDrawer}
            , title = config.title
            , search = Nothing
            , user = config.user
            },
        div [TopAppBar.denseFixedAdjust][
            I.viewDrawer
            {drawer = Drawer.dismissibleDrawer {dismissibleDrawerConfig | open = config.drawerOpen
                                                                        , additionalAttributes = [ style "z-index" "1" ]}
            , drawerTitle = config.drawerTitle
            , drawerSubtitle = config.drawerSubtitle
            , content = config.drawerContent
            }
            , Html.div [Drawer.appContent ] [
                config.body
            ]
    ]
    ]

viewPortrait : I.ViewerConfig msg -> List (Html msg)
viewPortrait config =
    [
    I.viewDrawer
        {drawer = Drawer.modalDrawer { modalDrawerConfig | open = config.drawerOpen
                                                         , additionalAttributes = [ style "z-index" "1" ]}
        , drawerTitle = config.drawerTitle
        , drawerSubtitle = config.drawerSubtitle
        , content = config.drawerContent
        } 
    , drawerScrim [] []
    , div [] [
        I.viewTopAppBar
            {topAppBar = topAppBar {topAppBarConfig | dense = False
                                                    , fixed = True}
            , navButton = Just {icon = "menu", message =  if config.drawerOpen then config.closeDrawer else config.openDrawer}
            , title = config.title
            , search = Nothing
            , user = Nothing
            }
        , div [TopAppBar.fixedAdjust] [config.body]
        ]
    ]