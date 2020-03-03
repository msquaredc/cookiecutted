module Viewer.Desktop exposing (..)

import Html exposing (Html,text,div)
import Viewer.Internal as I
import Material.TopAppBar as TopAppBar exposing (prominentTopAppBar, topAppBarConfig)
import Material.Drawer as Drawer exposing (permanentDrawer,permanentDrawerConfig)

viewLandscape : I.ViewerConfig msg -> List (Html msg)
viewLandscape config =
    [
    I.viewTopAppBar
        {topAppBar = prominentTopAppBar {topAppBarConfig | dense = False
                                            , fixed = True}
        , navButton = Nothing
        , title = config.title
        , search = Nothing
        , user = Nothing
        }
    , div [TopAppBar.prominentFixedAdjust] 
        [I.viewDrawer
            {drawer = Drawer.permanentDrawer permanentDrawerConfig
            , drawerTitle = config.drawerTitle
            , drawerSubtitle = config.drawerSubtitle
            , content = config.drawerContent
            } 
        , div [][ config.body ]
        ]
    ]
    

viewPortrait : I.ViewerConfig msg -> List (Html msg)
viewPortrait config =
    [
    I.viewTopAppBar
        {topAppBar = prominentTopAppBar {topAppBarConfig | dense = False
                                            , fixed = True}
        , navButton = Nothing
        , title = config.title
        , search = Nothing
        , user = Nothing
        }
    , div [TopAppBar.prominentFixedAdjust]
        [I.viewDrawer
            {drawer = Drawer.permanentDrawer permanentDrawerConfig
            , drawerTitle = config.drawerTitle
            , drawerSubtitle = config.drawerSubtitle
            , content = config.drawerContent
            } 
        , div [][ config.body]
        ]
    ]