module Viewer.Desktop exposing (..)

import Html exposing (Html,text,div)
import Html.Attributes exposing (style)
import Viewer.Internal as I
import Material.TopAppBar as TopAppBar exposing (prominentTopAppBar, topAppBarConfig)
import Material.Drawer as Drawer exposing (permanentDrawer,permanentDrawerConfig)

viewLandscape : I.ViewerConfig msg -> List (Html msg)
viewLandscape config =
    [
    I.viewTopAppBar
        {topAppBar = prominentTopAppBar {topAppBarConfig | dense = False
                                            , fixed = True}
        , navButton = Just {icon = config.navButtonIcon, message = config.navButtonCallback}
        , title = Maybe.withDefault "Landscape Desktop" config.title
        , search = Nothing
        , user = config.user
        }
    , div [TopAppBar.prominentFixedAdjust] 
        [I.viewDrawer
            {drawer = Drawer.permanentDrawer {permanentDrawerConfig | additionalAttributes = [ style "z-index" "1" ]}
            , drawerTitle = text config.drawerTitle
            , drawerSubtitle = config.drawerSubtitle
            , content = config.drawerContent
            } 
        ]
    , div [Drawer.appContent][ config.body ]
    ]
    

viewPortrait : I.ViewerConfig msg -> List (Html msg)
viewPortrait config =
    [
    I.viewTopAppBar
        {topAppBar = prominentTopAppBar {topAppBarConfig | dense = False
                                            , fixed = True}
        , navButton = Just {icon = config.navButtonIcon, message = config.navButtonCallback}
        , title = Maybe.withDefault "Portrait Desktop" config.title
        , search = Nothing
        , user = Nothing
        }
    , div [TopAppBar.prominentFixedAdjust]
        [I.viewDrawer
            {drawer = Drawer.permanentDrawer {permanentDrawerConfig | additionalAttributes = [ style "z-index" "1" ]}
            , drawerTitle = text config.drawerTitle
            , drawerSubtitle = config.drawerSubtitle
            , content = config.drawerContent
            } 
        ]
    , div [Drawer.appContent][ config.body]
    ]