module Viewer.Desktop exposing (..)

import Html exposing (Html,text,div)
import Html.Attributes exposing (style)
import Viewer.Internal as I
import Material.TopAppBar as TopAppBar exposing (prominent, config)
import Material.Drawer.Permanent as Drawer exposing (drawer, config)

viewLandscape : I.ViewerConfig msg -> List (Html msg)
viewLandscape config =
    [
    I.viewTopAppBar
        {topAppBar = prominent 
            (TopAppBar.config 
                |> TopAppBar.setDense False
                |> TopAppBar.setFixed True
            )
        , navButton = Just {icon = config.navButtonIcon, message = config.navButtonCallback}
        , title = Maybe.withDefault "Landscape Desktop" config.title
        , search = Nothing
        , user = config.user
        }
    , div [TopAppBar.prominentFixedAdjust] 
        [I.viewDrawer
            {drawer = Drawer.drawer 
                (Drawer.config |> Drawer.setAttributes [ style "z-index" "1" ])
            , drawerTitle = text config.drawerTitle
            , drawerSubtitle = config.drawerSubtitle
            , content = config.drawerContent
            } 
        ]
    , Drawer.content [][ config.body ]
    ]
    

viewPortrait : I.ViewerConfig msg -> List (Html msg)
viewPortrait config =
    [
    I.viewTopAppBar
        {topAppBar = prominent 
            (TopAppBar.config 
                |> TopAppBar.setDense False
                |> TopAppBar.setFixed True
            )
        , navButton = Just {icon = config.navButtonIcon, message = config.navButtonCallback}
        , title = Maybe.withDefault "Portrait Desktop" config.title
        , search = Nothing
        , user = Nothing
        }
    , div [TopAppBar.prominentFixedAdjust]
        [I.viewDrawer
            {drawer = Drawer.drawer (Drawer.config |> Drawer.setAttributes [ style "z-index" "1" ])
            , drawerTitle = text config.drawerTitle
            , drawerSubtitle = config.drawerSubtitle
            , content = config.drawerContent
            } 
        ]
    , Drawer.content [] [config.body]
    ]