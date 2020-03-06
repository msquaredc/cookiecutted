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
            , navButton = Just {icon = config.navButtonIcon, message = config.navButtonCallback}
            , title = Maybe.withDefault "Landscape Tablet" config.title
            , search = Nothing
            , user = config.user
            },
        div [TopAppBar.denseFixedAdjust][
            I.viewDrawer
            {drawer = Drawer.dismissibleDrawer {dismissibleDrawerConfig | open = config.drawerOpen
                                                                        , onClose = config.closeDrawer
                                                                        , additionalAttributes = [ style "z-index" "1" ]}
            , drawerTitle = text config.drawerTitle
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

        -- [ topAppBar topAppBarConfig
        --     [ TopAppBar.row []
        --         [ TopAppBar.section [ TopAppBar.alignStart ]
        --             [ Html.span [ TopAppBar.title ]
        --                 [ text "Title" ]
        --             ]
        --         ]
        --     ]
        --     , Drawer.modalDrawer
        --     { modalDrawerConfig
        --         | open = True
        --         , onClose = config.closeDrawer
        --         , additionalAttributes = []
        --     }
        --     [ Drawer.drawerContent [TopAppBar.fixedAdjust] [] ]
        -- , drawerScrim [] []
        -- , Html.div [Drawer.appContent] [ config.body ]
        -- ]
    [
    I.viewDrawer
        {drawer = Drawer.modalDrawer { modalDrawerConfig | open = config.drawerOpen
                                                         , onClose = config.closeDrawer}
        , drawerTitle = text config.drawerTitle
        , drawerSubtitle = config.drawerSubtitle
        , content = config.drawerContent
        } 
    , drawerScrim [] []
    , div [Drawer.appContent] [
        I.viewTopAppBar
            {topAppBar = topAppBar {topAppBarConfig | dense = False
                                                    , fixed = True}
            , navButton = Just {icon = config.navButtonIcon, message = config.navButtonCallback}
            , title = Maybe.withDefault "Portrait Tablet" config.title
            , search = Nothing
            , user = Nothing
            }
        ]
    , div [TopAppBar.fixedAdjust] [config.body]
    ]