module Viewer.Tablet exposing (..)

import Html exposing (Html,text,div)
import Html.Attributes exposing (style)
import Viewer.Internal as I
import Material.TopAppBar as TopAppBar
import Material.Drawer.Dismissible as DDrawer
import Material.Drawer.Modal as MDrawer

viewLandscape : I.ViewerConfig msg -> List (Html msg)
viewLandscape config =
    [
        I.viewTopAppBar
            {topAppBar = TopAppBar.regular 
                (TopAppBar.config
                    |> TopAppBar.setDense True
                    |> TopAppBar.setFixed True)
            , navButton = Just {icon = config.navButtonIcon, message = config.navButtonCallback}
            , title = Maybe.withDefault "Landscape Tablet" config.title
            , search = Nothing
            , user = config.user
            },
        div [TopAppBar.denseFixedAdjust][
            I.viewDrawer
            {drawer = DDrawer.drawer 
                        (DDrawer.config 
                            |> DDrawer.setOpen config.drawerOpen
                            |> DDrawer.setOnClose config.closeDrawer
                            |> DDrawer.setAttributes [ style "z-index" "1" ])
            , drawerTitle = text config.drawerTitle
            , drawerSubtitle = config.drawerSubtitle
            , content = config.drawerContent
            }
            , DDrawer.content [] [
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
        {drawer = MDrawer.drawer (MDrawer.config
                                    |> MDrawer.setOpen config.drawerOpen
                                    |> MDrawer.setOnClose config.closeDrawer) 
        , drawerTitle = text config.drawerTitle
        , drawerSubtitle = config.drawerSubtitle
        , content = config.drawerContent
        } 
    , MDrawer.scrim [] []
    , MDrawer.content [] [
        I.viewTopAppBar
            {topAppBar = 
                TopAppBar.regular 
                (TopAppBar.config
                    |> TopAppBar.setDense True
                    |> TopAppBar.setFixed True)
            , navButton = Just {icon = config.navButtonIcon, message = config.navButtonCallback}
            , title = Maybe.withDefault "Portrait Tablet" config.title
            , search = Nothing
            , user = config.user
            }
        ]
    , div [TopAppBar.fixedAdjust] [config.body]
    ]