module Viewer.Handset exposing (viewLandscape, viewPortrait)

import Html exposing (Html, div, text)
import Material.Drawer.Modal as Drawer exposing (scrim)
import Material.List as MList
import Material.List.Item as MLItem
import Material.TopAppBar as TopAppBar
import Viewer.Internal as I


viewLandscape : I.ViewerConfig msg -> List (Html msg)
viewLandscape config =
    [ I.viewDrawer
        { drawer =
            Drawer.drawer
                (Drawer.config
                    |> Drawer.setOpen config.drawerOpen
                    |> Drawer.setOnClose config.closeDrawer
                )
        , drawerTitle = text config.drawerTitle
        , drawerSubtitle = config.drawerSubtitle
        , content = config.drawerContent
        }
    , scrim [] []
    , Drawer.content []
        [ I.viewTopAppBar
            { topAppBar =
                TopAppBar.short
                    (TopAppBar.config
                        |> TopAppBar.setDense True
                        |> TopAppBar.setFixed False
                    )
            , navButton = Just { icon = config.navButtonIcon, message = config.navButtonCallback }
            , title = Maybe.withDefault "Landscape Handset" config.title
            , search = Nothing
            , user = config.user
            , actions = config.actions
            }
        ]
    , div [ TopAppBar.denseFixedAdjust ] [ config.body ]
    ]


viewPortrait : I.ViewerConfig msg -> List (Html msg)
viewPortrait config =
    if config.drawerOpen then
        [ -- I.viewDrawer
          --     {drawer = Drawer.modalDrawer {modalDrawerConfig | open = config.drawerOpen
          --                                                     , onClose = config.closeDrawer
          --                                                     }
          --     , drawerTitle = config.drawerTitle
          --     , drawerSubtitle = config.drawerSubtitle
          --     , content = config.drawerContent
          --     }
          -- , drawerScrim [][]
          -- , div [Drawer.appContent][
          I.viewTopAppBar
            { topAppBar =
                TopAppBar.short
                    (TopAppBar.config
                        |> TopAppBar.setDense True
                        |> TopAppBar.setFixed False
                    )
            , navButton = Just { icon = "arrow_back", message = config.closeDrawer }
            , title = config.drawerTitle
            , search = Nothing
            , user = config.user
            , actions = config.actions
            }
        , div [ TopAppBar.denseFixedAdjust ]
            [ MList.list MList.config
                (MLItem.listItem MLItem.config [ text "Line item" ])
                [ MLItem.listItem MLItem.config [ text "Line item" ]
                ]

            --Html.h1 [Typography.headline6][config.drawerSubtitle]
            ]
        ]

    else
        [ I.viewTopAppBar
            { topAppBar =
                TopAppBar.short
                    (TopAppBar.config
                        |> TopAppBar.setDense True
                        |> TopAppBar.setFixed False
                    )
            , navButton = Just { icon = config.navButtonIcon, message = config.navButtonCallback }
            , title = Maybe.withDefault "Portrait Handset" config.title
            , search = Nothing
            , user = config.user
            , actions = config.actions
            }
        , div [ TopAppBar.denseFixedAdjust ] [ config.body ]
        ]
