module Viewer.Desktop exposing (viewLandscape, viewPortrait)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Material.Drawer.Permanent as Drawer
import Material.TopAppBar as TopAppBar exposing (prominent)
import Viewer.Internal as I


viewLandscape : I.ViewerConfig msg -> List (Html msg)
viewLandscape config =
    [ I.viewTopAppBar
        { topAppBar =
            prominent
                (TopAppBar.config
                    |> TopAppBar.setDense False
                    |> TopAppBar.setFixed True
                )
        , navButton = Just { icon = config.navButtonIcon, message = config.navButtonCallback }
        , title = Maybe.withDefault "Landscape Desktop" config.title
        , search = Nothing
        , user = config.user
        , actions = config.actions
        }
    , div
        [ TopAppBar.prominentFixedAdjust
        , style "display" "flex"
        , style "flex-flow" "row nowrap"
        ]
        [ I.viewDrawer
            { drawer =
                Drawer.drawer
                    (Drawer.config |> Drawer.setAttributes [ style "z-index" "1" ])
            , drawerTitle = text config.drawerTitle
            , drawerSubtitle = config.drawerSubtitle
            , content = config.drawerContent
            }
        , div [] [ config.body ]
        ]

    --, Drawer.content [][ config.body ]
    ]


viewPortrait : I.ViewerConfig msg -> List (Html msg)
viewPortrait config =
    [ I.viewTopAppBar
        { topAppBar =
            prominent
                (TopAppBar.config
                    |> TopAppBar.setDense False
                    |> TopAppBar.setFixed True
                )
        , navButton = Just { icon = config.navButtonIcon, message = config.navButtonCallback }
        , title = Maybe.withDefault "Portrait Desktop" config.title
        , search = Nothing
        , user = config.user
        , actions = config.actions
        }
    , div [ TopAppBar.prominentFixedAdjust ]
        [ I.viewDrawer
            { drawer = Drawer.drawer (Drawer.config |> Drawer.setAttributes [ style "z-index" "1" ])
            , drawerTitle = text config.drawerTitle
            , drawerSubtitle = config.drawerSubtitle
            , content = config.drawerContent
            }
        ]
    , Drawer.content [] [ config.body ]
    ]
