module TestDrawer exposing (..)

import Html exposing (text)
import Html.Attributes exposing (style)
import Material.Drawer
    exposing
        ( drawerContent
        , permanentDrawer
        , permanentDrawerConfig
        , modalDrawer
        , modalDrawerConfig
        , dismissibleDrawer
        , dismissibleDrawerConfig
        , drawerScrim
        )
import Material.List
    exposing
        ( list
        , listConfig
        , listItem
        , listItemConfig
        )

main =
    Html.div []
    [ modalDrawer
        { modalDrawerConfig
            | open = True
            , onClose = Nothing
        }
        [ drawerContent [] [] ]
    , drawerScrim [] []
    , Html.div [] [ text "Main Content" ]
    ]