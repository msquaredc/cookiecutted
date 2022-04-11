module TestDrawer exposing (..)

import Html exposing (text)
import Html.Attributes exposing (style)
import Material.Drawer.Modal as ModalDrawer
    exposing
        ( content
        --, permanentDrawer
        --, permanentDrawerConfig
        --, modalDrawer
        --, modalDrawerConfig
        , drawer
        --, drawerScrim
        )

import Material.List
    exposing
        ( list
        , config
        )
import Material.List.Item exposing (listItem, config)
main =
    Html.div []
    [ drawer ModalDrawer.config
        -- { 
        --     | open = True
        --     , onClose = Nothing
        -- }
        [ content [] [] ]
    , ModalDrawer.scrim [] []
    , Html.div [] [ text "Main Content" ]
    ]