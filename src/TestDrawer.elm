module TestDrawer exposing (main)

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
        ( config
        , list
        )
import Material.List.Item exposing (config, listItem)


main : Html.Html msg
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
