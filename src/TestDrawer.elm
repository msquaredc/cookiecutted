module TestDrawer exposing (main)

import Html exposing (text)
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
