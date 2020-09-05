module Viewer.EditableText exposing (..)

import Html exposing (Html, Attribute)
import Material.TextField as TextField
import Html exposing (div)
import Html.Events exposing (onClick)
import Msg exposing (EditableTextMsg)

type alias Config msg =
    { active : Bool
    , activator : msg
    , deactivator : String -> msg
    , callback : String -> msg
    }


text : Config msg -> List (Attribute msg) -> String -> Html msg
text c attributes value =
    if c.active then
        -- list {listConfig | nonInteractive = True}
        --     [ listItem listItemConfig
        --         [
        TextField.outlined
            (TextField.config
                |> TextField.setValue (Just value)
                |> TextField.setOnInput c.callback
                |> TextField.setOnChange c.deactivator
                |> TextField.setLabel Nothing
                -- , fullwidth = True
                |> TextField.setAttributes attributes
            )
        --     ]
        -- ]

    else
        -- list {listConfig | nonInteractive = True }
        --     [ listItem {listItemConfig | onClick = Just activator}
        --         [
        div ((onClick <| c.activator) :: attributes) [ Html.text value ]