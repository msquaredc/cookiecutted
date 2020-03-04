module Viewer.Internal exposing (..)

import Html exposing (Html,text,div)
import Html.Attributes
import Identicon exposing (identicon)
import Material.Button as Button exposing (buttonConfig)
import Material.TopAppBar as TopAppBar exposing (topAppBar)
import Material.IconButton as IconButton exposing (customIconButton,iconButton, iconButtonConfig)
import Material.TextField as TextField exposing (textFieldConfig)
import Material.Icon as Icon exposing (iconConfig)
import Material.Card as Card exposing (cardConfig, cardPrimaryActionConfig)
import Material.Theme as Theme
import Material.Drawer as Drawer
import Material.Typography as Typography

--
-- External
--

type alias ViewerConfig msg =
    {
        title : Maybe String,
        openDrawer : Maybe msg,
        body : Html msg,
        user : Maybe (Html msg),
        drawerOpen : Bool,
        closeDrawer : Maybe msg
        , drawerTitle : Html msg
        , drawerSubtitle : Html msg
        , drawerContent : Html msg
        , navButtonIcon : String
        , navButtonCallback : Maybe msg
    }

--
-- TOP APP BAR
--
type alias TopAppBarConfig msg =
    {
        topAppBar : List (Html msg) -> Html msg
        , navButton : Maybe (NavButtonConfig msg)
        , title : String
        , search : Maybe (SearchConfig msg)
        , user : Maybe (Html msg)
    }

type alias NavButtonConfig msg =
    {
        icon : String,
        message : Maybe msg
    }

type alias SearchConfig msg =
    {
        search : String,
        callback : String -> msg
    }

viewTopAppBar : TopAppBarConfig msg -> Html msg
viewTopAppBar config =

    config.topAppBar
        [ TopAppBar.row []
            [
                TopAppBar.section [ TopAppBar.alignStart ]
                    [ Maybe.map (navButton) config.navButton
                      |> Maybe.withDefault (div [][])
                    , Html.span
                        [ TopAppBar.title

                        --, Html.Attributes.style "text-transform" "uppercase"
                        --, Html.Attributes.style "font-weight" "400"
                        --, Typography.headline5
                        ]
                        [ text config.title ]
                    ]
            , TopAppBar.section [ TopAppBar.alignEnd ]
                [ case config.search of
                    Nothing ->
                        div [] []

                    Just s ->
                        TextField.textField
                            { textFieldConfig
                                | trailingIcon = TextField.textFieldIcon iconConfig "search"
                                , value = s.search

                                --, outlined = True
                                , additionalAttributes = [ Theme.surface ]
                                , onInput = Just s.callback
                            }
                , case config.user of
                    Nothing ->
                        div [] []

                    Just s ->
                        customIconButton
                            { iconButtonConfig | additionalAttributes = [ TopAppBar.actionItem ] }
                            [ s ]
                ]
            ]
        ]

navButton : NavButtonConfig msg -> Html msg
navButton config =
    iconButton
        { iconButtonConfig
            | additionalAttributes = [ TopAppBar.navigationIcon ]
            , onClick = config.message
        }
        config.icon

--
-- DRAWER
--
type alias DrawerConfig msg =
    {
        drawer : List (Html msg) -> Html msg
        , drawerTitle : Html msg
        , drawerSubtitle : Html msg
        , content : Html msg
    }

viewDrawer : DrawerConfig msg -> Html msg
viewDrawer config  =
    config.drawer
        -- Drawer.dismissibleDrawer
        --     { dismissibleDrawerConfig
        --         | open = config.drawerOpen
        --         , onClose = Just (Msg.Viewer Msg.CloseDrawer)
        --         , additionalAttributes =
        --             [ TopAppBar.fixedAdjust
        --             ]
        --     }
            [ Drawer.drawerHeader []
                [Html.h3 [ Drawer.drawerTitle ] [ config.drawerTitle]
                , Html.h6 [ Drawer.drawerSubtitle ] [ config.drawerSubtitle ]
                ]
                -- drawerHeader []
                -- [ Maybe.map (identicon "100%") detail.user
                --     |> Maybe.withDefault (div [] [])
                -- ]
            , config.content
            -- drawerContent [] []
            ]
        -- , div [ Drawer.appContent, Typography.typography ]
        --     [body]

--
-- CARD
-- 
defaultCardConfig : CardConfig msg
defaultCardConfig = 
    {
        id = "",
        primaryAction = Nothing
    }

type alias CardConfig msg =
    {
        id : String,
        primaryAction : Maybe msg
    }

viewCard : CardConfig msg -> Html msg
viewCard config = 
    Card.card cardConfig
        { blocks = Card.cardPrimaryAction {cardPrimaryActionConfig | onClick = config.primaryAction}
            [ Card.cardBlock <|
                div [Html.Attributes.style "margin-left" "auto"
                    , Html.Attributes.style "margin-right" "auto"
                    , Html.Attributes.style "padding-top" "1rem"
                    , Html.Attributes.style "width" "25%"]
                [identicon "100%" config.id]
            , Card.cardBlock <|
                Html.div [ Html.Attributes.style "padding" "1rem" ]
                    [ Html.h2
                        [ Typography.headline6
                        , Html.Attributes.style "margin" "0"
                        ]
                        [ text <| "Coding: "++ config.id ]
                    , Html.h3
                        [ Typography.subtitle2
                        , Theme.textSecondaryOnBackground
                        , Html.Attributes.style "margin" "0"
                        ]
                        [ text "Some interesting Subtitle" ]
                    ]
            , Card.cardBlock <|
                Html.div
                    [ Html.Attributes.style "padding" "0 1rem 0.5rem 1rem"
                    , Typography.body2
                    , Theme.textSecondaryOnBackground
                    ]
                    [ Html.p [] [ text "Description" ] ]
            ]
        , actions =
            Just <|
                Card.cardActions
                    { buttons =
                        [ Card.cardActionButton buttonConfig
                            "Visit"
                        ]
                    , icons =
                        [ Card.cardActionIcon iconButtonConfig
                            "favorite"
                        ]
                    }
        }