module Viewer.Internal exposing (..)

import Html exposing (Html,text,div)
import Html.Attributes
import Time exposing (Posix)
import Identicon exposing (identicon)
import Material.Button as Button exposing (config)
import Material.TopAppBar as TopAppBar exposing (config)
import Material.IconButton as IconButton exposing (customIcon,iconButton, config)
import Material.TextField as TextField exposing (config)
import Material.TextField.Icon as TextFieldIcon
import Material.Icon as Icon exposing (icon)
import Material.Card as Card exposing (config, primaryAction)
import Material.Theme as Theme
import Material.Drawer.Modal as Drawer
import Material.Typography as Typography
import Html.Events

--
-- External
--

type alias ViewerConfig msg =
    {
        title : Maybe String,
        openDrawer : msg,
        body : Html msg,
        user : Maybe (Html Never),
        drawerOpen : Bool,
        closeDrawer : msg
        , drawerTitle : String
        , drawerSubtitle : Html msg
        , drawerContent : Html msg
        , navButtonIcon : String
        , navButtonCallback :  msg
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
        , user : Maybe (Html Never)
    }

type alias NavButtonConfig msg =
    {
        icon : String,
        message : msg
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
                        TextField.filled <|
                            (TextField.config
                            |> TextField.setTrailingIcon (Just <| TextFieldIcon.icon "search")
                            |> TextField.setValue (Just <| s.search)
                            |> TextField.setAttributes [Theme.surface]
                            |> TextField.setOnInput s.callback

                            -- { textFieldConfig
                            --     | trailingIcon = TextField.textFieldIcon iconConfig "search"
                            --     , value = s.search

                            --     --, outlined = True
                            --     , additionalAttributes = [ Theme.surface ]
                            --     , onInput = Just s.callback
                            -- }
                            )
                , case config.user of
                    Nothing ->
                        div [] []

                    Just s ->
                        IconButton.iconButton
                            (IconButton.config
                            |> IconButton.setAttributes [ TopAppBar.actionItem ] )
                            (IconButton.customIcon Html.i []
                            [s])
                        
                    
                        -- customIconButton
                        --     { iconButtonConfig | additionalAttributes = [ TopAppBar.actionItem ] }
                        --     [ s ]
                ]
            ]
        ]

navButton : NavButtonConfig msg -> Html msg
navButton config =
    IconButton.iconButton
        (
            IconButton.config
            |> IconButton.setAttributes [TopAppBar.navigationIcon]
            |> IconButton.setOnClick config.message
        )
        <| IconButton.icon config.icon
    -- iconButton
    --     { iconButtonConfig
    --         | additionalAttributes = [ TopAppBar.navigationIcon ]
    --         , onClick = config.message
    --     }
    --     config.icon

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
            [ Drawer.header []
                [Html.h3 [ Drawer.title ] [ config.drawerTitle]
                , Html.h6 [ Drawer.subtitle ] [ config.drawerSubtitle ]
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
        title= "",
        primaryAction = Nothing
    }

type alias CardConfig msg =
    {
        id : String,
        title: String,
        primaryAction : Maybe msg
    }

viewCard : CardConfig msg -> Html msg
viewCard config = 
    let
        primod x = case config.primaryAction of
                        Just pA ->
                            Card.primaryAction [Html.Events.onClick pA] x
                        Nothing ->
                            x
    in
        Card.card
        
            Card.config
            {
                blocks = primod [
                    Card.block <|
                        div 
                            [Html.Attributes.style "margin-left" "auto"
                            , Html.Attributes.style "margin-right" "auto"
                            , Html.Attributes.style "padding-top" "1rem"
                            , Html.Attributes.style "width" "25%"]
                            [identicon "100%" config.id]
                    , Card.block <|
                        Html.div [ Html.Attributes.style "padding" "1rem" ]
                        [ Html.h2
                            [ Typography.headline6
                            , Html.Attributes.style "margin" "0"
                            ]
                            [ text <| config.title ]
                        , Html.h3
                            [ Typography.subtitle2
                            , Theme.textSecondaryOnBackground
                            , Html.Attributes.style "margin" "0"
                            ]
                            [ text "Some interesting Subtitle" ]
                        ]
                    , Card.block <|
                        Html.div
                        [ Html.Attributes.style "padding" "0 1rem 0.5rem 1rem"
                        , Typography.body2
                        , Theme.textSecondaryOnBackground
                        ]
                        [ Html.p [] [ text "Description" ] ]
                ]
                , actions = Just <| Card.actions
                    { buttons =
                            [ Card.button Button.config
                                "Visit"
                            ]
                        , icons =
                            [ Card.icon IconButton.config
                                <| IconButton.icon "favorite"
                            ]
                        }

            }
    
    -- Card.card cardConfig
    --     { blocks = Card.cardPrimaryAction {cardPrimaryActionConfig | onClick = config.primaryAction}
    --         [ Card.cardBlock <|
    --             div [Html.Attributes.style "margin-left" "auto"
    --                 , Html.Attributes.style "margin-right" "auto"
    --                 , Html.Attributes.style "padding-top" "1rem"
    --                 , Html.Attributes.style "width" "25%"]
    --             [identicon "100%" config.id]
    --         , Card.cardBlock <|
    --             Html.div [ Html.Attributes.style "padding" "1rem" ]
    --                 [ Html.h2
    --                     [ Typography.headline6
    --                     , Html.Attributes.style "margin" "0"
    --                     ]
    --                     [ text <| "Coding: "++ config.id ]
    --                 , Html.h3
    --                     [ Typography.subtitle2
    --                     , Theme.textSecondaryOnBackground
    --                     , Html.Attributes.style "margin" "0"
    --                     ]
    --                     [ text "Some interesting Subtitle" ]
    --                 ]
    --         , Card.cardBlock <|
    --             Html.div
    --                 [ Html.Attributes.style "padding" "0 1rem 0.5rem 1rem"
    --                 , Typography.body2
    --                 , Theme.textSecondaryOnBackground
    --                 ]
    --                 [ Html.p [] [ text "Description" ] ]
    --         ]
    --     , actions =
    --         Just <|
    --             Card.cardActions
    --                 { buttons =
    --                     [ Card.cardActionButton buttonConfig
    --                         "Visit"
    --                     ]
    --                 , icons =
    --                     [ Card.cardActionIcon iconButtonConfig
    --                         "favorite"
    --                     ]
    --                 }
    --     }


