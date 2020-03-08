module Viewer exposing (Details, Header, detailsConfig, header, notFound, textForm, update, view, wideTextForm)

--import Url.Builder

import Browser
import Device
import Dict
import Html exposing (Html, a, div, h1, h3, p, text)
import Html.Attributes exposing (class, href, style)
import Html.Events
import DateFormat.Relative exposing (relativeTime)
import Identicon exposing (identicon)
import Material.Button as Button exposing (buttonConfig, textButton, unelevatedButton)
import Material.Dialog exposing (dialog, dialogConfig)
import Material.Drawer as Drawer exposing (dismissibleDrawerConfig, drawerContent, drawerHeader)
import Material.Icon exposing (icon, iconConfig)
import Material.IconButton exposing (customIconButton, iconButton, iconButtonConfig)
import Material.LayoutGrid exposing (layoutGridCell)
import Material.List as ListItem exposing (list, listConfig, listGroupSubheader, listItem, listItemPrimaryText, listItemSecondaryText, listItemConfig, listItemDivider, listItemDividerConfig, listItemGraphic, listItemText)
import Material.TextField as TextField exposing (textFieldConfig)
import Material.Theme as Theme
import Material.TopAppBar as TopAppBar exposing (topAppBar, topAppBarConfig)
import Material.Typography as Typography
import Msg exposing (ViewerMsg(..))
import Session
import Time exposing (Posix)
import Type.Database as Db
import Type.Database.TypeMatching as Match
import Type.IO.Form as Form
import Type.IO.Setter as Updater
import Utils
import Viewer.Desktop as Desktop
import Viewer.Handset as Handset
import Viewer.Tablet as Tablet



{-
   This module builds the view. Every individual page's view function returns a Viewer.Details,
   which is used to generate a Browser.Document msg in this module
-}


type alias Details msg =
    { title : String
    , body : Maybe Posix -> List (Html msg)
    , search : Maybe String
    , user : Maybe String
    , top : Bool
    }


type alias Header =
    { drawerOpen : Bool
    , new_username : String
    }



-- UPDATE


update : ViewerMsg -> Header -> Header
update msg model =
    case msg of
        OpenDrawer ->
            { model | drawerOpen = True }

        CloseDrawer ->
            { model | drawerOpen = False }

        NewUsername text ->
            { model | new_username = text }



-- Search s ->
--     { model | search = Just s }
-- VIEW


toggleDrawer : Bool -> Msg.Msg
toggleDrawer drawerOpen =
    Msg.Viewer <|
        if drawerOpen then
            Msg.CloseDrawer

        else
            Msg.OpenDrawer


view : Session.Session -> (a -> Msg.Msg) -> Details Msg.Msg -> Header -> Maybe Posix -> Browser.Document Msg.Msg
view session msg details h time =
    { title = details.title ++ Utils.genericTitle
    , body =
        let
            device =
                Device.fromPixel session.windowSize.width session.windowSize.height
        in
        case session.user of
            Just _ ->
                (case ( device.device, device.orientation ) of
                    ( Device.Desktop, Device.Portrait ) ->
                        Desktop.viewPortrait

                    ( Device.Desktop, Device.Landscape ) ->
                        Desktop.viewLandscape

                    ( Device.Handset, Device.Portrait ) ->
                        Handset.viewPortrait

                    ( Device.Handset, Device.Landscape ) ->
                        Handset.viewLandscape

                    ( Device.Tablet, Device.Portrait ) ->
                        Tablet.viewPortrait

                    ( Device.Tablet, Device.Landscape ) ->
                        Tablet.viewLandscape
                )
                    { title = Nothing --Just details.title
                    , body = div [] <| details.body time
                    , openDrawer = Just (Msg.Viewer OpenDrawer)
                    , user = Maybe.map text session.user
                    , closeDrawer = Just (Msg.Viewer CloseDrawer)
                    , drawerOpen = h.drawerOpen
                    , drawerTitle = "User"
                    , drawerSubtitle = text <| "ID: " ++ Maybe.withDefault "" session.user
                    , drawerContent = viewDrawerContent 0
                    , navButtonIcon =
                        if details.top then
                            "menu"

                        else
                            "arrow_back"
                    , navButtonCallback =
                        Just <|
                            if details.top then
                                toggleDrawer h.drawerOpen

                            else
                                Msg.Back
                    }

            Nothing ->
                [ userDialog
                    True
                    (Dict.toList session.db.users
                        |> List.map (\( x, y ) -> ( x, y.value ))
                    )
                    h.new_username
                    time

                --    layoutGrid [] <|
                --     selectUser <|
                --         Match.keys Db.UserType session.db
                ]

    {- [
           if session.windowSize.height > session.windowSize.width then
               -- portrait mode

           else
               -- landscape mode
       ]
    -}
    -- [TestDrawer.main]
    --[Html.map Msg.Viewer (viewAll h details.title)]
    {- [ viewHeader2 h details

       --, Utils.logo 256
       , div
           [ TopAppBar.fixedAdjust

           --    , class "container"
           --    , class "main"
           --                , style "height" (String.fromInt (session.windowSize.height - headerHeight - footerHeight) ++ "px")
           ]
           [ viewDrawer h details ]
       ]
    -}
    }



-- HEADER
-- This header is statically generated. You'd likely want this to be dynamic in some way based on the active page & session
-- You could create an additional field in Viewer.Details for a header, and create this header in Main.elm or in each individual page's view
{- viewHeader : Html msg
   viewHeader =
       div [ class "header", class "container" ]
           [ div [ class "logo" ] [ viewLogo ]
           , div [ class "nav-links" ]
               [ a [ href "/" ] [ text "Home" ]
               , a [ href "/pageone" ] [ text "Page One" ]
               , a [ href "/pagewithsubpage/hello" ] [ text "Page With Subpage" ]

               --    , a [ href "newpage" ] [ text "New Page" ]
               ]
           ]
-}


viewDrawerContent : Int -> Html Msg.Msg
viewDrawerContent selectedIndex =
    let
        listItemConfig_ index =
            { listItemConfig
                | activated = selectedIndex == index
                , onClick = Nothing
            }
    in
    list listConfig
        [ listItem (listItemConfig_ 0)
            [ listItemGraphic [] [ icon iconConfig "home" ]
            , text "Home"
            ]
        , listItem (listItemConfig_ 1)
            [ listItemGraphic [] [ icon iconConfig "local_library" ]
            , text "Research"
            ]
        , listItem (listItemConfig_ 2)
            [ listItemGraphic [] [ icon iconConfig "ballot" ]
            , text "Coding"
            ]

        -- , listItem (listItemConfig_ 3)
        --     [ listItemGraphic [] [ icon iconConfig "drafts" ]
        --     , text "Drafts"
        --     ]
        , listItemDivider listItemDividerConfig
        , listGroupSubheader [] [ text "Favorites" ]
        , listItem (listItemConfig_ 4)
            [ listItemGraphic [] [ icon iconConfig "bookmark" ]
            , text "Family"
            ]
        , listItem (listItemConfig_ 5)
            [ listItemGraphic [] [ icon iconConfig "bookmark" ]
            , text "Friends"
            ]
        , listItem (listItemConfig_ 6)
            [ listItemGraphic [] [ icon iconConfig "bookmark" ]
            , text "Work"
            ]
        , listItemDivider listItemDividerConfig
        , listItem (listItemConfig_ 7)
            [ listItemGraphic [] [ icon iconConfig "settings" ]
            , text "Settings"
            ]
        , listItem (listItemConfig_ 8)
            [ listItemGraphic [] [ icon iconConfig "announcement" ]
            , text "Help & feedback"
            ]
        ]


viewHeader2 : Header -> Details Msg.Msg -> Html Msg.Msg
viewHeader2 config details =
    topAppBar { topAppBarConfig | fixed = True }
        [ TopAppBar.row []
            [ TopAppBar.section [ TopAppBar.alignStart ]
                [ iconButton
                    { iconButtonConfig
                        | additionalAttributes = [ TopAppBar.navigationIcon ]
                        , onClick = Just <| toggleDrawer config.drawerOpen
                    }
                    "menu"
                , Html.span
                    [ TopAppBar.title

                    --, Html.Attributes.style "text-transform" "uppercase"
                    --, Html.Attributes.style "font-weight" "400"
                    --, Typography.headline5
                    ]
                    [ text details.title ]
                ]
            , TopAppBar.section [ TopAppBar.alignEnd ]
                [ case details.search of
                    Nothing ->
                        div [] []

                    Just s ->
                        TextField.textField
                            { textFieldConfig
                                | trailingIcon = TextField.textFieldIcon iconConfig "search"
                                , value = s

                                --, outlined = True
                                , additionalAttributes = [ Theme.surface ]
                                , onInput = Just Msg.Search
                            }
                , case details.user of
                    Nothing ->
                        div [] []

                    Just s ->
                        customIconButton
                            { iconButtonConfig | additionalAttributes = [ TopAppBar.actionItem ] }
                            [ identicon "100%" s ]
                ]
            ]
        ]



-- FOOTER


viewFooter : Html msg
viewFooter =
    div [ class "footer", class "container" ]
        [ text "A simple, no-frills boilerplate for creating delightful Single Page Applications (SPAs) in Elm."
        , a [ href "https://github.com/jzxhuang/elm-spa-boilerplate" ] [ text "Check it out on Github!" ]
        , text "© 2018 - present Jeffrey Huang."
        ]



-- 404 PAGE (NotFound)


notFound : Details msg
notFound =
    { detailsConfig
        | title = "Page Not Found"
        , body = \_ -> 
            [ div [ class "not-found" ]
                [ div [ style "font-size" "12em" ] [ text "404" ]
                , h1 [ style "font-size" "3.5em" ] [ text "Page Not Found" ]
                , h3 [ style "font-size" "1.5em" ]
                    [ text "Oops - Looks like you got lost or clicked a bad link! "
                    , a [ href "/" ] [ text "Click here " ]
                    , text "to go back to the home page."
                    ]
                ]
            ]
    }


detailsConfig : Details msg
detailsConfig =
    { title = ""
    , body = \_ -> []
    , search = Nothing
    , user = Nothing
    , top = False
    }


header : Header
header =
    { drawerOpen = False
    , new_username = ""

    --, search = Nothing
    }


-- viewDrawer : Header -> Details Msg.Msg -> Html.Html Msg.Msg
-- viewDrawer config detail =
--     div demoPanel
--         [ Drawer.dismissibleDrawer
--             { dismissibleDrawerConfig
--                 | open = config.drawerOpen
--                 , onClose = Just (Msg.Viewer Msg.CloseDrawer)
--                 , additionalAttributes =
--                     [ TopAppBar.fixedAdjust
--                     ]
--             }
--             [ drawerHeader []
--                 [ Maybe.map (identicon "100%") detail.user
--                     |> Maybe.withDefault (div [] [])
--                 ]
--             , drawerContent [] []
--             ]
--         , div [ Drawer.appContent, Typography.typography ]
--             detail.body
--         ]



-- LOGO
-- viewLogo : Html msg
-- viewLogo =
--     a [ href "/", style "text-decoration" "none" ] [ Utils.logo 32 ]
-- STYLING HELPERS (lazy, hard-coded styling)


headerHeight : Int
headerHeight =
    60


footerHeight : Int
footerHeight =
    60


catalogPageContainer : List (Html.Attribute msg)
catalogPageContainer =
    [ Html.Attributes.style "position" "relative"
    , Typography.typography
    ]


demoPanel : List (Html.Attribute msg)
demoPanel =
    [ Html.Attributes.style "display" "-ms-flexbox"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "position" "relative"
    , Html.Attributes.style "height" "100vh"
    , Html.Attributes.style "overflow" "hidden"
    ]


demoContent : List (Html.Attribute msg)
demoContent =
    [ Html.Attributes.id "demo-content"
    , Html.Attributes.style "height" "100%"
    , Html.Attributes.style "-webkit-box-sizing" "border-box"
    , Html.Attributes.style "box-sizing" "border-box"
    , Html.Attributes.style "max-width" "100%"
    , Html.Attributes.style "padding-left" "16px"
    , Html.Attributes.style "padding-right" "16px"
    , Html.Attributes.style "padding-bottom" "100px"
    , Html.Attributes.style "width" "100%"
    , Html.Attributes.style "overflow" "auto"
    , Html.Attributes.style "display" "-ms-flexbox"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "-ms-flex-direction" "column"
    , Html.Attributes.style "flex-direction" "column"
    , Html.Attributes.style "-ms-flex-align" "center"
    , Html.Attributes.style "align-items" "center"
    , Html.Attributes.style "-ms-flex-pack" "start"
    , Html.Attributes.style "justify-content" "flex-start"
    ]


demoContentTransition : List (Html.Attribute msg)
demoContentTransition =
    [ Html.Attributes.style "max-width" "900px"
    , Html.Attributes.style "width" "100%"
    ]


demoTitle : List (Html.Attribute msg)
demoTitle =
    [ Html.Attributes.style "border-bottom" "1px solid rgba(0,0,0,.87)"
    ]



--
-- FORM FUNCTORS
--


textForm : Maybe String -> Form.FormFunctor msg
textForm label value callback =
    TextField.textField
        { textFieldConfig
            | value = value
            , onInput = Just callback
            , label = label
            , outlined = True
        }


wideTextForm : Maybe String -> Form.FormFunctor msg
wideTextForm label value callback =
    TextField.textField
        { textFieldConfig
            | value = value
            , onInput = Just callback
            , label = label
            , fullwidth = True
        }


selectUser : List String -> List (Html Msg.Msg)
selectUser users =
    if List.length users > 0 then
        if List.length users == 1 then
            [ List.head users
                |> Maybe.withDefault ""
                |> (\x -> layoutGridCell [] [ text <| "i have a user: " ++ x ])
            ]

        else
            [ Html.h2 [ Typography.headline6 ] [ text "Please choose your account:" ]
            , layoutGridCell []
                [ ListItem.list listConfig <|
                    List.map (\user -> ListItem.listItem { listItemConfig | onClick = Just (Msg.SetUser user) } [ ListItem.listItemGraphic [] [ identicon "100%" user ], text user ]) users
                ]
            ]

    else
        [ layoutGridCell []
            [ p []
                [ text "Looks like this is the first time you're using msquaredc!"
                ]
            , Button.textButton
                { buttonConfig | onClick = Just (Msg.CRUD (Msg.CreateRandom Db.UserType [])) }
                "Let's go!"
            ]
        ]


userDialog : Bool -> List ( String, Db.User ) -> String -> Maybe Posix -> Html Msg.Msg
userDialog open users new_username time =
    let
        addUserWithName username =
            Msg.CRUD
                (Msg.CreateRandom Db.UserType
                    [ \x ->
                        Match.setField
                            { kind = Db.UserType
                            , attribute = "name"
                            , setter = Updater.MaybeSetMsg << Just << Updater.StringMsg
                            , id = x
                            , value = username
                            }
                    ]
                )
    in
    dialog
        { dialogConfig
            | open = open
            , onClose = Nothing
        }
        { title = Just "Select an account"
        , content =
            if List.length users > 0 then
                [ list { listConfig | avatarList = True } <|
                    List.indexedMap
                        (\index ( id, user ) ->
                            listItem
                                { listItemConfig
                                    | onClick = Just (Msg.SetUser id)
                                    , activated = index == 0
                                    , selected = False
                                    , additionalAttributes =
                                        [ Html.Attributes.tabindex 0

                                        -- , Html.Events.onClick (Msg.Top (Msg.SetUser id))
                                        ]
                                }
                                [ listItemGraphic
                                    (userIdenticonIcon id).attributes
                                    (userIdenticonIcon id).elements
                                , listItemText []
                                    [ listItemPrimaryText []
                                        [ text <| Maybe.withDefault id user.name
                                        ]
                                    , listItemSecondaryText []
                                        [ text <| "Last login " ++ ( Maybe.withDefault "" <| Maybe.map (\x -> relativeTime x (Time.millisToPosix user.last_login)) time) ]
                                    ]
                                ]
                        )
                    <|
                        List.reverse <|
                            List.sortBy (\( _, b ) -> b.last_login) users
                ]

            else
                []
        , actions =
            [ list
                { listConfig
                  --                    | avatarList = True
                    | nonInteractive = True
                }
                [ listItem
                    { listItemConfig
                        | additionalAttributes =
                            [ Html.Attributes.tabindex 0

                            --    , Html.Events.onClick Close
                            ]
                    }
                    [ listItemText []
                        [ TextField.textField
                            { textFieldConfig
                                | onInput = Just <| Msg.Viewer << NewUsername
                                , label = Just "Add new User"
                                , onChange = Just addUserWithName
                                , value = new_username
                            }
                        ]
                    , unelevatedButton
                        { buttonConfig
                            | additionalAttributes =
                                [ Html.Attributes.style "margin-left" "16px"
                                ]
                            , icon = Just "add"
                        }
                        "add"
                    ]
                ]
            ]
        }


type alias HtmlElement msg =
    { attributes : List (Html.Attribute msg)
    , elements : List (Html msg)
    }


userIdenticonIcon : String -> HtmlElement msg
userIdenticonIcon id =
    { attributes =
        [ Html.Attributes.style "background-color" "rgba(0,0,0,.1)"
        , Html.Attributes.style "color" "#fff"
        ]
    , elements = [ identicon "66%" id ]
    }
