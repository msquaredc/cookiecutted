module Viewer exposing (Details, Header, detailsConfig, header, notFound, textForm, update, view, wideTextForm, system)

--import Url.Builder

import Browser
import DateFormat.Relative exposing (relativeTime)
import Device
import Dict
import Html exposing (Html, a, div, h1, h3, p, text)
import Html.Attributes exposing (class, href, style)
import Identicon exposing (identicon)
import Material.Button as Button exposing (text, unelevated)
import Material.Dialog as Dialog exposing (config, dialog)
import Material.Drawer.Dismissible exposing (config, header)
import Material.Icon as Icon exposing (icon)
import Material.IconButton as IconButton exposing (config, customIcon, iconButton)
import Material.LayoutGrid as LayoutGrid exposing (cell)
import Material.List as MList exposing (config, list)
import Material.List.Divider as MLDivider exposing (config, listItem)
import Material.List.Item as MLItem exposing (config, graphic, listItem, text)
import Material.Snackbar as Snackbar
import Material.TextField as TextField exposing (config)
import Material.TextField.Icon as TextFieldIcon
import Material.Theme as Theme
import Material.TopAppBar as TopAppBar exposing (config, regular)
import Material.Typography as Typography
import Msg exposing (ViewerMsg(..))
import Session
import Time exposing (Posix)
import Type.Database as Db
import Type.Database.TypeMatching as Match
import Type.IO.Form as Form
import Type.IO.Setter as Updater
import Type.IO.Internal as Id exposing (Id, box, unbox)
import Utils
import Viewer.Desktop as Desktop
import Viewer.Handset as Handset
import Viewer.Tablet as Tablet
import DnDList



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
    , actions : List (String, msg)
    }


type alias Header =
    { drawerOpen : Bool
    , new_username : String
    , queue : Snackbar.Queue Msg.Msg
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


viewSnackbar : Header -> Html Msg.Msg
viewSnackbar h =
    Snackbar.snackbar
        (Snackbar.config { onClosed = Msg.SnackbarClosed }
        |> Snackbar.setCloseOnEscape True)
        h.queue


view : Session.Session -> (a -> Msg.Msg) -> Details Msg.Msg -> Header -> Maybe Posix -> Browser.Document Msg.Msg
view session msg details h time =
    { title = details.title ++ Utils.genericTitle
    , body =
        viewSnackbar h
            :: (let
                    device =
                        Device.fromPixel session.windowSize.width session.windowSize.height
                in
                case session.user of
                    Just userid ->
                        let
                            username =
                                Dict.get userid session.db.users
                                    |> Maybe.map .value
                                    |> Maybe.andThen .name
                        in
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
                            { title = Just details.title --Nothing
                            , body = div [] <| details.body time
                            , openDrawer = Msg.Viewer OpenDrawer
                            , user = Maybe.map (identicon "100%") session.user
                            , closeDrawer = Msg.Viewer CloseDrawer
                            , drawerOpen = h.drawerOpen
                            , drawerTitle = Maybe.withDefault "User" username
                            , drawerSubtitle = Html.text <| "ID: " ++ userid
                            , drawerContent = viewDrawerContent 0
                            , navButtonIcon =
                                if details.top then
                                    "menu"

                                else
                                    "arrow_back"
                            , navButtonCallback =
                                if details.top then
                                    toggleDrawer h.drawerOpen

                                else
                                    Msg.Back
                            , actions = details.actions
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
               )

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
            MLItem.config
                |> MLItem.setSelected
                    (if index == selectedIndex then
                        Just MLItem.selected

                     else
                        Nothing
                    )

        -- selected == activated
    in
    list MList.config
        (MLItem.listItem (listItemConfig_ 0)
            [ MLItem.graphic [] [ Icon.icon [] "home" ]
            , Html.text "Home"
            ]
        )
        [ MLItem.listItem (listItemConfig_ 1)
            [ MLItem.graphic [] [ Icon.icon [] "local_library" ]
            , Html.text "Research"
            ]
        , MLItem.listItem (listItemConfig_ 2)
            [ MLItem.graphic [] [ Icon.icon [] "ballot" ]
            , Html.text "Coding"
            ]

        -- , listItem (listItemConfig_ 3)
        --     [ listItemGraphic [] [ icon iconConfig "drafts" ]
        --     , text "Drafts"
        --     ]
        , MLDivider.listItem MLDivider.config

        --, MList.group [] [ Html.text "Favorites" ] TODO: Uncomment
        , MLItem.listItem (listItemConfig_ 4)
            [ MLItem.graphic [] [ icon [] "bookmark" ]
            , Html.text "Family"
            ]
        , MLItem.listItem (listItemConfig_ 5)
            [ MLItem.graphic [] [ icon [] "bookmark" ]
            , Html.text "Friends"
            ]
        , MLItem.listItem (listItemConfig_ 6)
            [ MLItem.graphic [] [ icon [] "bookmark" ]
            , Html.text "Work"
            ]
        , MLDivider.listItem MLDivider.config
        , MLItem.listItem (listItemConfig_ 7)
            [ MLItem.graphic [] [ icon [] "settings" ]
            , Html.text "Settings"
            ]
        , MLItem.listItem (listItemConfig_ 8)
            [ MLItem.graphic [] [ icon [] "announcement" ]
            , Html.text "Help & feedback"
            ]
        ]


viewHeader2 : Header -> Details Msg.Msg -> Html Msg.Msg
viewHeader2 config details =
    regular
        (TopAppBar.config
            |> TopAppBar.setFixed True
        )
        [ TopAppBar.row []
            [ TopAppBar.section [ TopAppBar.alignStart ]
                [ iconButton
                    (IconButton.config
                        |> IconButton.setAttributes [ TopAppBar.navigationIcon ]
                        |> IconButton.setOnClick (toggleDrawer config.drawerOpen)
                    )
                  <|
                    IconButton.icon "menu"
                , Html.span
                    [ TopAppBar.title

                    --, Html.Attributes.style "text-transform" "uppercase"
                    --, Html.Attributes.style "font-weight" "400"
                    --, Typography.headline5
                    ]
                    [ Html.text details.title ]
                ]
            , TopAppBar.section [ TopAppBar.alignEnd ]
                [ case details.search of
                    Nothing ->
                        div [] []

                    Just s ->
                        TextField.filled
                            (TextField.config
                                |> TextField.setTrailingIcon (Just <| TextFieldIcon.icon "search")
                                |> TextField.setValue (Just s)
                                |> TextField.setAttributes [ Theme.surface ]
                                |> TextField.setOnInput Msg.Search
                            )
                , case details.user of
                    Nothing ->
                        div [] []

                    Just s ->
                        IconButton.iconButton
                            (IconButton.config |> IconButton.setAttributes [ TopAppBar.actionItem ])
                        <|
                            IconButton.customIcon Html.i
                                []
                                [ identicon "100%" s ]
                ]
            ]
        ]



-- FOOTER


viewFooter : Html msg
viewFooter =
    div [ class "footer", class "container" ]
        [ Html.text "A simple, no-frills boilerplate for creating delightful Single Page Applications (SPAs) in Elm."
        , a [ href "https://github.com/jzxhuang/elm-spa-boilerplate" ] [ Html.text "Check it out on Github!" ]
        , Html.text "© 2018 - present Jeffrey Huang."
        ]



-- 404 PAGE (NotFound)


notFound : Details msg
notFound =
    { detailsConfig
        | title = "Page Not Found"
        , body =
            \_ ->
                [ div [ class "not-found" ]
                    [ div [ style "font-size" "12em" ] [ Html.text "404" ]
                    , h1 [ style "font-size" "3.5em" ] [ Html.text "Page Not Found" ]
                    , h3 [ style "font-size" "1.5em" ]
                        [ Html.text "Oops - Looks like you got lost or clicked a bad link! "
                        , a [ href "/" ] [ Html.text "Click here " ]
                        , Html.text "to go back to the home page."
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
    , actions = []
    }

system : DnDList.System a Msg.Msg
system =
    DnDList.create 
        { beforeUpdate = \_ _ list -> list
        , movement = DnDList.Vertical
        , listen = DnDList.OnDrag
        , operation = DnDList.Rotate
        } 
        Msg.DnDEvent

header : Header
header =
    { drawerOpen = False
    , new_username = ""
    , queue = Snackbar.initialQueue
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
    TextField.filled
        (TextField.config
            |> TextField.setValue (Just value)
            |> TextField.setOnInput callback
            |> TextField.setLabel label
         --|> TextField.outlined True TODO: Uncomment
        )


wideTextForm : Maybe String -> Form.FormFunctor msg
wideTextForm label value callback =
    TextField.filled
        (TextField.config
            |> TextField.setValue (Just value)
            |> TextField.setOnInput callback
            |> TextField.setLabel label
            --|> TextField.outlined True TODO: Uncomment
            |> TextField.setFullwidth True
        )


selectUser : List String -> List (Html Msg.Msg)
selectUser users =
    if List.length users > 0 then
        if List.length users == 1 then
            [ List.head users
                |> Maybe.withDefault ""
                |> (\x -> LayoutGrid.cell [] [ Html.text <| "i have a user: " ++ x ])
            ]

        else
            [ Html.h2 [ Typography.headline6 ] [ Html.text "Please choose your account:" ]
            , LayoutGrid.cell [] <|
                let
                    sList =
                        List.map (\user -> MLItem.listItem (MLItem.config |> MLItem.setOnClick (Msg.SetUser user)) [ MLItem.graphic [] [ identicon "100%" user ], Html.text user ]) users
                in
                case sList of
                    fir :: res ->
                        [ MList.list MList.config
                            fir
                            res
                        ]

                    _ ->
                        []
            ]

    else
        [ LayoutGrid.cell []
            [ p []
                [ Html.text "Looks like this is the first time you're using msquaredc!"
                ]
            , Button.text
                (Button.config |> Button.setOnClick (Msg.CRUD (Msg.CreateRandom Db.UserType [])))
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
                            , id = box x
                            , value = username
                            }
                    ]
                )

        uList =
            List.indexedMap
                (\index ( id, user ) ->
                    MLItem.listItem
                        (MLItem.config
                            |> MLItem.setOnClick (Msg.SetUser id)
                            --|> MLItem.activated (index == 0) TODO: Uncomment
                            --|> MLItem.selected False TODO: Uncomment
                            |> MLItem.setAttributes [ Html.Attributes.tabindex 0 ]
                        )
                        [ MLItem.graphic
                            (userIdenticonIcon id).attributes
                            (userIdenticonIcon id).elements
                        , MLItem.text []
                            { primary = [ Html.text <| Maybe.withDefault id user.name ]
                            , secondary = [ Html.text <| "Last login " ++ (Maybe.withDefault "" <| Maybe.map (\x -> relativeTime x (Time.millisToPosix user.last_login)) time) ]
                            }
                        ]
                )
            <|
                List.reverse <|
                    List.sortBy (\( _, b ) -> b.last_login) users
    in
    Dialog.dialog
        (Dialog.config
            |> Dialog.setOpen open
        )
        { title = Just "Select an account"
        , content =
            case uList of
                f :: rest ->
                    [ list (MList.config |> MList.setAvatarList True)
                        f
                        rest
                    ]

                _ ->
                    []
        , actions =
            [ list
                (MList.config |> MList.setNonInteractive True)
                (MLItem.listItem
                    (MLItem.config
                        |> MLItem.setAttributes
                            [ Html.Attributes.tabindex 0

                            --    , Html.Events.onClick Close
                            ]
                    )
                    [ TextField.filled
                        (TextField.config
                            |> TextField.setOnInput (Msg.Viewer << NewUsername)
                            |> TextField.setLabel (Just "Add new User")
                            |> TextField.setOnChange addUserWithName
                            |> TextField.setValue (Just new_username)
                        )
                    , Button.unelevated
                        (Button.config
                            |> Button.setAttributes [ Html.Attributes.style "margin-left" "16px" ]
                            |> Button.setIcon (Just <| Button.icon "add")
                        )
                        "add"
                    ]
                )
                []
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
