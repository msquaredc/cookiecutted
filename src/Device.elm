module Device exposing (fromPixel, Device(..),WindowType(..),Orientation(..))

type alias DeviceConfig = 
    {
        device : Device
        , orientation : Orientation
        , windowType : WindowType
        , columns : Int
    }
type Device 
    = Handset
    | Tablet
    | Desktop

type WindowType
    = XSmall
    | Small
    | Medium
    | Large
    | XLarge

type Orientation
    = Portrait
    | Landscape

fromPixel : Int -> Int -> DeviceConfig
fromPixel width height =
    let
        dom = max width height
        orientation = if height > width then Portrait else Landscape
    in
        {
            device = getDevice dom orientation
            , orientation = orientation
            , windowType = getWindowType dom
            , columns = getColumns dom
        }

getDevice : Int -> Orientation -> Device
getDevice pixel orientation =
    case orientation of
        Landscape ->
            if pixel < 960 then
                Handset
            else
                if pixel < 1440 then
                    Tablet
                else
                    Desktop
        Portrait ->
            if pixel < 600 then
                Handset
            else
                if pixel < 960 then
                    Tablet
                else
                    Desktop
            
getWindowType : Int -> WindowType
getWindowType pixel =
    if pixel < 600 then
        XSmall
    else
        if pixel < 1024 then
            Small
        else
            if pixel < 1440 then
                Medium
            else
                if pixel < 1920 then
                    Large
                else
                    XLarge

getColumns : Int -> Int
getColumns pixel =
    if pixel < 600 then
        4
    else
        if pixel < 840 then
            8
        else
            12