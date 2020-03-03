module Type.Database.Entity exposing (..)

import Dict
import Html

type alias Entity a =
    {
        value : a  
    }



attributes : Entity a -> List String
attributes entity =
    (Dict.keys entity.int) 
    ++ (Dict.keys entity.float)
    ++ (Dict.keys entity.string)
    |> List.sort

table : List (Entity a) -> Html.Html msg 
table entities = 
    let 
        headers = List.head entities
                  |> Maybe.map attributes
                  |> Maybe.withDefault []
    in
        Html.table [] [
            Html.tr []
                (List.map (\x ->Html.th [][Html.text x]) headers)
            
        ]


    
