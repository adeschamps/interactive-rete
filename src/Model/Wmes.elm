module Model.Wmes exposing (Wmes, add, empty, remove, toList)

import Dict exposing (Dict)
import Model.Wme exposing (Wme)


type Wmes
    = Wmes { wmes : Dict Int Wme, counter : Int }


empty : Wmes
empty =
    Wmes { wmes = Dict.empty, counter = 0 }


add : Wme -> Wmes -> Wmes
add wme (Wmes model) =
    Wmes
        { model
            | wmes = model.wmes |> Dict.insert model.counter wme
            , counter = model.counter + 1
        }


remove : Int -> Wmes -> Wmes
remove id (Wmes model) =
    Wmes
        { model
            | wmes = model.wmes |> Dict.remove id
        }


toList : Wmes -> List ( Int, Wme )
toList (Wmes model) =
    model.wmes |> Dict.toList
