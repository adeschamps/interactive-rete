module Model.Symbols exposing
    ( Symbols
    , new
    , id, value
    , add
    )

{-| The rete doesn't really care about the type of data it's working
with, so internally it just stores IDs. The values that those IDs
correspond to are kept track of by this module.


# Definition

@docs Symbols


# Build

@docs new


# Query

@docs id, value


# Transform

@docs add

-}

import Dict exposing (Dict)


{-| A symbol registry assigns unique IDs to string values.
-}
type Symbols
    = Symbols
        { ids : Dict String Int
        , values : Dict Int String
        , counter : Int
        }


{-| Create a new symbol registry.
-}
new : Symbols
new =
    Symbols
        { ids = Dict.empty
        , values = Dict.empty
        , counter = 0
        }


{-| Get the ID of the given symbol.
-}
id : String -> Symbols -> Maybe Int
id value_ (Symbols { ids }) =
    ids |> Dict.get value_


{-| Given a symbol ID, look up its value.
-}
value : Int -> Symbols -> Maybe String
value id_ (Symbols { values }) =
    values |> Dict.get id_


{-| Add a symbol to the registry. This is idempotent.
-}
add : String -> Symbols -> Symbols
add value_ (Symbols model) =
    if Dict.member value_ model.ids then
        Symbols model

    else
        Symbols
            { model
                | ids = model.ids |> Dict.insert value_ model.counter
                , values = model.values |> Dict.insert model.counter value_
                , counter = model.counter + 1
            }
