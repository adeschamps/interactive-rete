module Model.Timeline exposing (Timeline, cacheSize, init, select, state, step)

{-| Suppose you want to travel back in time to the state of the world at any
given moment. The simplest thing to do would be to store a list of all the past
states, and then retrieve the state at a given index. This works, but it becomes
problematic because it consumes a lot of memory.

That said, this module doesn't do anything optimized yet. We'll try to get the
API right, then make it efficient later.

-}


{-| A sequence of states over time, represented as an initial state along with a
sequence of function that produce the next state.
-}
type Timeline a
    = Timeline
        { selectedIndex : Int
        , initialState : a
        , states : List a
        }


{-| Create a new timeline with an initial state.
-}
init : a -> Timeline a
init initial =
    Timeline
        { selectedIndex = 0
        , initialState = initial
        , states = []
        }


{-| Retrieve the state at the current index in the timeline.
-}
state : Int -> Timeline a -> a
state index (Timeline model) =
    model.states
        |> List.drop (List.length model.states - index)
        |> List.head
        |> Maybe.withDefault model.initialState


{-| Get the most recent state.
-}
currentState : Timeline a -> a
currentState ((Timeline { selectedIndex }) as timeline) =
    state selectedIndex timeline


{-| Get the number of cached states. Technically this is revealing an
implementation detail and shouldn't be exposed, but since the whole point of
this module is to represent lots of states using less memory, it seems
appropriate that the user should be able to check that it's actually doing what
it says.
-}
cacheSize : Timeline a -> Int
cacheSize (Timeline model) =
    model.states |> List.length


{-| Create a new state in the timeline by applying the given function to the
most recent state.
-}
step : (a -> a) -> Timeline a -> Timeline a
step transition (Timeline model) =
    let
        current =
            model.states |> List.head |> Maybe.withDefault model.initialState

        next =
            transition current
    in
    Timeline { model | states = next :: model.states }


{-| Mark the state at the given index as active. The idea here is that this
won't affect the outputs of this module, but it will act as a hint to optimize
efficiency.
-}
select : Int -> Timeline a -> Timeline a
select index (Timeline model) =
    Timeline { model | selectedIndex = index }
