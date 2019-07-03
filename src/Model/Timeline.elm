module Model.Timeline exposing (Timeline, cacheSize, init, size, state, step)

{-| Suppose you want to travel back in time to the state of the world at any
given moment. The simplest thing to do would be to store a list of all the past
states, and then retrieve the state at a given index. This works, but it becomes
problematic because it consumes a lot of memory.

That said, this module doesn't do anything optimized yet. We'll try to get the
API right, then make it efficient later.

-}

import Array exposing (Array)


{-| A sequence of states over time, represented as an initial state along with a
sequence of function that produce the next state.
-}
type Timeline a
    = Timeline
        { initialState : a
        , currentState : a
        , history : Array (Block a)
        , transitions : List (a -> a)
        }


type alias Block a =
    { start : Int
    , states : List a
    }


chunkSize : Int
chunkSize =
    4


{-| Create a new timeline with an initial state.
-}
init : a -> Timeline a
init initial =
    Timeline
        { initialState = initial
        , currentState = initial
        , history = Array.fromList [ { start = 0, states = [ initial ] } ]
        , transitions = []
        }


size : Timeline a -> Int
size (Timeline { transitions }) =
    1 + List.length transitions


{-| Retrieve the state at the current index in the timeline.
-}
state : Int -> Timeline a -> a
state index (Timeline model) =
    case model.history |> searchHistory index 0 of
        Just ( startIndex, startState ) ->
            model.transitions
                |> List.drop (List.length model.transitions - index)
                |> List.take (index - startIndex)
                |> List.foldr identity startState

        Nothing ->
            model.currentState


searchHistory : Int -> Int -> Array (Block a) -> Maybe ( Int, a )
searchHistory target level history =
    case Array.get level history of
        Just block ->
            if target < block.start then
                searchHistory target (level + 1) history

            else
                let
                    stepSize =
                        chunkSize ^ (level + 1)

                    targetIndex =
                        (target - block.start) // stepSize
                in
                block.states
                    |> List.reverse
                    |> List.drop targetIndex
                    |> List.head
                    |> Maybe.map (\item -> ( block.start + targetIndex * stepSize, item ))

        Nothing ->
            Nothing


{-| Get the most recent state.
-}
currentState : Timeline a -> a
currentState (Timeline model) =
    model.currentState


{-| Get the number of cached states. Technically this is revealing an
implementation detail and shouldn't be exposed, but since the whole point of
this module is to represent lots of states using less memory, it seems
appropriate that the user should be able to check that it's actually doing what
it says.
-}
cacheSize : Timeline a -> Int
cacheSize (Timeline model) =
    Array.foldl (.states >> List.length >> (+)) 0 model.history


{-| Create a new state in the timeline by applying the given function to the
most recent state.
-}
step : (a -> a) -> Timeline a -> Timeline a
step transition (Timeline model) =
    let
        newState =
            transition model.currentState
    in
    Timeline
        { model
            | currentState = newState
            , history = model.history |> addHistory 0 newState
            , transitions = transition :: model.transitions
        }


addHistory : Int -> a -> Array (Block a) -> Array (Block a)
addHistory level newState history =
    let
        checkBlock block =
            if List.length block.states < chunkSize then
                Ok block

            else
                Err ( block.start, block.states |> List.reverse |> List.head |> Maybe.withDefault newState )
    in
    case Array.get level history |> Maybe.map checkBlock of
        Just (Ok block) ->
            history
                |> Array.set level { block | states = newState :: block.states }

        Just (Err ( start, firstItem )) ->
            history
                |> Array.set level (Block (start + chunkSize ^ (level + 1)) [ newState ])
                |> addHistory (level + 1) firstItem

        Nothing ->
            history
                |> Array.push (Block 0 [ newState ])
