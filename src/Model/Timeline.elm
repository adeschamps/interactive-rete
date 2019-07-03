module Model.Timeline exposing
    ( Timeline, init, step
    , size, state, currentState, cacheSize
    )

{-| Suppose you want to travel back in time to the state of the world at any
given moment. The simplest thing to do would be to store a list of all the past
states, and then retrieve the state at a given index. This works, but it becomes
problematic because it consumes a lot of memory.

This module stores the initial state of a model along with a list of functions
that can be used to produce the next state. This still takes a linear amount of
memory, but the transition functions should be much smaller than the model
itself. This is because a typical transition function is just an update function
that has been partially applied with a relatively small message. Starting from
the initial state, we can recreate any other state by applying all the updates.

However, we've just traded one problem for another - by just storing the
updates, we have to do a lot more computation in order to recreate a past state.
To balance these tradeoffs, we store intermediate states as well. These are
stored in a series of lists of increasingly coarse resolution. This means that
for more recent states we might have every one saved, while for older states we
might only save ever sixteenth state, and then do a bit of computation in order
to recreate the state that we're looking for.

@docs Timeline, init, step


# Query

@docs size, state, currentState, cacheSize

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


{-| Get the total number of states that have been stored in the timeline. The
timeline does not necessarily store all these states, but they can all be
recreated by reapplying the transition functions.
-}
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
            -- This branch should only be reached if the caller has asked for a
            -- target index that is out of bounds.
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


{-| Create a new state in the timeline by applying the given transition function
to the most recent state. The transition function itself will be saved so that
the timeline can recreate the new state even if it no longer being explicitly
stored.
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
