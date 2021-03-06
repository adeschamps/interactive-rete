module Model.Symbols exposing
    ( Symbols, Symbol, id
    , new
    , symbol, value, toList
    , add
    , Generator, step, genSym, genId, constant, map, map2, map3, andThen
    )

{-| The rete doesn't really care about the type of data it's working
with, so internally it just stores IDs. The values that those IDs
correspond to are kept track of by this module.


# Definition

@docs Symbols, Symbol, id


# Build

@docs new


# Query

@docs symbol, value, toList


# Transform

@docs add


# Generation

Sometimes we need to get or create IDs for a bunch of values at once,
but it's tedious to pass around the state of the symbol manager. It's
also inconvenient that asking for the ID of a value returns a `Maybe`.
Generators allow us to build up a computation that will be executed
all at once in the context of the symbol manager by calling the `step`
function.

@docs Generator, step, genSym, genId, constant, map, map2, map3, andThen

-}

import Dict exposing (Dict)


{-| A symbol registry assigns unique IDs to string values.
-}
type Symbols
    = Symbols
        { ids : Dict String Symbol
        , values : Dict Int String
        , counter : Int
        }


{-| A symbol is a key to a value in the Rete. It is internally
represented as an integer so that it can be easily passed around.
-}
type Symbol
    = Symbol Int


{-| Get the internal integer representation of a `Symbol`.
-}
id : Symbol -> Int
id (Symbol id_) =
    id_


{-| Create a new symbol registry.
-}
new : Symbols
new =
    Symbols
        { ids = Dict.empty
        , values = Dict.empty
        , counter = 0
        }


{-| Get the `Symbol` representation of the given value.
-}
symbol : String -> Symbols -> Maybe Symbol
symbol value_ (Symbols { ids }) =
    ids |> Dict.get value_


{-| Given a `Symbol` ID, look up its value.

This makes a small sacrifice in safety and correctness in order to
gain some convenience. Technically we can't guarantee that a given
`Symbol` corresponds to a valid value. However, since `Symbol`s can
only be constructed by this module, this is a relatively easy
constraint to enforce. The caveat is that if you have more than one
instance of a `Symbols` registry, you shouldn't query one with the
other's IDs.

If you do somehow try to retrieve the value for an invalid `Symbol`,
you'll either get a crash or a default string that is obviously wrong.

-}
value : Symbol -> Symbols -> String
value (Symbol id_) (Symbols { values }) =
    case values |> Dict.get id_ of
        Just value_ ->
            value_

        Nothing ->
            defaultValue ()


{-| Get a list of all the ID and symbol pairs.
-}
toList : Symbols -> List ( Symbol, String )
toList (Symbols { values }) =
    values |> Dict.toList |> List.map (\( id_, value_ ) -> ( Symbol id_, value_ ))


{-| Add a symbol to the registry. This is idempotent.
-}
add : String -> Symbols -> Symbols
add value_ (Symbols model) =
    if Dict.member value_ model.ids then
        Symbols model

    else
        Symbols
            { model
                | ids = model.ids |> Dict.insert value_ (Symbol model.counter)
                , values = model.values |> Dict.insert model.counter value_
                , counter = model.counter + 1
            }



---- GENERATION ----


{-| A recipe for generating a new value in the contect of a symbol
generator.
-}
type Generator a
    = Generator (Symbols -> ( a, Symbols ))


{-| Run a generator in the context of a symbol generator. Make sure to
save the new symbol generator; if you use the old one again, you may
wind up with different values that share the same ID.
-}
step : Generator a -> Symbols -> ( a, Symbols )
step (Generator generator) symbols =
    generator symbols


{-| Create a generator for the `Symbol` ID corresponding to a string.
If the symbol manager already has an ID for this value, then it'll
return that; otherwise it'll generate a new ID.
-}
genSym : String -> Generator Symbol
genSym value_ =
    Generator (getOrCreate value_)


{-| Generate a new `Symbol` but return the underlying integer ID
instead. This is convenient if you creating output for an API that
requires primitive values.
-}
genId : String -> Generator Int
genId value_ =
    genSym value_ |> map id


{-| Generate the same value every time. The symbol manager itself will
not be used.
-}
constant : a -> Generator a
constant const =
    Generator (\sym -> ( const, sym ))


{-| Transform a value produced by a generator.
-}
map : (a -> b) -> Generator a -> Generator b
map func (Generator genA) =
    Generator
        (\sym0 ->
            let
                ( a, sym1 ) =
                    genA sym0
            in
            ( func a, sym1 )
        )


{-| Combine two generators.
-}
map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 func (Generator genA) (Generator genB) =
    Generator
        (\sym0 ->
            let
                ( a, sym1 ) =
                    genA sym0

                ( b, sym2 ) =
                    genB sym1
            in
            ( func a b, sym2 )
        )


{-| Combine three generators.
-}
map3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 func (Generator genA) (Generator genB) (Generator genC) =
    Generator
        (\sym0 ->
            let
                ( a, sym1 ) =
                    genA sym0

                ( b, sym2 ) =
                    genB sym1

                ( c, sym3 ) =
                    genC sym2
            in
            ( func a b c, sym3 )
        )


{-| Create a new generator that depends on the result of a previous
generator.
-}
andThen : (a -> Generator b) -> Generator a -> Generator b
andThen func (Generator genA) =
    Generator
        (\sym ->
            let
                ( result, newSym ) =
                    genA sym

                (Generator genB) =
                    func result
            in
            genB newSym
        )



---- HELPERS ----


getOrCreate : String -> Symbols -> ( Symbol, Symbols )
getOrCreate value_ (Symbols model) =
    case Dict.get value_ model.ids of
        Just sym ->
            ( sym, Symbols model )

        Nothing ->
            let
                sym =
                    Symbol model.counter
            in
            ( sym
            , Symbols
                { model
                    | ids = model.ids |> Dict.insert value_ sym
                    , values = model.values |> Dict.insert model.counter value_
                    , counter = model.counter + 1
                }
            )


defaultValue : () -> String
defaultValue () =
    Debug.todo "A DEFAULT SYMBOL WAS CONSTRUCTED"
