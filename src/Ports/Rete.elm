port module Ports.Rete exposing
    ( Msg(..), subscriptions
    , addProduction, AddProductionArgs, removeProduction, RemoveProductionArgs, addWme, AddWmeArgs, removeWme, RemoveWmeArgs
    )

{-| This module defines the interface for interacting with the
underlying rete implementation.


# Incoming

@docs Msg, subscriptions


# Outgoing

@docs addProduction, AddProductionArgs, removeProduction, RemoveProductionArgs, addWme, AddWmeArgs, removeWme, RemoveWmeArgs

-}

------ INCOMING ------


{-| All the different event types.
-}
type Msg
    = Initialized InitializedArgs
    | AddedNode AddedNodeArgs
    | RemovedNode RemovedNodeArgs
    | AddedProduction AddedProductionArgs
    | RemovedProduction RemovedProductionArgs
    | AddedToken AddedTokenArgs
    | RemovedToken RemovedTokenArgs
    | AddedWme AddedWmeArgs
    | RemovedWme RemovedWmeArgs
    | AddedAlphaMemory AddedAlphaMemoryArgs
    | RemovedAlphaMemory RemovedAlphaMemoryArgs


{-| Subscribe to all the different event types.
-}
subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ initialized Initialized
        , addedNode AddedNode
        , removedNode RemovedNode
        , addedProduction AddedProduction
        , removedProduction RemovedProduction
        , addedToken AddedToken
        , removedToken RemovedToken
        , addedWme AddedWme
        , removedWme RemovedWme
        , addedAlphaMemory AddedAlphaMemory
        , removedAlphaMemory RemovedAlphaMemory
        ]


{-| Sent when the rete is initialized or reset.
-}
port initialized : (InitializedArgs -> msg) -> Sub msg


type alias InitializedArgs =
    { dummyNodeId : Int
    , dummyTokenId : Int
    }


{-| Sent when a node is inserted into the beta network.
-}
port addedNode : (AddedNodeArgs -> msg) -> Sub msg


type alias AddedNodeArgs =
    { id : Int
    , parentId : Int
    , children : List Int
    , kind : String
    , alphaNodeId : Maybe Int
    }


{-| Sent when a node is removed from the beta network.
-}
port removedNode : (RemovedNodeArgs -> msg) -> Sub msg


type alias RemovedNodeArgs =
    { id : Int
    }


port addedProduction : (AddedProductionArgs -> msg) -> Sub msg


type alias AddedProductionArgs =
    { id : Int

    -- , name : String
    , pNodeId : Int
    }


port removedProduction : (RemovedProductionArgs -> msg) -> Sub msg


type alias RemovedProductionArgs =
    { id : Int
    }


port addedToken : (AddedTokenArgs -> msg) -> Sub msg


type alias AddedTokenArgs =
    { id : Int
    , parentId : Int
    , wmeTimetag : Int
    , betaNodeId : Int
    }


port removedToken : (RemovedTokenArgs -> msg) -> Sub msg


type alias RemovedTokenArgs =
    { id : Int
    }


port addedWme : (AddedWmeArgs -> msg) -> Sub msg


type alias AddedWmeArgs =
    { timetag : Int
    , id : Int
    , attribute : Int
    , value : Int
    }


port removedWme : (RemovedWmeArgs -> msg) -> Sub msg


type alias RemovedWmeArgs =
    { timetag : Int
    }


port addedAlphaMemory : (AddedAlphaMemoryArgs -> msg) -> Sub msg


type alias AddedAlphaMemoryArgs =
    { id : Int
    , test :
        { id : Maybe Int
        , attribute : Maybe Int
        , value : Maybe Int
        }
    }


port removedAlphaMemory : (RemovedAlphaMemoryArgs -> msg) -> Sub msg


type alias RemovedAlphaMemoryArgs =
    { id : Int
    }



------ OUTGOING ------


port addProduction : AddProductionArgs -> Cmd msg


type alias AddProductionArgs =
    { id : Int
    , conditions : List Condition
    }


type alias Condition =
    { id : Test
    , attribute : Test
    , value : Test
    }


type alias Test =
    { symbol : Int
    , isVariable : Bool
    }


port removeProduction : RemoveProductionArgs -> Cmd msg


type alias RemoveProductionArgs =
    { id : Int
    }


port addWme : AddWmeArgs -> Cmd msg


type alias AddWmeArgs =
    { id : Int
    , attribute : Int
    , value : Int
    }


port removeWme : RemoveWmeArgs -> Cmd msg


type alias RemoveWmeArgs =
    { timetag : Int
    }
