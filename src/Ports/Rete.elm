port module Ports.Rete exposing
    ( Msg(..), subscriptions
    , addProduction, removeProduction, addWme, removeWme
    )

{-| This module defines the interface for interacting with the
underlying rete implementation.


# Incoming

@docs Msg, subscriptions


# Outgoing

@docs addProduction, removeProduction, addWme, removeWme

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
        ]


port initialized : (InitializedArgs -> msg) -> Sub msg


type alias InitializedArgs =
    { dummyNodeId : Int
    , dummyTokenId : Int
    }


port addedNode : (AddedNodeArgs -> msg) -> Sub msg


type alias AddedNodeArgs =
    { id : Int
    , parentId : Int
    , children : List Int
    , kind : String
    , alphaNodeId : Maybe Int
    }


port removedNode : (RemovedNodeArgs -> msg) -> Sub msg


type alias RemovedNodeArgs =
    { id : Int
    }


port addedProduction : (AddedProductionArgs -> msg) -> Sub msg


type alias AddedProductionArgs =
    { id : Int
    , name : String
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



------ OUTGOING ------


port addProduction : AddProductionArgs -> Cmd msg


type alias AddProductionArgs =
    { name : String
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
