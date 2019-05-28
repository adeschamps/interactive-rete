port module Ports.Rete exposing (Msg(..), subscriptions)

------ INCOMING ------


type Msg
    = AddedSymbol AddedSymbolArgs
    | RemovedSymbol RemovedSymbolArgs
    | AddedNode AddedNodeArgs
    | RemovedNode RemovedNodeArgs
    | AddedProduction AddedProductionArgs
    | RemovedProduction RemovedProductionArgs
    | AddedToken AddedTokenArgs
    | RemovedToken RemovedTokenArgs


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ addedSymbol AddedSymbol
        , removedSymbol RemovedSymbol
        , addedNode AddedNode
        , removedNode RemovedNode
        , addedProduction AddedProduction
        , removedProduction RemovedProduction
        , addedToken AddedToken
        , removedToken RemovedToken
        ]


port addedSymbol : (AddedSymbolArgs -> msg) -> Sub msg


type alias AddedSymbolArgs =
    { id : Int
    , name : String
    }


port removedSymbol : (RemovedSymbolArgs -> msg) -> Sub msg


type alias RemovedSymbolArgs =
    { id : Int
    }


port addedNode : (AddedNodeArgs -> msg) -> Sub msg


type alias AddedNodeArgs =
    { id : Int
    , parentId : Int
    , children : List Int
    , kind : String
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
    , wmeId : Int
    }


port removedToken : (RemovedTokenArgs -> msg) -> Sub msg


type alias RemovedTokenArgs =
    { id : Int
    }



------ OUTGOING ------


type Command
    = AddProduction AddProductionArgs
    | RemoveProduction RemoveProductionArgs
    | AddWme AddWmeArgs
    | RemoveWme RemoveWmeArgs


send : Command -> Cmd msg
send command =
    case command of
        AddProduction args ->
            addProduction args

        RemoveProduction args ->
            removeProduction args

        AddWme args ->
            addWme args

        RemoveWme args ->
            removeWme args


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
    {}


port removeWme : RemoveWmeArgs -> Cmd msg


type alias RemoveWmeArgs =
    {}
