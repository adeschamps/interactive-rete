module Model.Rete exposing (Node, Rete, Symbol, SymbolID, Wme(..), addNode, addToken, empty, removeNode, removeToken)

import Graph exposing (Graph)
import IntDict


type alias Rete =
    { network : Graph Node ()
    , tokens : Graph Token ()
    }


type Wme
    = Wme SymbolID SymbolID SymbolID


type alias SymbolID =
    Int


type Symbol
    = Constant String
    | Identifier String


type Node
    = Alpha AlphaNode
    | Beta BetaNode
    | Join
    | Production


type alias AlphaNode =
    { test : List Test
    , wmes : List Wme
    }


type Test
    = ConstantTest SymbolID
    | VariableTest String


type alias BetaNode =
    { tokens : List TokenID
    }


type alias TokenID =
    Graph.NodeId


type alias Token =
    { wme : Wme
    , betaNode : Graph.NodeId
    }


empty : Rete
empty =
    let
        dummyNode =
            Beta { tokens = [ 0 ] }

        dummyToken =
            { wme = Wme -1 -1 -1, betaNode = 0 }
    in
    { network = Graph.fromNodesAndEdges [ Graph.Node 0 dummyNode ] []
    , tokens = Graph.fromNodesAndEdges [ Graph.Node 0 dummyToken ] []
    }


addToken : { id : Int, token : Token, parent : Int } -> Rete -> Rete
addToken { id, token, parent } rete =
    { rete
        | tokens =
            rete.tokens
                |> Graph.insert
                    { node = Graph.Node id token
                    , incoming = IntDict.empty
                    , outgoing = IntDict.singleton parent ()
                    }
    }


removeToken : Int -> Rete -> Rete
removeToken id rete =
    { rete
        | tokens = rete.tokens |> Graph.remove id
    }


addNode : { id : Int, node : Node, parent : Int } -> Rete -> Rete
addNode { id, node, parent } rete =
    { rete
        | network =
            rete.network
                |> Graph.insert
                    { node = Graph.Node id node
                    , incoming = IntDict.singleton parent ()
                    , outgoing = IntDict.empty
                    }
    }


removeNode : Int -> Rete -> Rete
removeNode id rete =
    { rete
        | network = rete.network |> Graph.remove id
    }
