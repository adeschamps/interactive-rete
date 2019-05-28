module Model.Rete exposing (Node, Rete, Symbol, SymbolID, empty)

import Graph exposing (Graph)


type alias Rete =
    { network : Graph Node ()
    , tokens : Graph Token ()
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
