module Model.Wme exposing (Wme, parser, testData)

import Parser exposing ((|.), (|=), Parser, spaces, succeed, symbol)
import Set


type alias Wme =
    { id : String
    , attribute : String
    , value : String
    }


parser : Parser Wme
parser =
    succeed Wme
        |. symbol "("
        |. spaces
        |= value
        |. spaces
        |. symbol "^"
        |= value
        |. spaces
        |= value
        |. spaces
        |. symbol ")"


value : Parser String
value =
    Parser.variable
        { start = Char.isAlphaNum
        , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
        , reserved = Set.fromList []
        }


testData : List Wme
testData =
    [ Wme "b1" "on" "b2"
    , Wme "b1" "on" "b3"
    , Wme "b1" "color" "red"
    , Wme "b2" "on" "table"
    , Wme "b2" "left-of" "b3"
    , Wme "b2" "color" "blue"
    , Wme "b3" "left-of" "b4"
    , Wme "b3" "on" "table"
    , Wme "b3" "color" "red"
    ]
