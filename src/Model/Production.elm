module Model.Production exposing (Condition, Production, Test(..), parser, symbolGenerator, testData)

import Model.Symbols as Symbols exposing (Symbols)
import Parser exposing ((|.), (|=), Parser, spaces, succeed, symbol)
import Ports.Rete exposing (AddProductionArgs)
import Set


type alias Production =
    { id : Int
    , name : String
    , conditions : List Condition
    , inRete : Maybe Int
    }


type alias Condition =
    { id : Test
    , attribute : Test
    , value : Test
    }


type Test
    = ConstantTest String
    | VariableTest String



---- PARSER ----


parser : Int -> String -> Parser Production
parser id name =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = condition
        , trailing = Parser.Optional
        }
        |> Parser.map (\conditions -> { id = id, name = name, conditions = conditions, inRete = Nothing })


condition : Parser Condition
condition =
    succeed Condition
        |. symbol "("
        |. spaces
        |= test
        |. spaces
        |. symbol "^"
        |= test
        |. spaces
        |= test
        |. spaces
        |. symbol ")"


test : Parser Test
test =
    let
        variableTest =
            succeed VariableTest |. symbol "<" |= Parser.variable { start = always True, inner = \c -> c /= '>', reserved = Set.empty } |. symbol ">"

        constantTest =
            succeed ConstantTest |= Parser.variable { start = always True, inner = \c -> Char.isAlpha c || c == '-', reserved = Set.empty }
    in
    Parser.oneOf [ variableTest, constantTest ]



---- SYMBOL GENERATION ----


symbolGenerator : Production -> Symbols.Generator AddProductionArgs
symbolGenerator production =
    let
        genTest test_ =
            case test_ of
                VariableTest _ ->
                    Symbols.constant { symbol = -1, isVariable = True }

                ConstantTest value ->
                    Symbols.genId value |> Symbols.map (\symbol -> { symbol = symbol, isVariable = False })

        genCondition cond =
            Symbols.map3 (\a b c -> { id = a, attribute = b, value = c })
                (genTest cond.id)
                (genTest cond.attribute)
                (genTest cond.value)
    in
    production.conditions
        |> List.foldr
            (genCondition >> Symbols.map2 (::))
            (Symbols.constant [])
        |> Symbols.map (\conditions -> { id = production.id, conditions = conditions })



---- TEST DATA ----


testData : List Production
testData =
    let
        var =
            VariableTest

        const =
            ConstantTest

        c1 =
            Condition (var "x") (const "on") (var "y")

        c2 =
            Condition (var "y") (const "left-of") (var "z")

        c3 =
            Condition (var "z") (const "color") (const "red")

        c4 =
            Condition (var "a") (const "color") (const "maize")

        c5 =
            Condition (var "b") (const "color") (const "blue")

        c6 =
            Condition (var "c") (const "color") (const "green")

        c7 =
            Condition (var "d") (const "color") (const "white")

        c8 =
            Condition (var "s") (const "on") (const "table")

        c9 =
            Condition (var "y") (var "a") (var "b")

        c10 =
            Condition (var "a") (const "left-of") (var "d")
    in
    [ { id = 1
      , name = "P1"
      , conditions = [ c1, c2, c3 ]
      , inRete = Nothing
      }
    , { id = 2
      , name = "P2"
      , conditions = [ c1, c2, c4, c5 ]
      , inRete = Nothing
      }
    , { id = 3
      , name = "P3"
      , conditions = [ c1, c2, c4, c3 ]
      , inRete = Nothing
      }
    ]
