module Tests exposing (productions, symbolsTests, timelineTests, wmes)

import Expect
import Fuzz exposing (..)
import Model.Production as Production
import Model.Symbols as Symbols
import Model.Timeline as Timeline
import Model.Wme as Wme
import Parser
import Test exposing (..)


wmes : Test
wmes =
    describe "WME parsing"
        [ test "Parse valid WME" <|
            \_ ->
                "(S1 ^superstate nil)"
                    |> Parser.run Wme.parser
                    |> Expect.equal (Ok { id = "S1", attribute = "superstate", value = "nil" })
        , test "WMEs can't contain variables" <|
            \_ ->
                "(<s> ^superstate nil)"
                    |> Parser.run Wme.parser
                    |> Expect.err
        ]


productions : Test
productions =
    describe "Production parsing"
        [ test "Parse valid production" <|
            \_ ->
                "(<s> ^io <io>) (<io> ^input-link <il>)"
                    |> Parser.run (Production.parser 0 "name")
                    |> Expect.equal
                        (Ok
                            { id = 0
                            , name = "name"
                            , inRete = Nothing
                            , conditions =
                                [ { id = Production.VariableTest "s", attribute = Production.ConstantTest "io", value = Production.VariableTest "io" }
                                , { id = Production.VariableTest "io", attribute = Production.ConstantTest "input-link", value = Production.VariableTest "il" }
                                ]
                            }
                        )
        , test "Parse invalid production" <|
            \_ ->
                "<a> ^missing-parens <b>"
                    |> Parser.run (Production.parser 0 "name")
                    |> Expect.err
        ]


symbolsTests : Test
symbolsTests =
    describe "Symbol managing"
        [ test "Empty symbol registry" <|
            \_ ->
                let
                    symbols =
                        Symbols.new
                in
                symbols
                    |> Symbols.symbol "foo"
                    |> Expect.equal Nothing
        , test "Store and retrieve a symbol" <|
            \_ ->
                let
                    symbols =
                        Symbols.new |> Symbols.add "foo"
                in
                symbols
                    |> Symbols.symbol "foo"
                    |> Result.fromMaybe ()
                    |> Expect.ok
        , test "Storage is idempotent" <|
            \_ ->
                let
                    symbols1 =
                        Symbols.new |> Symbols.add "foo"

                    sym1 =
                        symbols1 |> Symbols.symbol "foo"

                    symbols2 =
                        symbols1 |> Symbols.add "foo"

                    sym2 =
                        symbols2 |> Symbols.symbol "foo"
                in
                sym1
                    |> Expect.all
                        [ Expect.notEqual Nothing
                        , Expect.equal sym2
                        ]
        , test "No duplicate symbols" <|
            \_ ->
                let
                    symbols =
                        Symbols.new |> Symbols.add "foo" |> Symbols.add "bar"
                in
                Symbols.symbol "foo" symbols
                    |> Expect.all
                        [ Expect.notEqual Nothing
                        , Expect.notEqual (Symbols.symbol "bar" symbols)
                        ]
        , fuzz (list string) "Roundtrip" <|
            \strings ->
                let
                    generator =
                        strings
                            |> List.foldr
                                (Symbols.genSym >> Symbols.map2 (::))
                                (Symbols.constant [])

                    ( ids, symbols ) =
                        Symbols.new |> Symbols.step generator

                    getValue id =
                        Symbols.value id symbols
                in
                ids
                    |> List.map getValue
                    |> Expect.equal strings
        ]


timelineTests : Test
timelineTests =
    describe "Timeline"
        [ test "Initial state" <|
            \_ ->
                Timeline.init 1
                    |> Timeline.state 0
                    |> Expect.equal 1
        , test "Initial size" <|
            \_ ->
                Timeline.init ()
                    |> Timeline.size
                    |> Expect.equal 1
        , test "Double values" <|
            \_ ->
                Timeline.init 1
                    |> Timeline.step ((*) 2)
                    |> Timeline.step ((*) 2)
                    |> Timeline.state 2
                    |> Expect.equal 4
        , describe "Intermediate state"
            [ test "Of small timeline" <|
                \_ ->
                    Timeline.init 1
                        |> Timeline.step ((*) 2)
                        |> Timeline.step ((*) 2)
                        |> Timeline.step ((*) 2)
                        |> Timeline.step ((*) 2)
                        |> Timeline.state 2
                        |> Expect.equal 4
            , test "Of large timeline" <|
                \_ ->
                    List.repeat 128 ((+) 1)
                        |> List.foldl Timeline.step (Timeline.init 0)
                        |> Timeline.state 71
                        |> Expect.equal 71
            ]
        , test "Size of large timeline" <|
            \_ ->
                List.repeat 128 identity
                    |> List.foldr Timeline.step (Timeline.init ())
                    |> Timeline.size
                    |> Expect.equal 129
        , describe "Cache size"
            [ test "Empty timeline" <|
                \_ ->
                    Timeline.init ()
                        |> Timeline.cacheSize
                        |> Expect.equal 1
            , test "Small timeline" <|
                \_ ->
                    Timeline.init ()
                        |> Timeline.step identity
                        |> Timeline.step identity
                        |> Timeline.cacheSize
                        |> Expect.equal 3
            , test "Large timeline" <|
                \_ ->
                    List.repeat 128 identity
                        |> List.foldr Timeline.step (Timeline.init ())
                        |> Timeline.cacheSize
                        |> Expect.all
                            [ Expect.greaterThan 0
                            , Expect.lessThan 128
                            ]
            ]
        ]
