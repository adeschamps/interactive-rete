module Tests exposing (all)

import Expect
import Fuzz exposing (..)
import Model.Production as Production
import Model.Symbols as Symbols
import Model.Wme as Wme
import Parser
import Test exposing (..)


all : Test
all =
    describe "Test Suite"
        [ productions
        , wmes
        , symbolsTests
        ]


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
                    |> Symbols.id "foo"
                    |> Expect.equal Nothing
        , test "Store and retrieve a symbol" <|
            \_ ->
                let
                    symbols =
                        Symbols.new |> Symbols.add "foo"
                in
                symbols
                    |> Symbols.id "foo"
                    |> Result.fromMaybe ()
                    |> Expect.ok
        , test "Storage is idempotent" <|
            \_ ->
                let
                    symbols1 =
                        Symbols.new |> Symbols.add "foo"

                    id1 =
                        symbols1 |> Symbols.id "foo"

                    symbols2 =
                        symbols1 |> Symbols.add "foo"

                    id2 =
                        symbols2 |> Symbols.id "foo"
                in
                id1
                    |> Expect.all
                        [ Expect.notEqual Nothing
                        , Expect.equal id2
                        ]
        , test "No duplicate symbols" <|
            \_ ->
                let
                    symbols =
                        Symbols.new |> Symbols.add "foo" |> Symbols.add "bar"
                in
                Symbols.id "foo" symbols
                    |> Expect.all
                        [ Expect.notEqual Nothing
                        , Expect.notEqual (Symbols.id "bar" symbols)
                        ]
        , fuzz (list string) "Roundtrip" <|
            \strings ->
                let
                    generator =
                        strings
                            |> List.foldr
                                (Symbols.genId >> Symbols.map2 (::))
                                (Symbols.constant [])

                    ( ids, symbols ) =
                        Symbols.new |> Symbols.step generator

                    getValue id =
                        Symbols.value id symbols
                in
                ids
                    |> List.filterMap getValue
                    |> Expect.equal strings
        ]
