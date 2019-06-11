module Tests exposing (all)

import Expect
import Model.Production as Production
import Model.Wme as Wme
import Parser
import Test exposing (..)


all : Test
all =
    describe "Test Suite"
        [ productions
        , wmes
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
