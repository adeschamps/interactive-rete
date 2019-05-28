module View.Production exposing (view)

import Element exposing (Element, column, el, fill, row, text, width)
import Element.Background as Background
import Element.Events as Events
import Palette


type alias Production msg =
    { name : String
    , conditions : List (Condition msg)
    }


type alias Condition msg =
    { id : Test msg
    , attribute : Test msg
    , value : Test msg
    }


type alias Test msg =
    { name : String
    , constant : Bool
    , hovered : Bool
    , selected : Bool
    , onClick : msg
    , onHover : msg
    , onUnhover : msg
    }


view : Production msg -> Element msg
view { name, conditions } =
    column []
        [ text name
        , column [] (List.map viewCondition conditions)
        ]


viewCondition : Condition msg -> Element msg
viewCondition condition =
    let
        test getTest =
            viewTest (getTest condition)
    in
    row []
        [ text "("
        , test .id
        , text " ^"
        , test .attribute
        , text " "
        , test .value
        , text ")"
        ]


viewTest : Test msg -> Element msg
viewTest test =
    let
        label =
            if test.constant then
                test.name

            else
                "<" ++ test.name ++ ">"

        background =
            if test.selected then
                Palette.selected

            else if test.hovered then
                Palette.hovered

            else
                Palette.white
    in
    el
        [ Events.onClick test.onClick
        , Events.onMouseEnter test.onHover
        , Events.onMouseLeave test.onUnhover
        , Background.color background
        ]
    <|
        text label
