module View.Production exposing (Condition, InsertionState(..), Production, Test, view)

import Element exposing (Element, column, el, fill, row, spaceEvenly, text, width)
import Element.Background as Background
import Element.Events as Events
import Element.Input as Input
import Palette


type alias Production msg =
    { name : String
    , conditions : List (Condition msg)
    , insertionState : InsertionState msg
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


type InsertionState msg
    = Inserted { onRemove : msg }
    | NotInserted { onInsert : msg, onDelete : msg }


view : Production msg -> Element msg
view { name, conditions, insertionState } =
    column [ width fill ]
        [ row [ width fill, spaceEvenly ] [ text name, viewControls insertionState ]
        , column [] (List.map viewCondition conditions)
        ]


viewControls : InsertionState msg -> Element msg
viewControls insertionState =
    let
        insertArgs =
            case insertionState of
                Inserted { onRemove } ->
                    { label = text "=>", onPress = Just onRemove }

                NotInserted { onInsert } ->
                    { label = text "<=", onPress = Just onInsert }

        deleteOnPress =
            case insertionState of
                Inserted _ ->
                    Nothing

                NotInserted { onDelete } ->
                    Just onDelete
    in
    row []
        [ Input.button [] insertArgs
        , Input.button [] { label = text "X", onPress = deleteOnPress }
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
