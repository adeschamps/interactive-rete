module View.Wme exposing (InsertionState(..), view)

import Element exposing (Element, el, fill, row, spaceEvenly, text, width)
import Element.Background as Background
import Element.Events as Events
import Element.Input as Input
import Palette


type alias Wme msg =
    { index : Int
    , id : Symbol msg
    , attribute : Symbol msg
    , value : Symbol msg
    , insertionState : InsertionState msg
    }


type InsertionState msg
    = Inserted { onRemove : msg }
    | NotInserted { onInsert : msg, onDelete : msg }


type alias Symbol msg =
    { label : String
    , hovered : Bool
    , selected : Bool
    , onClick : msg
    , onHover : msg
    , onUnhover : msg
    }


view : Wme msg -> Element msg
view wme =
    row [ width fill, spaceEvenly ]
        [ viewWme wme
        , viewControls wme
        ]


viewControls : Wme msg -> Element msg
viewControls wme =
    let
        insertArgs =
            case wme.insertionState of
                Inserted { onRemove } ->
                    { label = text "=>", onPress = Just onRemove }

                NotInserted { onInsert } ->
                    { label = text "<=", onPress = Just onInsert }

        deleteOnPress =
            case wme.insertionState of
                Inserted _ ->
                    Nothing

                NotInserted { onDelete } ->
                    Just onDelete
    in
    row []
        [ Input.button [] insertArgs
        , Input.button []
            { label = text "X"
            , onPress = deleteOnPress
            }
        ]


viewWme : Wme msg -> Element msg
viewWme wme =
    let
        sym get =
            viewSymbol (get wme)
    in
    row []
        [ text <| String.fromInt wme.index ++ ": ("
        , sym .id
        , text " ^"
        , sym .attribute
        , text " "
        , sym .value
        , text ")"
        ]


viewSymbol : Symbol msg -> Element msg
viewSymbol symbol =
    let
        background =
            if symbol.selected then
                Palette.selected

            else if symbol.hovered then
                Palette.hovered

            else
                Palette.white
    in
    el
        [ Events.onClick symbol.onClick
        , Events.onMouseEnter symbol.onHover
        , Events.onMouseLeave symbol.onUnhover
        , Background.color background
        ]
    <|
        text symbol.label
