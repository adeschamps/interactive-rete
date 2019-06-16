module View.Symbol exposing (Symbol, view)

import Element exposing (Element, el, text)
import Element.Background as Background
import Element.Events as Events
import Palette


type alias Symbol msg =
    { name : String
    , hovered : Bool
    , selected : Bool
    , onClick : msg
    , onHover : msg
    , onUnhover : msg
    }


view : Symbol msg -> Element msg
view { selected, hovered, onClick, onHover, onUnhover, name } =
    let
        background =
            if selected then
                Palette.selected

            else if hovered then
                Palette.hovered

            else
                Palette.white
    in
    el
        [ Events.onClick onClick
        , Events.onMouseEnter onHover
        , Events.onMouseLeave onUnhover
        , Background.color background
        ]
    <|
        text name
