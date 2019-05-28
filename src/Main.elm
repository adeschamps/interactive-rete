module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Color
import Debug exposing (todo)
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, explain, fill, height, htmlAttribute, image, layout, maximum, padding, px, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, placeholder)
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html.Events.Extra exposing (onEnter)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import Model.Production as Production exposing (Production, Test)
import Model.Rete as Rete exposing (Rete)
import Model.Wme as Wme exposing (Wme)
import Model.Wmes as Wmes exposing (Wmes)
import Palette
import Parser
import Ports.Rete
import Time
import TypedSvg as Svg exposing (svg)
import TypedSvg.Attributes as SvgAttr exposing (class, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..))
import View.Production
import View.Wme



---- MODEL ----


type alias Model =
    { productionInput : String
    , productions : Dict String Production
    , symbols : Dict Int String
    , selection : SymbolSelection
    , hover : SymbolSelection
    , productionEditor : ProductionEditor
    , wmeInput : String
    , wmes : Wmes
    , rete : Rete
    , network : Graph Entity ()
    , networkSimulation : Force.State NodeId
    , drag : Maybe Drag
    }


type alias Entity =
    Force.Entity NodeId { value : String }


type SymbolSelection
    = NoSelection
    | ConstantSelection String
    | VariableSelection String String


type ProductionEditor
    = NotEditing
    | Editing String
    | BadProduction String (List Parser.DeadEnd)


type alias Drag =
    { start : ( Float, Float )
    , current : ( Float, Float )
    , index : NodeId
    }


init : ( Model, Cmd Msg )
init =
    ( { productionInput = ""
      , productions =
            Production.testData
                |> List.map (\p -> ( p.name, p ))
                |> Dict.fromList
      , symbols = Dict.empty
      , selection = NoSelection
      , hover = NoSelection
      , productionEditor = NotEditing
      , wmeInput = ""
      , wmes = Wme.testData |> List.foldl Wmes.add Wmes.empty
      , rete = Rete.empty
      , network = initGraph
      , networkSimulation = initForces
      , drag = Nothing
      }
    , Cmd.none
    )


initGraph : Graph Entity ()
initGraph =
    let
        makeNode : Graph.NodeContext Rete.Node e -> Graph.NodeContext Entity e
        makeNode ctx =
            { node = Node ctx.node.id <| Force.entity ctx.node.id (Debug.toString ctx.node.label)
            , incoming = ctx.incoming
            , outgoing = ctx.outgoing
            }
    in
    Rete.empty.network |> Graph.mapContexts makeNode


initForces : Force.State NodeId
initForces =
    let
        graph =
            initGraph

        w =
            800

        h =
            600

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (w / 2) (h / 2)
            ]
    in
    Force.simulation forces |> Force.iterations 100



---- UPDATE ----


type Msg
    = UserModifiedProduction String
    | UserHoveredTest String Test
    | UserUnhoveredTest
    | UserSelectedTest String Test
    | UserSelectedSymbol String
    | UserHoveredSymbol String
    | UserUnhoveredSymbol
    | UserActivatedEditor
    | UserLeftEditor String
    | UserModifiedWme String
    | UserSubmittedWme
    | UserInsertedWme Int
    | UserRemovedWme Int
    | UserDeletedWme Int
    | Graph GraphMsg
    | ReteChanged ReteMsg


type GraphMsg
    = BrowserSentAnimationFrame Time.Posix
    | UserBeganDrag NodeId ( Float, Float )
    | UserContinuedDrag ( Float, Float )
    | UserEndedDrag ( Float, Float )


type alias ReteMsg =
    Ports.Rete.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserModifiedProduction contents ->
            ( { model | productionEditor = Editing contents }, Cmd.none )

        UserHoveredTest productionName test ->
            let
                hover =
                    case test of
                        Production.ConstantTest value ->
                            ConstantSelection value

                        Production.VariableTest name ->
                            VariableSelection productionName name
            in
            ( { model | hover = hover }, Cmd.none )

        UserUnhoveredTest ->
            ( { model | hover = NoSelection }, Cmd.none )

        UserSelectedTest productionName test ->
            let
                selection =
                    case test of
                        Production.ConstantTest value ->
                            ConstantSelection value

                        Production.VariableTest name ->
                            VariableSelection productionName name
            in
            ( { model | selection = selection }, Cmd.none )

        UserSelectedSymbol value ->
            ( { model | selection = ConstantSelection value }, Cmd.none )

        UserHoveredSymbol value ->
            ( { model | hover = ConstantSelection value }, Cmd.none )

        UserUnhoveredSymbol ->
            ( { model | hover = NoSelection }, Cmd.none )

        UserActivatedEditor ->
            let
                initialText =
                    case model.productionEditor of
                        NotEditing ->
                            ""

                        Editing contents ->
                            contents

                        BadProduction contents problems ->
                            contents
            in
            ( { model | productionEditor = Editing initialText }, Cmd.none )

        UserLeftEditor contents ->
            case Parser.run (Production.parser "P") contents of
                Ok production ->
                    ( { model
                        | productions = model.productions |> Dict.insert production.name production
                        , productionEditor = NotEditing
                      }
                    , Cmd.none
                    )

                Err problems ->
                    ( { model | productionEditor = BadProduction contents problems }, Cmd.none )

        UserModifiedWme content ->
            ( { model | wmeInput = content }, Cmd.none )

        UserSubmittedWme ->
            case Parser.run Wme.parser model.wmeInput of
                Ok wme ->
                    ( { model
                        | wmeInput = ""
                        , wmes = model.wmes |> Wmes.add wme
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        UserInsertedWme _ ->
            ( model, Cmd.none )

        UserRemovedWme _ ->
            ( model, Cmd.none )

        UserDeletedWme _ ->
            ( model, Cmd.none )

        Graph graphMsg ->
            ( model |> updateGraph graphMsg, Cmd.none )

        ReteChanged reteMsg ->
            ( model |> updateRete reteMsg, Cmd.none )


updateGraph : GraphMsg -> Model -> Model
updateGraph msg model =
    case msg of
        BrowserSentAnimationFrame time ->
            let
                ( newState, list ) =
                    Graph.nodes model.network
                        |> List.map .label
                        |> Force.tick model.networkSimulation

                updateContextWithValue ctx value =
                    let
                        node =
                            ctx.node
                    in
                    { ctx | node = { node | label = value } }

                graphUpdater value =
                    Maybe.map (\ctx -> updateContextWithValue ctx value)

                newNetwork =
                    list |> List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph) model.network
            in
            { model | network = newNetwork, networkSimulation = newState }

        UserBeganDrag index xy ->
            { model
                | drag = Just (Drag xy xy index)
                , networkSimulation = Force.reheat model.networkSimulation
            }

        UserContinuedDrag xy ->
            case model.drag of
                Just { start, index } ->
                    { model
                        | drag = Just (Drag start xy index)
                        , network = model.network |> moveNodeTo index xy
                        , networkSimulation = Force.reheat model.networkSimulation
                    }

                Nothing ->
                    model

        UserEndedDrag xy ->
            case model.drag of
                Just { index } ->
                    { model
                        | drag = Nothing
                        , network = model.network |> moveNodeTo index xy
                    }

                Nothing ->
                    model


moveNodeTo : NodeId -> ( Float, Float ) -> Graph Entity a -> Graph Entity a
moveNodeTo index ( x, y ) =
    let
        updateNode : NodeContext Entity a -> NodeContext Entity a
        updateNode ctx =
            let
                node =
                    ctx.node

                label =
                    node.label
            in
            { ctx | node = { node | label = { label | x = x, y = y } } }
    in
    Graph.update index (Maybe.map updateNode)


updateRete : ReteMsg -> Model -> Model
updateRete msg model =
    model



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        graphSubscriptions =
            case model.drag of
                Nothing ->
                    if Force.isCompleted model.networkSimulation then
                        Sub.none

                    else
                        Browser.Events.onAnimationFrame (BrowserSentAnimationFrame >> Graph)

                Just _ ->
                    Sub.batch
                        [ Browser.Events.onMouseMove (Decode.map (.clientPos >> UserContinuedDrag) Mouse.eventDecoder)
                        , Browser.Events.onMouseUp (Decode.map (.clientPos >> UserEndedDrag) Mouse.eventDecoder)
                        , Browser.Events.onAnimationFrame BrowserSentAnimationFrame
                        ]
                        |> Sub.map Graph
    in
    Sub.batch
        [ graphSubscriptions
        , Ports.Rete.subscriptions |> Sub.map ReteChanged
        ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Interactive Rete"
    , body = [ layout [ Background.color Palette.background ] <| viewPage model ]
    }


viewPage : Model -> Element Msg
viewPage model =
    column [ centerX, width (fill |> maximum 1200), height fill, Background.color Palette.white ]
        [ viewHeader
        , row [ width fill, height fill ]
            [ viewGraph model.network
            , viewControls model
            ]
        ]


viewHeader : Element Msg
viewHeader =
    row [ width fill, padding 5, Background.color (Element.rgb 0.8 0.8 1.0) ]
        [ text "Interactive Rete"
        ]


viewGraph : Graph Entity e -> Element Msg
viewGraph network =
    let
        linkElement : Graph Entity e -> Edge e -> Svg Msg
        linkElement graph edge =
            let
                source =
                    Graph.get edge.from graph
                        |> Maybe.map (.node >> .label)
                        |> Maybe.withDefault (Force.entity 0 "")

                target =
                    Graph.get edge.to graph
                        |> Maybe.map (.node >> .label)
                        |> Maybe.withDefault (Force.entity 0 "")
            in
            Svg.line
                [ x1 source.x
                , y1 source.y
                , x2 target.x
                , y2 target.y
                , stroke (Color.rgb 0 0 0)
                ]
                []

        onMouseDown index =
            Mouse.onDown (.clientPos >> UserBeganDrag index >> Graph)

        nodeElement : Node Entity -> Svg Msg
        nodeElement node =
            Svg.circle
                [ r 2.5
                , SvgAttr.fill (Fill Color.black)
                , stroke (Color.rgba 0 0 0 1)
                , strokeWidth 7
                , onMouseDown node.id
                , cx node.label.x
                , cy node.label.y
                ]
                [ Svg.title [] [ TypedSvg.Core.text node.label.value ] ]
    in
    el [ width fill ] <|
        Element.html <|
            svg [ viewBox 0 0 800 600 ]
                [ Graph.edges network
                    |> List.map (linkElement network)
                    |> Svg.g [ class [ "links" ] ]
                , Graph.nodes network
                    |> List.map nodeElement
                    |> Svg.g [ class [ "nodes" ] ]
                ]



-- image [ width fill, centerX ] { src = "/logo.svg", description = "The Elm Logo" }


viewControls : Model -> Element Msg
viewControls model =
    column [ height fill, width (px 300) ]
        [ viewSection "Symbols" <| viewSymbols model
        , viewSection "WMEs" <| viewWmes model
        , viewSection "Productions" <| viewProductions model
        , viewSection "Debug" <| viewDebug model
        ]


viewSection : String -> Element Msg -> Element Msg
viewSection title body =
    column [ width fill, padding 5 ]
        [ el [ width fill, Font.size 18, padding 5, Background.color Palette.sectionHeading ] <| text title
        , body
        ]


viewSymbols : Model -> Element Msg
viewSymbols model =
    let
        symbols =
            [ { name = "superstate" }
            , { name = "S1" }
            , { name = "O1" }
            , { name = "nil" }
            ]

        viewSymbol symbol =
            Element.text symbol.name
    in
    column [] (symbols |> List.map viewSymbol)


viewWmes : Model -> Element Msg
viewWmes model =
    let
        background =
            case Parser.run Wme.parser model.wmeInput of
                Ok _ ->
                    Background.color (Element.rgb 1 1 1)

                Err problems ->
                    Background.color (Element.rgb 1 0.8 0.8)

        wmeInput =
            Input.text [ background, htmlAttribute (onEnter UserSubmittedWme) ]
                { onChange = UserModifiedWme
                , text = model.wmeInput
                , placeholder = Just (placeholder [] <| text "Enter a WME")
                , label = labelHidden "WME Input"
                }

        toViewModel ( index, wme ) =
            let
                prepSymbol sym =
                    { label = sym
                    , hovered =
                        case model.hover of
                            NoSelection ->
                                False

                            ConstantSelection value ->
                                sym == value

                            VariableSelection _ _ ->
                                False
                    , selected =
                        case model.selection of
                            NoSelection ->
                                False

                            ConstantSelection value ->
                                sym == value

                            VariableSelection _ _ ->
                                False
                    , onClick = UserSelectedSymbol sym
                    , onHover = UserHoveredSymbol sym
                    , onUnhover = UserUnhoveredSymbol
                    }
            in
            { index = index
            , id = prepSymbol wme.id
            , attribute = prepSymbol wme.attribute
            , value = prepSymbol wme.value
            , insertionState = View.Wme.Inserted { onRemove = UserRemovedWme index }
            }

        wmeList =
            column [ width fill ]
                (model.wmes |> Wmes.toList |> List.map (toViewModel >> View.Wme.view))
    in
    column []
        [ wmeInput
        , wmeList
        ]


viewProductions : Model -> Element Msg
viewProductions model =
    let
        toViewModel production =
            let
                prepCondition { id, attribute, value } =
                    { id = prepTest id
                    , attribute = prepTest attribute
                    , value = prepTest value
                    }

                prepTest test =
                    let
                        ( name, constant ) =
                            case test of
                                Production.ConstantTest n ->
                                    ( n, True )

                                Production.VariableTest n ->
                                    ( n, False )
                    in
                    { name = name
                    , constant = constant
                    , hovered =
                        case model.hover of
                            NoSelection ->
                                False

                            ConstantSelection value ->
                                constant && name == value

                            VariableSelection productionName variable ->
                                not constant && productionName == production.name && name == variable
                    , selected =
                        case model.selection of
                            NoSelection ->
                                False

                            ConstantSelection value ->
                                constant && name == value

                            VariableSelection productionName variable ->
                                not constant && productionName == production.name && name == variable
                    , onClick = UserSelectedTest production.name test
                    , onHover = UserHoveredTest production.name test
                    , onUnhover = UserUnhoveredTest
                    }
            in
            { name = production.name
            , conditions = production.conditions |> List.map prepCondition
            }
    in
    case model.productionEditor of
        NotEditing ->
            column [ width fill, Events.onClick UserActivatedEditor ]
                (model.productions
                    |> Dict.values
                    |> List.map (toViewModel >> View.Production.view)
                )

        Editing contents ->
            viewProductionEditor contents

        BadProduction contents problems ->
            el [ Background.color (Element.rgb 1.0 0.5 0.5), Events.onClick UserActivatedEditor ] <| text (Parser.deadEndsToString problems)


viewProductionEditor : String -> Element Msg
viewProductionEditor contents =
    Input.multiline [ Events.onLoseFocus (UserLeftEditor contents) ]
        { onChange = UserModifiedProduction
        , text = contents
        , placeholder = Just (placeholder [] <| text "Enter a production")
        , label = labelHidden "production"
        , spellcheck = False
        }


viewDebug : Model -> Element Msg
viewDebug model =
    column []
        [ text (Debug.toString model.drag)
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
