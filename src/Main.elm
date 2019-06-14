module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Color
import Debug exposing (todo)
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, fill, height, htmlAttribute, layout, maximum, padding, px, row, scrollbars, text, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, placeholder)
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html.Events.Extra exposing (onEnter)
import Html.Events.Extra.Mouse as Mouse
import IntDict
import Json.Decode as Decode
import Model.Production as Production exposing (Production, Test)
import Model.Rete as Rete exposing (Rete)
import Model.Symbols as Symbols exposing (Symbols)
import Model.Wme as Wme
import Model.Wmes as Wmes exposing (Wmes)
import Palette
import Parser
import Ports.Rete
import Time
import TypedSvg as Svg exposing (svg)
import TypedSvg.Attributes as SvgAttr exposing (class, stroke, viewBox)
import TypedSvg.Attributes.InPx as InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..))
import View.Production
import View.Wme



---- MODEL ----


type alias Model =
    { productionInput : String
    , productions : Dict Int Production
    , symbols : Symbols
    , selection : SymbolSelection
    , hover : SymbolSelection
    , productionEditor : ProductionEditor
    , wmeInput : String
    , wmes : Wmes
    , rete : Rete
    , network : Graph Entity ()
    , networkSimulation : Force.State NodeId
    , drag : Maybe Drag
    , events : List ReteMsg
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
                |> List.map (\p -> ( p.id, p ))
                |> Dict.fromList
      , symbols = Symbols.new
      , selection = NoSelection
      , hover = NoSelection
      , productionEditor = NotEditing
      , wmeInput = ""
      , wmes = Wme.testData |> List.foldl Wmes.add Wmes.empty
      , rete = Rete.empty
      , network = Graph.empty
      , networkSimulation = initForces Graph.empty
      , drag = Nothing
      , events = []
      }
    , Cmd.none
    )


initForces : Graph Entity () -> Force.State NodeId
initForces graph =
    let
        w =
            800

        h =
            600

        link { from, to } =
            ( from, to )

        forces =
            [ Force.center (w / 2) (h / 2)
            , Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            ]
    in
    Force.simulation forces



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
    | UserInsertedProduction Int
    | UserRemovedProduction Int
    | UserDeletedProduction Int
    | UserModifiedWme String
    | UserSubmittedWme
    | UserInsertedWme Int
    | UserRemovedWme Int
    | UserDeletedWme Int
    | Graph GraphMsg
    | ReteChanged ReteMsg
    | ReteUpdateTicked


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
            let
                nextId =
                    model.productions |> Dict.keys |> List.maximum |> Maybe.map ((+) 1) |> Maybe.withDefault 0
            in
            case Parser.run (Production.parser nextId "P") contents of
                Ok production ->
                    ( { model
                        | productions = model.productions |> Dict.insert production.id production
                        , productionEditor = NotEditing
                      }
                    , Cmd.none
                    )

                Err problems ->
                    ( { model | productionEditor = BadProduction contents problems }, Cmd.none )

        UserInsertedProduction id ->
            case model.productions |> Dict.get id of
                Just production ->
                    let
                        ( args, symbols ) =
                            model.symbols |> Symbols.step (Production.symbolGenerator production)
                    in
                    ( { model | symbols = symbols }, Ports.Rete.addProduction args )

                Nothing ->
                    ( model, Cmd.none )

        UserRemovedProduction id ->
            ( model, Ports.Rete.removeProduction { id = id } )

        UserDeletedProduction id ->
            ( { model | productions = model.productions |> Dict.remove id }, Cmd.none )

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

        UserInsertedWme id ->
            case model.wmes |> Wmes.get id of
                Just wme ->
                    let
                        ( args, symbols ) =
                            model.symbols |> Symbols.step (Wme.symbolGenerator wme)
                    in
                    ( { model | symbols = symbols }, Ports.Rete.addWme args )

                Nothing ->
                    ( model, Cmd.none )

        UserRemovedWme id ->
            ( model, Ports.Rete.removeWme { timetag = id } )

        UserDeletedWme id ->
            ( { model | wmes = model.wmes |> Wmes.remove id }, Cmd.none )

        Graph graphMsg ->
            ( model |> updateGraph graphMsg, Cmd.none )

        ReteChanged reteMsg ->
            ( { model | events = model.events ++ [ reteMsg ] }, Cmd.none )

        ReteUpdateTicked ->
            case model.events |> List.head of
                Just reteMsg ->
                    ( { model | events = model.events |> List.drop 1 } |> updateRete reteMsg, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


updateGraph : GraphMsg -> Model -> Model
updateGraph msg model =
    case msg of
        BrowserSentAnimationFrame time ->
            let
                ( newState, list ) =
                    Graph.nodes model.network
                        |> List.map .label
                        -- |> List.map
                        --     (\node ->
                        --         case node.value of
                        --             "Alpha" ->
                        --                 { node | vx = node.vx + 1 }
                        --             "Beta (root)" ->
                        --                 { node | vy = node.vy - 1 }
                        --             _ ->
                        --                 node
                        --     )
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
    case msg of
        Ports.Rete.Initialized args ->
            { model
                | network =
                    Graph.empty
                        |> Graph.insert
                            { node = Node args.dummyNodeId <| Force.entity args.dummyNodeId "Beta (root)"
                            , incoming = IntDict.empty
                            , outgoing = IntDict.empty
                            }
            }

        Ports.Rete.AddedNode args ->
            let
                withAlphaNodeIfPresent =
                    case args.alphaNodeId of
                        Just alphaId ->
                            IntDict.insert (alphaNodeId alphaId) ()

                        Nothing ->
                            identity

                ( x, y ) =
                    model.network
                        |> Graph.get args.parentId
                        |> Maybe.map (.node >> .label)
                        |> Maybe.map (\parent -> ( parent.x, parent.y + 30 ))
                        |> Maybe.withDefault ( 0, 0 )

                entity : Entity
                entity =
                    { id = args.id
                    , value = args.kind
                    , x = x
                    , y = y
                    , vx = 0
                    , vy = 0
                    }

                node =
                    { node = Node entity.id entity
                    , incoming = IntDict.singleton args.parentId () |> withAlphaNodeIfPresent
                    , outgoing = IntDict.empty
                    }
            in
            { model
                | network = model.network |> Graph.insert node
            }
                |> updateForces

        Ports.Rete.RemovedNode { id } ->
            { model
                | network = model.network |> Graph.remove id
            }
                |> updateForces

        Ports.Rete.AddedProduction { id, pNodeId } ->
            { model
                | productions = model.productions |> Dict.update id (Maybe.map (\p -> { p | inRete = Just pNodeId }))
            }

        Ports.Rete.RemovedProduction { id } ->
            { model
                | productions = model.productions |> Dict.update id (Maybe.map (\p -> { p | inRete = Nothing }))
            }

        Ports.Rete.AddedToken { id, parentId, nodeId } ->
            model

        Ports.Rete.RemovedToken { id } ->
            { model
                | rete = model.rete |> Rete.removeToken id
            }

        Ports.Rete.AddedWme args ->
            model

        Ports.Rete.RemovedWme { timetag } ->
            { model
                | wmes = model.wmes |> Wmes.remove timetag
            }

        Ports.Rete.AddedAlphaMemory { id } ->
            let
                ( x, y ) =
                    model.network
                        |> Graph.fold (\ctx ( currentX, currentY ) -> ( max currentX ctx.node.label.x, min currentY ctx.node.label.y )) ( 0, 1000 )
                        |> (\( maxX, minY ) -> ( maxX + 30, minY - 30 ))

                entity =
                    { id = alphaNodeId id
                    , value = "Alpha"
                    , x = x
                    , y = y
                    , vx = 0
                    , vy = 0
                    }

                node =
                    { node = Node entity.id entity
                    , incoming = IntDict.empty
                    , outgoing = IntDict.empty
                    }
            in
            { model
                | network = model.network |> Graph.insert node
            }
                |> updateForces

        Ports.Rete.RemovedAlphaMemory { id } ->
            { model
                | network = model.network |> Graph.remove (alphaNodeId id)
            }
                |> updateForces

        Ports.Rete.MatchedProduction _ ->
            model

        Ports.Rete.UnmatchedProduction _ ->
            model


updateForces : Model -> Model
updateForces model =
    { model | networkSimulation = initForces model.network }



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

        timelineSubscriptions =
            if List.isEmpty model.events then
                Sub.none

            else
                Time.every 100 (always ReteUpdateTicked)
    in
    Sub.batch
        [ graphSubscriptions
        , Ports.Rete.subscriptions |> Sub.map ReteChanged
        , timelineSubscriptions
        ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Interactive Rete"
    , body = [ layout [ Background.color Palette.background, height fill ] <| viewPage model ]
    }


viewPage : Model -> Element Msg
viewPage model =
    row [ centerX, width (fill |> maximum 1200), height fill, Background.color Palette.white ]
        [ el [ width (px 200), height fill ] <| viewTimeline model
        , el [ width fill, height fill ] <| viewGraph model.network
        , el [ width (px 300), height fill, scrollbars ] <| viewControls model
        ]


viewHeader : Element Msg
viewHeader =
    row [ width fill, padding 5, Background.color (Element.rgb 0.8 0.8 1.0) ]
        [ text "Interactive Rete"
        ]


viewTimeline : Model -> Element Msg
viewTimeline model =
    let
        viewReteEvent msg =
            case msg of
                Ports.Rete.Initialized _ ->
                    text "Initialize"

                Ports.Rete.AddedNode { id, kind } ->
                    text <| "+ " ++ kind ++ " node " ++ String.fromInt id

                Ports.Rete.RemovedNode { id } ->
                    text <| "- Node " ++ String.fromInt id

                Ports.Rete.AddedAlphaMemory { id } ->
                    text <| "+ Alpha node " ++ String.fromInt id

                Ports.Rete.RemovedAlphaMemory { id } ->
                    text <| "- Alpha node " ++ String.fromInt id

                Ports.Rete.AddedProduction { id } ->
                    text <| "Added production " ++ String.fromInt id

                Ports.Rete.RemovedProduction { id } ->
                    text <| "Removed production " ++ String.fromInt id

                event ->
                    text <| Debug.toString event
    in
    viewSection "Timeline" <|
        column [ width fill, scrollbars ]
            (model.events |> List.map viewReteEvent)


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
    in
    Element.html <|
        svg [ viewBox 0 0 800 600 ]
            [ Graph.edges network
                |> List.map (linkElement network)
                |> Svg.g [ class [ "links" ] ]
            , Graph.nodes network
                |> List.map nodeElement
                |> Svg.g [ class [ "nodes" ] ]
            ]


nodeElement : Node Entity -> Svg Msg
nodeElement node =
    let
        onMouseDown index =
            Mouse.onDown (.clientPos >> UserBeganDrag index >> Graph)

        nodeColor =
            case node.label.value of
                "Alpha" ->
                    Color.yellow

                "Beta" ->
                    Color.lightPurple

                "Join" ->
                    Color.green

                "P" ->
                    Color.gray

                _ ->
                    Color.black

        nodeRadius =
            case node.label.value of
                "Join" ->
                    5

                _ ->
                    10

        values =
            [ Svg.title [] [ TypedSvg.Core.text (node.label.value ++ " " ++ String.fromInt node.id) ] ]
    in
    case node.label.value of
        "Alpha" ->
            let
                size =
                    20
            in
            Svg.rect
                [ SvgAttr.fill (Fill nodeColor)
                , stroke Color.black
                , strokeWidth 1
                , onMouseDown node.id
                , InPx.x <| node.label.x - (size / 2)
                , InPx.y <| node.label.y - (size / 2)
                , InPx.width size
                , InPx.height size
                ]
                values

        _ ->
            Svg.circle
                [ r nodeRadius
                , SvgAttr.fill (Fill nodeColor)
                , stroke (Color.rgba 0 0 0 1)
                , strokeWidth 1
                , onMouseDown node.id
                , cx node.label.x
                , cy node.label.y
                ]
                values


viewControls : Model -> Element Msg
viewControls model =
    column [ width fill ]
        [ viewSection "WMEs" <| viewWmes model
        , viewSection "Productions" <| viewProductions model
        , viewSection "Symbols" <| viewSymbols model
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
            model.symbols
                |> Symbols.toList
                |> List.map (\( _, name ) -> { name = name })

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

            -- , insertionState = View.Wme.Inserted { onRemove = UserRemovedWme index }
            , insertionState = View.Wme.NotInserted { onInsert = UserInsertedWme index, onDelete = UserDeletedWme index }
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
        toViewModel : Production -> View.Production.Production Msg
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
            , insertionState =
                case production.inRete of
                    Just _ ->
                        View.Production.Inserted
                            { onRemove = UserRemovedProduction production.id
                            }

                    Nothing ->
                        View.Production.NotInserted
                            { onInsert = UserInsertedProduction production.id
                            , onDelete = UserDeletedProduction production.id
                            }
            }

        editor =
            case model.productionEditor of
                NotEditing ->
                    Input.button []
                        { label = text "Add Production", onPress = Just UserActivatedEditor }

                Editing contents ->
                    viewProductionEditor contents

                BadProduction contents problems ->
                    column [ width fill ]
                        [ viewProductionEditor contents
                        , el [ Background.color (Element.rgb 1.0 0.5 0.5), Events.onClick UserActivatedEditor ] <| text (Debug.toString problems)
                        ]
    in
    column [ width fill ]
        [ editor
        , column [ width fill ]
            (model.productions
                |> Dict.values
                |> List.map (toViewModel >> View.Production.view)
            )
        ]


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



---- HELPERS ----


{-| Alpha nodes and beta network nodes have separate id spaces, so if
we simply put them both in the same graph then we would have
collisions. A possible solution would be to make all alpha nodes
negative, but the default node position for non-positive values causes
issues. Adding a positive constant to the node id works okay, but it
assumes that the ids will never reach that number.
-}
alphaNodeId : Int -> Int
alphaNodeId id =
    id + 10000
