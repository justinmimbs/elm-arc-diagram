import AcyclicDigraph exposing (Node, Edge, Cycle, AcyclicDigraph)
import Diagram
import DiagramConnectivity as Diagram
import Set exposing (Set)
import Dict exposing (Dict)
import Html exposing (Html)


main : Program Never Model Node
main =
  Html.beginnerProgram
    { model =  Model exampleEdges exampleLabels Nothing
    , update = update
    , view = view
    }


type alias Model =
  { edges : Set Edge
  , labels : Dict Node String
  , selectedNode : Maybe Node
  }


update : Node -> Model -> Model
update node model =
  { model | selectedNode = model.selectedNode |> toggleMaybe node }


defaultLayout =  Diagram.defaultLayout


layout : Diagram.Layout
layout =
  { defaultLayout
    | edgeRadius = 200
    , labelMaxWidth = 60
  }


view : Model -> Html Node
view { edges, labels, selectedNode } =
  let
    toLabel = (flip Dict.get) labels >> Maybe.withDefault ""
  in
    case AcyclicDigraph.fromEdges edges of
      Err cycles ->
        Html.div
          []
          [ Html.text "Graph has the following cycles:"
          , Html.ol
              []
              (cycles |> List.map (viewCycle toLabel))
          ]

      Ok graph ->
        let
          paint =
            selectedNode
              |> Maybe.map
                  (Diagram.basicPaintConnectivity toLabel graph)
              |> Maybe.withDefault
                  (Diagram.basicPaint toLabel)
        in
          Diagram.view
            layout
            paint
            graph


viewCycle : (Node -> String) -> Cycle -> Html a
viewCycle toLabel cycle =
  Html.li
    []
    [ Html.text (cycle |> List.map toLabel |> String.join " -> ") ]


toggleMaybe : a -> Maybe a -> Maybe a
toggleMaybe a ma =
  if ma == Just a then
    Nothing
  else
    Just a


--

exampleEdges : Set Edge
exampleEdges =
  Set.fromList
    [ (2, 1)
    , (3, 1)
    , (3, 2)
    , (4, 2)
    , (5, 3)
    , (5, 4)
  --, (5, 9) -- make cycle
    , (6, 4)
    , (7, 4)
    , (8, 1)
    , (8, 3)
    , (8, 4)
    , (8, 6)
    , (9, 1)
    , (9, 3)
    , (9, 5)
    , (9, 6)
    ]


exampleLabels : Dict Node String
exampleLabels =
  Dict.fromList
    [ (1, "Alfa")
    , (2, "Bravo")
    , (3, "Charlie")
    , (4, "Delta")
    , (5, "Echo")
    , (6, "Foxtrot")
    , (7, "Golf")
    , (8, "Hotel")
    , (9, "India")
    ]