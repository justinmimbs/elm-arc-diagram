module ArcDiagram.Distance exposing
  ( Distance, DistancePaint
  , paint, basicPaint, defaultDistancePaint
  )

{-| This module provides convenience functions for creating `ArcDiagram.Paint`
values that will color nodes and edges based on _distance_ from a given node.
This is useful for highlighting the subgraph which is reachable from a
given node.


## Distance

@docs Distance


## Paint

@docs basicPaint, DistancePaint, defaultDistancePaint, paint
-}

import AcyclicDigraph exposing (AcyclicDigraph, Node, Edge)
import ArcDiagram exposing (Paint)
import Dict exposing (Dict)
import Digraph
import Html exposing (Html)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes


{-| Represents the distance between two nodes. A value of `Just number`
indicates the number of edges in a shortest path connecting the nodes. A value
of `Nothing` means there is no path connecting the nodes.

Unconventionally, the number may be negative, which indicates there is a path
in the reverse direction. This way `Distance` can represent both forward and
backward connections.
-}
type alias Distance =
  Maybe Int


{-| Similar to `Paint`, but each function also takes a `Distance` argument.
-}
type alias DistancePaint =
  { viewLabel : (Node -> Distance -> Svg Node)
  , colorNode : (Node -> Distance -> String)
  , colorEdge : (Edge -> Distance -> String)
  }


{-| Get a `Paint` value from a `DistancePaint` value, a graph, and a node.

    colorFromDistance : Distance -> String
    colorFromDistance distance =
      case distance of
        Just _ ->
          "black"

        Nothing ->
          "lightgray"


    view : AcyclicDigraph -> Node -> Html Node
    view graph node =
      ArcDiagram.view
        ArcDiagram.defaultLayout
        (ArcDiagram.Distance.paint
          { viewLabel = \n d -> viewColorLabel (colorFromDistance d) (toLabel n)
          , colorNode = always colorFromDistance
          , colorEdge = always colorFromDistance
          }
          graph
          node
        )
        graph
-}
paint : DistancePaint -> AcyclicDigraph -> Node -> Paint
paint { viewLabel, colorNode, colorEdge } graph node =
  let
    outgoing = graph |> AcyclicDigraph.toEdges |> Digraph.toAdjacencyList
    incoming = Digraph.transpose outgoing
    -- signed distances
    distancesFrom =
      Dict.union
        (outgoing |> Digraph.distancesFrom node)
        (incoming |> Digraph.distancesFrom node |> Dict.map (always negate))
  in
      { viewLabel = \node -> viewLabel node (Dict.get node distancesFrom)
      , colorNode = \node -> colorNode node (Dict.get node distancesFrom)
      , colorEdge = \edge -> colorEdge edge (distanceForEdge distancesFrom edge)
      }


{-| The `defaultDistancePaint` will color connected nodes and edges blue or red
(for forward or backward connections), and color unconnected nodes, edges, and
labels light gray.
-}
defaultDistancePaint : DistancePaint
defaultDistancePaint =
  { viewLabel = \n d -> viewLabelDimmed (isNothing d) (toString n)
  , colorNode = always colorFromDistance
  , colorEdge = always colorFromDistance
  }


{-| Get a `Paint` value that uses the `defaultDistancePaint` coloring and your
own label text, by providing a _toLabel_ function, a graph, and a node.

    ArcDiagram.view
      ArcDiagram.defaultLayout
      (ArcDiagram.Distance.basicPaint toLabel graph node)
      graph

See the [Selectable Node](https://github.com/justinmimbs/elm-arc-diagram)
example for more detail.
-}
basicPaint : (Node -> String) -> AcyclicDigraph -> Node -> Paint
basicPaint toLabel =
  paint
    { defaultDistancePaint
      | viewLabel = \n d -> viewLabelDimmed (isNothing d) (toLabel n)
    }


viewLabelDimmed : Bool -> String -> Svg a
viewLabelDimmed isDimmed string =
  Svg.text_
    [ Svg.Attributes.x "4px"
    , Svg.Attributes.fontFamily "Helvetica, Arial"
    , Svg.Attributes.fontSize "12px"
    , Svg.Attributes.dominantBaseline "middle"
    , Svg.Attributes.fill (labelColor isDimmed)
    ]
    [ Svg.text string
    ]


labelColor : Bool -> String
labelColor isDimmed =
  if isDimmed then
    "rgb(200, 200, 200)"
  else
    "black"


distanceForEdge : Dict Node Int -> Edge -> Distance
distanceForEdge distancesFrom (a, b) =
  Maybe.map2
    (\da db ->
      if da >= 0 && db >= 0 then
        Just da
      else if da <= 0 && db <= 0 then
        Just db
      else
        Nothing
    )
    (Dict.get a distancesFrom)
    (Dict.get b distancesFrom)
  |> Maybe.withDefault Nothing


colorFromDistance : Distance -> String
colorFromDistance distance =
  case distance of
    Just d ->
      if d == 0 then
        "black"
      else
        let
          colorFromAlpha = if d > 0 then blueFromAlpha else redFromAlpha
        in
          colorFromAlpha <| 1 - ((min 3 (toFloat (abs d)) - 1) * 0.3)
    Nothing ->
      "rgba(0, 0, 0, 0.2)"


blueFromAlpha : Float -> String
blueFromAlpha =
  rgba 35 135 206


redFromAlpha : Float -> String
redFromAlpha =
  rgba 224 69 39


rgba : Int -> Int -> Int -> Float -> String
rgba r g b a =
  "rgba("
  ++ (List.map toString [ r, g, b ] ++ [ toString a ] |> String.join ", ")
  ++ ")"


-- Maybe extra

isNothing : Maybe a -> Bool
isNothing m =
  case m of
    Just _  -> False
    Nothing -> True
