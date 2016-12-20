module DiagramConnectivity exposing
  ( paintConnectivity
  , basicPaintConnectivity
  )

import AcyclicDigraph exposing (AcyclicDigraph)
import Dict exposing (Dict)
import Diagram
import Digraph exposing (Node, Edge)
import Html exposing (Html)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes


type alias Distance =
  Maybe Int


paintConnectivity viewLabel colorNode colorEdge graph node =
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


--basicPaintConnectivity : (Node -> String) -> AcyclicDigraph -> Node -> Paint
basicPaintConnectivity toLabel =
  paintConnectivity
    (\n d -> viewLabelDimmed (isNothing d) (toLabel n))
    (always colorFromDistance)
    (always colorFromDistance)


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


labelColor : Bool -> String
labelColor isDimmed =
  if isDimmed then
    "rgb(200, 200, 200)"
  else
    "black"


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
