module DiagramConnectivity exposing
  ( view
  , viewWithOptions, Options, defaultOptions
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


type alias Options =
  { edgeSpacing : Int
  , nodePadding : Int
  , yMinSpacing : Int
  , edgeRadius : Int
  , labelMaxWidth : Int
  , colorNode : Distance -> Node -> String
  , colorEdge : Distance -> Edge -> String
  , viewLabel : Distance -> Node -> Svg Node
  }


diagramDefaultOptions = Diagram.defaultOptions

defaultOptions : Options
defaultOptions =
  { edgeSpacing = diagramDefaultOptions.edgeSpacing
  , nodePadding = diagramDefaultOptions.nodePadding
  , yMinSpacing = diagramDefaultOptions.yMinSpacing
  , edgeRadius = diagramDefaultOptions.edgeRadius
  , labelMaxWidth = diagramDefaultOptions.labelMaxWidth
  , colorNode = \d _ -> colorFromDistance d
  , colorEdge = \d _ -> colorFromDistance d
  , viewLabel = \d n -> viewLabel (isNothing d) (toString n)
  }


viewLabel : Bool -> String -> Svg a
viewLabel isDimmed string =
  Svg.text_
    [ Svg.Attributes.x "4px"
    , Svg.Attributes.fontFamily "Helvetica, Arial"
    , Svg.Attributes.fontSize "12px"
    , Svg.Attributes.dominantBaseline "middle"
    , Svg.Attributes.fill (if isDimmed then "gray" else "black")
    ]
    [ Svg.text string
    ]


view : (Node -> String) -> Node -> AcyclicDigraph -> Html Node
view stringFromNode =
  viewWithOptions
    { defaultOptions
      | viewLabel = \d n -> viewLabel (isNothing d) (stringFromNode n)
    }


viewWithOptions : Options -> Node -> AcyclicDigraph -> Html Node
viewWithOptions options node graph =
  let
    outgoing = graph |> AcyclicDigraph.toEdges |> Digraph.toAdjacencyList
    incoming = Digraph.transpose outgoing
    -- signed distances
    distancesFrom =
      Dict.union
        (outgoing |> Digraph.distancesFrom node)
        (incoming |> Digraph.distancesFrom node |> Dict.map (always negate))
  in
    Diagram.viewWithOptions
      { edgeSpacing = options.edgeSpacing
      , nodePadding = options.nodePadding
      , yMinSpacing = options.yMinSpacing
      , edgeRadius = options.edgeRadius
      , labelMaxWidth = options.labelMaxWidth
      , colorNode = \node -> options.colorNode (Dict.get node distancesFrom) node
      , colorEdge = \edge -> options.colorEdge (distanceForEdge distancesFrom edge) edge
      , viewLabel = \node -> options.viewLabel (Dict.get node distancesFrom) node
      }
      graph


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
