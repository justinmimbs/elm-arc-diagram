module Diagram exposing
  ( GraphData, graphDataFromEdges
  , LayoutConfig, defaultLayoutConfig
  , DrawingConfig, defaultDrawingConfig
  , view, viewWithConfig
  )

import Dict exposing (Dict)
import Digraph exposing (Node, Edge, AdjacencyList, toAdjacencyList, transpose, degree, topologicalRank, topologicalSortBy)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)
import Svg exposing (Svg, svg, g, path, rect, text_, text)
import Svg.Attributes exposing (x, y, width, height, transform, strokeLinecap, d, stroke, fill)
import Svg.Events exposing (onClick)


-- "graph" here specifically means "acyclic digraph"

type alias GraphData =
  (Set Edge, Dict Node Int)


graphDataFromEdges : Set Edge -> Maybe GraphData
graphDataFromEdges edges =
  edges
    |> topologicalRank
    |> Maybe.map ((,) edges)


type alias LayoutConfig =
  { edgeSpacing : Int
  , nodePadding : Int
  , yMinSpacing : Int
  , edgeRadius : Int
  , labelMaxWidth : Int
  }


defaultLayoutConfig : LayoutConfig
defaultLayoutConfig =
  { edgeSpacing = 2
  , nodePadding = 4
  , yMinSpacing = 20
  , edgeRadius = 4
  , labelMaxWidth = 300
  }


type alias DrawingConfig =
  { colorNode : (Node -> String)
  , colorEdge : (Edge -> String)
  , viewLabel : (Node -> Svg Node)
  }


defaultDrawingConfig : DrawingConfig
defaultDrawingConfig =
  { colorNode = always "black"
  , colorEdge = always "gray"
  , viewLabel = toString >> defaultViewLabel
  }


defaultViewLabel : String -> Svg a
defaultViewLabel string =
  text_
    [ x (4 |> px)
    , Svg.Attributes.fontFamily "Helvetica, Arial"
    , Svg.Attributes.fontSize (12 |> px)
    , Svg.Attributes.dominantBaseline "middle"
    ]
    [ text string
    ]


--

functionFromDict : v -> Dict comparable v -> comparable -> v
functionFromDict default dict =
  (flip Dict.get) dict >> Maybe.withDefault default


centeringOffset : Int -> Int -> Int
centeringOffset outer inner =
  max 0 ((outer - inner) // 2)


layoutNodes : LayoutConfig -> AdjacencyList -> AdjacencyList -> List Node -> Dict Node Rect
layoutNodes { edgeSpacing, nodePadding, yMinSpacing } incoming outgoing ordered =
  List.foldl
    (\n ((cursorX, cursorY), dict) ->
      let
        indegree = degree incoming n
        outdegree = degree outgoing n
        width = (outdegree * edgeSpacing + nodePadding * 2)
        height = (indegree * edgeSpacing + nodePadding * 2)
        -- center rect within yMinSpacing
        yOffset = centeringOffset yMinSpacing height
        rect =
          Rect
            cursorX
            (cursorY + yOffset)
            width
            height
      in
        ( (cursorX + rect.width, cursorY + (max yMinSpacing rect.height))
        , dict |> Dict.insert n rect
        )
    )
    ( (0, 0)
    , Dict.empty
    )
    ordered
  |> Tuple.second


layoutEdges : Set Edge -> List Node -> Dict Edge (Int, Int)
layoutEdges edges ordered =
  let
    ordinalFromNode : Node -> Int
    ordinalFromNode =
      ordered
        |> List.indexedMap
            (flip (,))
        |> Dict.fromList
        |> functionFromDict -1

    orderedEdges : List Edge
    orderedEdges =
      edges
        |> Set.toList
        |> List.sortBy (\(a, b) -> (ordinalFromNode a, ordinalFromNode b))

  in
    List.foldl
      (\(a, b) (dict, (outgoing, incoming)) ->
        let
          aOutOrdinal = Dict.get a outgoing |> Maybe.withDefault 0
          bInOrdinal = Dict.get b incoming |> Maybe.withDefault 0
        in
          ( dict |> Dict.insert (a, b) (aOutOrdinal, bInOrdinal)
          , ( outgoing |> Dict.insert a (aOutOrdinal + 1)
            , incoming |> Dict.insert b (bInOrdinal + 1)
            )
          )
      )
      (Dict.empty, (Dict.empty, Dict.empty))
      orderedEdges
    |> Tuple.first


listTopNodes : Dict Node Int -> List Node -> List Node
listTopNodes nodeToRank ordered =
  List.foldl
    (\n (ns, rank) ->
      let
        nRank = Dict.get n nodeToRank |> Maybe.withDefault 0
      in
        if nRank == rank then
          (ns, rank)
        else
          (n :: ns, nRank)
    )
    ([], -1)
    ordered
  |> Tuple.first
  |> List.reverse


calculateSize : LayoutConfig -> List Node -> (Node -> Rect) -> Coord
calculateSize layout ordered rectFromNode =
  let
    lastRect = ordered |> List.reverse |> List.head |> Maybe.map rectFromNode |> Maybe.withDefault emptyRect
  in
    addCoord
      (rectTopRight lastRect)
      (layout.labelMaxWidth, max layout.yMinSpacing lastRect.height)


view : (Node -> String) -> GraphData -> Html Node
view nodeToString =
  viewWithConfig
    defaultLayoutConfig
    { defaultDrawingConfig
      | viewLabel = nodeToString >> defaultViewLabel
    }


viewWithConfig : LayoutConfig -> DrawingConfig -> GraphData -> Html Node
viewWithConfig layout drawing (edges, nodeToRank) =
  let
    outgoing = edges |> toAdjacencyList
    incoming = outgoing |> transpose
    -- order same-rank nodes by: incoming degree, outgoing degree descending
    ordered = topologicalSortBy (\n -> (degree incoming n, degree outgoing n |> negate)) nodeToRank
    topNodes = listTopNodes nodeToRank ordered

    -- layout dicts
    nodeToRect = layoutNodes layout incoming outgoing ordered
    edgeToConnectionOrdinals = layoutEdges edges ordered

    -- layout functions
    connectionOrdinalsFromEdge =
      functionFromDict (0, 0) edgeToConnectionOrdinals

    rectFromNode =
      functionFromDict emptyRect nodeToRect

    connectionShift : Int -> Int
    connectionShift ordinal =
      ordinal * layout.edgeSpacing + layout.nodePadding

    (w, h) = calculateSize layout ordered rectFromNode
  in
    svg
      [ width (w |> px)
      , height (h |> px)
      ]
      [ g
          [ transform "translate(-0.5, -0.5)"
          , strokeLinecap "square"
          ]
          (edges
            |> Set.toList
            |> List.map
                (\(n, m) ->
                  let
                    (nOut, mIn) = connectionOrdinalsFromEdge (n, m)
                    nRect = rectFromNode n
                    mRect = rectFromNode m
                  in
                    viewOrthoConnector
                      (drawing.colorEdge (n, m))
                      layout.edgeRadius
                      (nRect |> rectBottomRight |> addCoord (connectionShift nOut |> negate, 0)) -- outgoing connection: from bottom-right, stack left
                      (mRect |> rectBottomLeft |> addCoord (0, connectionShift mIn |> negate)) -- incoming connection: to bottom-left, stack up
                )
          )
      , g
          [ Svg.Attributes.style "cursor: default;"
          ]
          (ordered
            |> List.map
                (viewNode layout drawing rectFromNode)
          )
      , g
          []
          (topNodes
            |> List.map
                (\n ->
                  let
                    nRect = rectFromNode n
                    yOffset = centeringOffset layout.yMinSpacing nRect.height
                  in
                    Svg.rect
                      [ fill "rgba(0, 0, 0, 0.2)"
                      , x (nRect.x + nRect.width |> px)
                      , y (nRect.y - yOffset |> px)
                      , width (layout.labelMaxWidth |> px)
                      , height (1 |> px)
                      ]
                      []
                )
          )
      ]


viewNode : LayoutConfig -> DrawingConfig -> (Node -> Rect) -> Node -> Svg Node
viewNode layout drawing toRect n =
  let
    nRect = n |> toRect
    yOffset = centeringOffset layout.yMinSpacing nRect.height
  in
    g
      [ transform <| translate nRect.x nRect.y
      ]
      [ rect
          [ width (nRect.width |> px)
          , height (nRect.height |> px)
          , fill (drawing.colorNode n)
          ]
          []
      , g
          [ transform <| translate nRect.width (nRect.height // 2 + 2) ]
          [ n |> drawing.viewLabel
          ]
      , rect
          [ y (negate yOffset |> px)
          , width (nRect.width + layout.labelMaxWidth |> px)
          , height (max layout.yMinSpacing nRect.height |> px)
          , fill "transparent"
          , onClick n
          ]
          []
      ]


viewOrthoConnector : String -> Int -> Coord -> Coord -> Svg a
viewOrthoConnector color radius from to =
  path
    [ stroke color
    , fill "transparent"
    , d (pathOrthoConnector radius from to)
    ]
    []


-- TODO This only draws paths going DOWN, RIGHT. Ideally it could draw any of 8 permutations.
pathOrthoConnector : Int -> Coord -> Coord -> String
pathOrthoConnector radius (x, y) (u, v) =
  let
    width = abs (u - x)
    height = abs (v - y)
    r = min radius (min width height)
  in
    join
      [ moveTo (x, y)
      , lineTo (x, v - r)
      , circularArcTo r False False (x + r, v)
      , lineTo (u, v)
      ]


px : number -> String
px n =
  toString n ++ "px"


translate : number -> number -> String
translate x y =
  "translate(" ++ toString x ++ ", " ++ toString y ++ ")"


--

type alias Coord =
  (Int, Int)


addCoord : Coord -> Coord -> Coord
addCoord (x, y) (u, v) =
  ( x + u
  , y + v
  )


type alias Rect =
  { x : Int
  , y : Int
  , width : Int
  , height : Int
  }


emptyRect : Rect
emptyRect =
  Rect 0 0 0 0


rectTopRight : Rect -> Coord
rectTopRight { x, y, width, height } =
  ( x + width
  , y
  )


rectBottomLeft : Rect -> Coord
rectBottomLeft { x, y, width, height } =
  ( x
  , y + height
  )


rectBottomRight : Rect -> Coord
rectBottomRight { x, y, width, height } =
  ( x + width
  , y + height
  )


-- construct path descriptions

join : List String -> String
join =
  String.join " "


prefixCoord : String -> Coord -> String
prefixCoord prefix (x, y) =
  join [ prefix, toString x, toString y ]


boolToFlag : Bool -> String
boolToFlag bool =
  case bool of
    True  -> "1"
    False -> "0"


moveTo : Coord -> String
moveTo =
  prefixCoord "M"


lineTo : Coord -> String
lineTo =
  prefixCoord "L"


{-| Describe elliptical arc.
-}
arcTo : Int -> Int -> Int -> Bool -> Bool -> Coord -> String
arcTo rx ry rotation largeArc clockwise (x, y) =
  join
    [ "A"
    , toString rx
    , toString ry
    , toString rotation
    , boolToFlag largeArc
    , boolToFlag clockwise
    , toString x
    , toString y
    ]


circularArcTo : Int -> Bool -> Bool -> Coord -> String
circularArcTo r =
  arcTo r r 0
