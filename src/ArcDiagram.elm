module ArcDiagram exposing
  ( view
  , Layout, defaultLayout
  , Paint, basicPaint, defaultPaint
  )

import AcyclicDigraph exposing (Node, Edge, AcyclicDigraph)
import Dict exposing (Dict)
import Digraph exposing (AdjacencyList)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


type alias Layout =
  { edgeSpacing : Int
  , nodePadding : Int
  , yMinSpacing : Int
  , edgeRadius : Int
  , labelMaxWidth : Int
  }


defaultLayout : Layout
defaultLayout =
  { edgeSpacing = 2
  , nodePadding = 4
  , yMinSpacing = 20
  , edgeRadius = 4
  , labelMaxWidth = 100
  }


type alias Paint =
  { viewLabel : Node -> Svg Node
  , colorNode : Node -> String
  , colorEdge : Edge -> String
  }


defaultPaint : Paint
defaultPaint =
  { viewLabel = toString >> viewLabel
  , colorNode = always "black"
  , colorEdge = always "rgba(0, 0, 0, 0.4)"
  }


basicPaint : (Node -> String) -> Paint
basicPaint toLabel =
  { defaultPaint | viewLabel = toLabel >> viewLabel }


viewLabel : String -> Svg a
viewLabel string =
  Svg.text_
    [ Svg.Attributes.x "4px"
    , Svg.Attributes.fontFamily "Helvetica, Arial, san-serif"
    , Svg.Attributes.fontSize "12px"
    , Svg.Attributes.dominantBaseline "middle"
    ]
    [ Svg.text string
    ]


-- lookup: flip (Dict.getWithDefault v))
lookup : v -> Dict comparable v -> comparable -> v
lookup default dict =
  (flip Dict.get) dict >> Maybe.withDefault default


centeringOffset : Int -> Int -> Int
centeringOffset outer inner =
  max 0 ((outer - inner) // 2)


layoutNodes : Layout -> AdjacencyList -> AdjacencyList -> List Node -> Dict Node Rect
layoutNodes { edgeSpacing, nodePadding, yMinSpacing } incoming outgoing orderedNodes =
  List.foldl
    (\n ((cursorX, cursorY), dict) ->
      let
        indegree = incoming |> Digraph.degree n
        outdegree = outgoing |> Digraph.degree n
        width = (outdegree * edgeSpacing + nodePadding * 2)
        height = (indegree * edgeSpacing + nodePadding * 2)
        yOffset = centeringOffset yMinSpacing height -- center rect within yMinSpacing
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
    orderedNodes
  |> Tuple.second


layoutEdges : Layout -> (Node -> Rect) -> List Edge -> Dict Edge (Coord, Coord)
layoutEdges layout toRect orderedEdges =
  let
    -- connection ordinal -> pixels
    connectionShift : Int -> Int
    connectionShift ordinal =
      ordinal * layout.edgeSpacing + layout.nodePadding

    edgeToConnectionOrdinals : Dict Edge (Int, Int)
    edgeToConnectionOrdinals =
      orderedEdges
        |> List.foldl
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
        |> Tuple.first

  in
    edgeToConnectionOrdinals
      |> Dict.map
          (\(a, b) (aOut, bIn) ->
            ( a |> toRect |> rectBottomRight |> addCoord (connectionShift aOut |> negate, 0) -- outgoing connection: from bottom-right, stack left
            , b |> toRect |> rectBottomLeft |> addCoord (0, connectionShift bIn |> negate) -- incoming connection: to bottom-left, stack up
            )
          )


{-| Sort a set of edges based on the provided ordering of nodes.
-}
sortEdges : List Node -> Set Edge -> List Edge
sortEdges orderedNodes edges =
  let
    ordinalFromNode : Node -> Int
    ordinalFromNode =
      orderedNodes
        |> List.indexedMap
            (flip (,))
        |> Dict.fromList
        |> lookup -1
  in
    edges
      |> Set.toList
      |> List.sortBy (\(a, b) -> (ordinalFromNode a, ordinalFromNode b))


{-| List the first node of each topological layer.
-}
listTopNodes : Dict Node Int -> List Node -> List Node
listTopNodes rankedNodes orderedNodes =
  List.foldl
    (\n (ns, rank) ->
      let
        nRank = Dict.get n rankedNodes |> Maybe.withDefault 0
      in
        if nRank == rank then
          (ns, rank)
        else
          (n :: ns, nRank)
    )
    ([], -1)
    orderedNodes
  |> Tuple.first
  |> List.reverse


calculateTotalSize : Layout -> List Node -> (Node -> Rect) -> Coord
calculateTotalSize { yMinSpacing, labelMaxWidth } orderedNodes rectFromNode =
  let
    lastRect = orderedNodes |> List.reverse |> List.head |> Maybe.map rectFromNode |> Maybe.withDefault emptyRect
  in
    addCoord
      (rectTopRight lastRect)
      (labelMaxWidth, max yMinSpacing lastRect.height)


view : Layout -> Paint -> AcyclicDigraph -> Html Node
view layout paint graph =
  let
    edges = AcyclicDigraph.toEdges graph
    outgoing = edges |> Digraph.toAdjacencyList
    incoming = outgoing |> Digraph.transpose

    rankedNodes = AcyclicDigraph.topologicalRank graph
    orderedNodes =
      AcyclicDigraph.topologicalSortBy
        -- order same-rank nodes by: incoming degree (ascending), outgoing degree (descending)
        (\n -> (incoming |> Digraph.degree n, outgoing |> Digraph.degree n |> negate))
        rankedNodes

    orderedEdges = sortEdges orderedNodes edges

    nodeToRect = layoutNodes layout incoming outgoing orderedNodes
    rectFromNode = lookup emptyRect nodeToRect
    edgeToEndpoints = layoutEdges layout rectFromNode orderedEdges

    (w, h) = calculateTotalSize layout orderedNodes rectFromNode
  in
    Svg.svg
      [ Svg.Attributes.width (w |> px)
      , Svg.Attributes.height (h |> px)
      , Svg.Attributes.style "cursor: default;"
      ]
      [ Svg.g
          [ Svg.Attributes.transform "translate(-0.5, 0.5)"
          , Svg.Attributes.strokeLinecap "square"
          ]
          (orderedEdges
            |> List.map
                (\edge ->
                  let
                    (from, to) = Dict.get edge edgeToEndpoints |> Maybe.withDefault (origin, origin)
                  in
                    viewOrthoConnector (paint.colorEdge edge) layout.edgeRadius from to
                )
          )
      , Svg.g
          []
          (orderedNodes
            |> List.map
                (viewNode layout paint rectFromNode)
          )
      , Svg.g
          []
          (listTopNodes rankedNodes orderedNodes
            |> List.map
                (\n ->
                  let
                    nRect = rectFromNode n
                    yOffset = centeringOffset layout.yMinSpacing nRect.height
                  in
                    Svg.rect
                      [ Svg.Attributes.fill "rgba(0, 0, 0, 0.2)"
                      , Svg.Attributes.x (nRect.x + nRect.width |> px)
                      , Svg.Attributes.y (nRect.y - yOffset |> px)
                      , Svg.Attributes.width (layout.labelMaxWidth |> px)
                      , Svg.Attributes.height ("1px")
                      ]
                      []
                )
          )
      ]


viewNode : Layout -> Paint -> (Node -> Rect) -> Node -> Svg Node
viewNode layout paint toRect n =
  let
    nRect = n |> toRect
    yOffset = centeringOffset layout.yMinSpacing nRect.height
  in
    Svg.g
      [ Svg.Attributes.transform <| translate nRect.x nRect.y
      ]
      [ Svg.rect
          [ Svg.Attributes.width (nRect.width |> px)
          , Svg.Attributes.height (nRect.height |> px)
          , Svg.Attributes.fill (paint.colorNode n)
          ]
          []
      , Svg.g
          [ Svg.Attributes.transform <| translate nRect.width (nRect.height // 2 + 2) ]
          [ n |> paint.viewLabel
          ]
      , Svg.rect
          [ Svg.Attributes.y (negate yOffset |> px)
          , Svg.Attributes.width (nRect.width + layout.labelMaxWidth |> px)
          , Svg.Attributes.height (max layout.yMinSpacing nRect.height |> px)
          , Svg.Attributes.fill "transparent"
          , Svg.Events.onClick n
          ]
          []
      ]


viewOrthoConnector : String -> Int -> Coord -> Coord -> Svg a
viewOrthoConnector color radius from to =
  Svg.path
    [ Svg.Attributes.stroke color
    , Svg.Attributes.fill "transparent"
    , Svg.Attributes.d (pathOrthoConnector radius from to)
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


-- Coord, Rect

type alias Coord =
  (Int, Int)


origin : Coord
origin =
  (0, 0)


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


-- Construct Svg.path descriptions.

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
