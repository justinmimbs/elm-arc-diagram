module Diagram exposing (view)

import Dict exposing (Dict)
import Digraph exposing (Node, Edge, AdjacencyList, toAdjacencyList, transpose, degree, topologicalRank, topologicalSortBy)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)
import Svg exposing (Svg, svg, g, path, rect, text_, text)
import Svg.Attributes exposing (x, y, width, height, transform, strokeLinecap, d, stroke, fill)


type alias Config =
  { edgeSpacing : Int
  , nodePadding : Int
  , yMinSpacing : Int
  , edgeRadius : Int
  }


defaultConfig : Config
defaultConfig =
  { edgeSpacing = 2
  , nodePadding = 4
  , yMinSpacing = 20
  , edgeRadius = 4
  }


functionFromDict : v -> Dict comparable v -> comparable -> v
functionFromDict default dict =
  (flip Dict.get) dict >> Maybe.withDefault default


layoutNodes : Config -> AdjacencyList -> AdjacencyList -> List Node -> Dict Node Rect
layoutNodes { edgeSpacing, nodePadding, yMinSpacing } incoming outgoing ordered =
  List.foldl
    (\n ((cursorX, cursorY), dict) ->
      let
        indegree = degree incoming n
        outdegree = degree outgoing n
        width = (outdegree * edgeSpacing + nodePadding * 2)
        height = (indegree * edgeSpacing + nodePadding * 2)
        -- center rect within yMinSpacing
        yOffset = max 0 ((yMinSpacing - height) // 2)
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

    orderedEdges =
      Set.map
        (\(a, b) ->
          ((ordinalFromNode a, ordinalFromNode b), (a, b))
        )
        edges
  in
    Set.foldl
      (\(_, (a, b)) (dict, (outgoing, incoming)) ->
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


view : (Node -> String) -> Set Edge -> Dict Node Int -> Html a
view =
  viewWithConfig defaultConfig


viewWithConfig : Config -> (Node -> String) -> Set Edge -> Dict Node Int -> Html a
viewWithConfig config labelFromNode edges nodeToRank =
  let
    outgoing = edges |> toAdjacencyList
    incoming = outgoing |> transpose
    -- order same-rank nodes by: incoming degree, outgoing degree descending
    ordered = topologicalSortBy (\n -> (degree incoming n, degree outgoing n |> negate)) nodeToRank
    topNodes = listTopNodes nodeToRank ordered

    -- layout dicts
    nodeToRect = layoutNodes config incoming outgoing ordered
    edgeToConnectionOrdinals = layoutEdges edges ordered

    -- layout functions
    connectionOrdinalsFromEdge =
      functionFromDict (0, 0) edgeToConnectionOrdinals

    rectFromNode =
      functionFromDict emptyRect nodeToRect

    connectionShift : Int -> Int
    connectionShift ordinal =
      ordinal * config.edgeSpacing + config.nodePadding

  in
    svg
      [ width "1200px"
      , height "1500px"
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
                      config.edgeRadius
                      (nRect |> rectBottomRight |> addCoord (connectionShift nOut |> negate, 0)) -- outgoing connection: from bottom-right, stack left
                      (mRect |> rectBottomLeft |> addCoord (0, connectionShift mIn |> negate)) -- incoming connection: to bottom-left, stack up
                )
          )
      , g
          []
          (ordered
            |> List.map
                (viewNode << labelFromNode <<* rectFromNode)
          )
      , g
          []
          (topNodes
            |> List.map
                (\n ->
                  let
                    nRect = rectFromNode n
                    yOffset = max 0 ((config.yMinSpacing - nRect.height) // 2)
                  in
                    Svg.rect
                      [ fill "rgba(0, 0, 0, 0.2)"
                      , x (nRect.x + nRect.width |> px)
                      , y (nRect.y - yOffset |> px)
                      , width (300 |> px)
                      , height (1 |> px)
                      ]
                      []
                )
          )
      ]


-- <*>
(<<*) : (x -> a -> b) -> (x -> a) -> x -> b
(<<*) f g x =
  f x (g x)

infixl 8 <<*


viewNode : String -> Rect -> Svg a
viewNode label r =
  g
    [ transform <| "translate(" ++ toString r.x ++ ", " ++ toString r.y ++ ")"
    ]
    [ rect
        [ width (r.width |> px)
        , height (r.height |> px)
        ]
        []
    , text_
        [ x (r.width + 4 |> px)
        , y (r.height // 2 + 2 |> px)
        , Svg.Attributes.fontFamily "Helvetica, Arial"
        , Svg.Attributes.fontSize "12px"
        , Svg.Attributes.dominantBaseline "middle"
        ]
        [ text label
        ]
    ]


viewOrthoConnector : Int -> Coord -> Coord -> Svg a
viewOrthoConnector radius from to =
  path
    [ stroke "gray"
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


px : number -> String
px n =
  toString n ++ "px"


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
