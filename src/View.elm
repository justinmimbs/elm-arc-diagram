module Main exposing (..)

import Dict exposing (Dict)
import Digraph exposing (Node, Edge, AdjacencyList, toAdjacencyList, transpose, degree, topologicalRank, topologicalSortBy)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)
import Svg exposing (Svg, svg, g, path, rect)
import Svg.Attributes exposing (x, y, width, height, transform, strokeLinecap, d, stroke, fill)


imports : Set Edge
imports =
  Set.fromList
    [ (1, 2)
    , (1, 3)
    , (1, 8)
    , (1, 9)
    , (2, 3)
    , (2, 4)
    , (3, 5)
    , (3, 8)
    , (3, 9)
    , (4, 5)
    , (4, 6)
    , (4, 7)
    , (4, 8)
    , (5, 9)
    , (6, 8)
    , (6, 9)
    ]


type alias Config =
  { edgeSpacing : Int
  , nodePadding : Int
  , yMinSpacing : Int
  , edgeRadius : Int
  }


main : Html a
main =
  let
    edges = imports |> Set.map (\(a, b) -> (b, a)) -- reverse edges
    config =
      { edgeSpacing = 2
      , nodePadding = 4
      , yMinSpacing = 20
      , edgeRadius = 4
      }
  in
    Html.div
      [ Html.Attributes.style [ ("margin", "40px") ]
      ]
      [ edges
          |> topologicalRank
          |> unpack
              Html.text
              (view config edges)
      ]


unpack : (e -> x) -> (a -> x) -> Result e a -> x
unpack fromErr fromOk result =
  case result of
    Err e -> fromErr e
    Ok a  -> fromOk a


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
        rect =
          Rect
            cursorX
            cursorY
            (outdegree * edgeSpacing + nodePadding * 2)
            (indegree * edgeSpacing + nodePadding * 2)
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


view : Config -> Set Edge -> Dict Node Int -> Html a
view config edges nodeToRank =
  let
    outgoing = edges |> toAdjacencyList
    incoming = outgoing |> transpose
    -- order same-rank nodes by: incoming degree, outgoing degree descending
    ordered = topologicalSortBy (\n -> (degree incoming n, degree outgoing n |> negate)) nodeToRank

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
      [ width "900px"
      , height "600px"
      ]
      [ g
          []
          (ordered
            |> List.map
                (rectFromNode >> viewRect)
          )
      , g
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
      ]


viewRect : Rect -> Svg a
viewRect r =
  rect
    [ x (r.x |> px)
    , y (r.y |> px)
    , width (r.width |> px)
    , height (r.height |> px)
    ]
    []


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
