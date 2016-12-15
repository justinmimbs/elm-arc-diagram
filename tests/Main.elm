module Main exposing (..)

import AcyclicDigraph
import Dict exposing (Dict)
import Digraph exposing (Node, Edge)
import Html
import Set exposing (Set)


edges1 : Set Edge
edges1 =
  Set.fromList
    [ ( 3,  4)
    , ( 3,  8)
    , ( 3, 10)
    , ( 5, 11)
    , ( 7,  8)
    , ( 7, 11)
    , ( 8,  9)
    , ( 9,  2)
    --, ( 9,  5) -- make cycle
    , (11,  2)
    , (11,  9)
    , (11, 10)
    ]


edges2 : Set Edge
edges2 =
  Set.fromList
    [ (1, 2)
    , (1, 3)
    , (2, 4)
    , (2, 6)
    , (3, 5)
    , (4, 1)
    , (6, 7)
    , (6, 8)
    ]


edges3 : Set Edge
edges3 =
  Set.fromList
    [ ( 2,  3)
    , ( 3,  4)
    , ( 3,  8)
    , ( 3,  9)
    , ( 3, 10)
    , ( 5, 11)
    , ( 7,  8)
    , ( 7, 11)
    , ( 8,  9)
    , ( 9,  2)
    , ( 9,  5)
    , (11,  2)
    , (11,  9)
    , (11, 10)
    ]


tests : List (() -> Bool)
tests =
  [ \() ->
      edges1
        |> AcyclicDigraph.fromEdges
        |> Result.map AcyclicDigraph.topologicalRank
        |> (==) (Ok <| Dict.fromList [ (2, 4), (3, 1), (4, 2), (5, 1), (7, 1), (8, 2), (9, 3), (10, 3), (11, 2) ])
  , \() ->
      edges1
        |> Set.insert (9, 5) -- make cycle
        |> AcyclicDigraph.fromEdges
        |> Result.map AcyclicDigraph.topologicalRank
        |> (==) (Err [ [ 5, 11, 9, 5 ] ])
  , \() ->
      edges2
        |> Digraph.toAdjacencyList
        |> Digraph.pathsFrom 1
        |> (==) [ [ 1, 2, 4, 1 ], [ 1, 2, 6, 7 ], [ 1, 2, 6, 8 ], [ 1, 3, 5 ] ]
  , \() ->
      edges3
        |> Digraph.findCycles
        |> (==) [ [ 2, 3, 8, 9, 2 ], [ 2, 3, 8, 9, 5, 11, 2 ], [ 2, 3, 9, 2 ], [ 2, 3, 9, 5, 11, 2 ], [ 5, 11, 9, 5 ] ]
  ]


result =
  tests
    |> List.map ((|>) ())
    |> List.all ((==) True)
    |> (\pass -> if pass then "All tests passed" else "Failing tests")
    |> Debug.log "result"


main =
  Html.div [] [ Html.text result ]
