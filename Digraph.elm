module Digraph exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


{-| From a set of edges, get the set of nodes with no incoming edges.
-}
noIncoming : Set (Int, Int) -> Set Int
noIncoming edges =
  Set.diff
    (edges |> Set.map Tuple.first)
    (edges |> Set.map Tuple.second)


{-| From a set of edges, get a dictionary of (node -> topological rank) if the
edges are acyclic.
-}
topologicalRank : Set (Int, Int) -> Result String (Dict Int Int)
topologicalRank edges =
  let
    (remainingEdges, rankedNodes) =
      topologicalRankHelp
        1
        (noIncoming edges)
        edges
        Dict.empty
  in
    if Set.isEmpty remainingEdges then
      Ok rankedNodes
    else
      Err "Edges form a cycle"


topologicalRankHelp : Int -> Set Int -> Set (Int, Int) -> Dict Int Int -> (Set (Int, Int), Dict Int Int)
topologicalRankHelp rank addNodes edges rankedNodes =
  if Set.isEmpty addNodes then
    (edges, rankedNodes)

  else
    let
      newRankedNodes =
        Set.foldl
          ((flip Dict.insert) rank)
          rankedNodes
          addNodes

      (removedEdges, remainingEdges) =
        Set.partition
          (Tuple.first >> (flip Set.member) addNodes)
          edges

      addNodesNext =
        Set.diff
          (removedEdges |> Set.map Tuple.second)
          (remainingEdges |> Set.map Tuple.second)

    in
      topologicalRankHelp
        (rank + 1)
        addNodesNext
        remainingEdges
        newRankedNodes


-- example

example : Set (Int, Int)
example = Set.fromList
  [ (5, 11)
  , (7, 11)
  , (7, 8)
  , (3, 8)
  , (3, 10)
  , (3, 4)
  , (11, 2)
  , (11, 9)
  , (11, 10)
  , (8, 9)
  --, (9, 5) -- make cycle
  ]


ranked =
  topologicalRank example
