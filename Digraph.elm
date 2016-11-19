module Digraph exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Edge =
  (Int, Int)


type alias Path =
  List Int


{-| From a set of edges, get the set of nodes with no incoming edges.
-}
noIncoming : Set Edge -> Set Int
noIncoming edges =
  Set.diff
    (edges |> Set.map Tuple.first)
    (edges |> Set.map Tuple.second)


{-| From a set of edges, get a dictionary of (node -> topological rank) if the
edges are acyclic.
-}
topologicalRank : Set Edge -> Result String (Dict Int Int)
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
      Err "Graph must be acyclic to be topologically ranked"


topologicalRankHelp : Int -> Set Int -> Set Edge -> Dict Int Int -> (Set Edge, Dict Int Int)
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


{-| Convert a set of edges to a mapping of (source node -> list of target nodes).
-}
toDict : Set Edge -> Dict Int (List Int)
toDict =
  Set.foldl
    (\(a, b) ->
      Dict.update
        a
        (Maybe.withDefault [] >> (::) b >> Just)
    )
    Dict.empty


{-| List all paths in the graph from a given node. Cyclic paths are included.
-}
pathsFrom : Int -> Dict Int (List Int) -> List Path
pathsFrom =
  filterPathsFrom (always True)


{-| Like `pathsFrom`, but accepts a predicate function used to determine which
nodes to follow.
-}
filterPathsFrom : (Int -> Bool) -> Int -> Dict Int (List Int) -> List Path
filterPathsFrom pred n dict =
  filterPathsFromHelp pred dict [] n
    |> List.map List.reverse


filterPathsFromHelp : (Int -> Bool) -> Dict Int (List Int) -> Path -> Int -> List Path
filterPathsFromHelp pred dict prePath n =
  let
    path =
      n :: prePath
  in
    if List.member n prePath then
      -- path has a cycle; stop following
      [ path ]
    else
      Dict.get n dict
        |> Maybe.map
            -- follow nodes
            (List.filter pred
              >> List.concatMap (filterPathsFromHelp pred dict path))
        |> Maybe.withDefault
            -- path has reached terminal node
            [ path ]


{-| Find the k-degenerate subgraph where k = 2 (i.e. the 2-core). Recursively
remove edges containing nodes of 1 degree (i.e. all incoming and outgoing
edges). An acyclic graph has no 2-core.
-}
degenerate : Set Edge -> Set Edge
degenerate edges =
  let
    sources = Set.map Tuple.first edges
    targets = Set.map Tuple.second edges
    edges2 =
      Set.filter
        (\(s, t) ->
          Set.member s targets && Set.member t sources
        )
        edges
  in
    if Set.isEmpty edges2 || Set.size edges2 == Set.size edges then
      edges2
    else
      degenerate edges2


last : List a -> Maybe a
last list =
  list
    |> List.drop (List.length list - 1)
    |> List.head


findCycles : Set Edge -> List Path
findCycles edges =
  let
    edges2 = degenerate edges
  in
    if Set.isEmpty edges2 then
      []
    else
      let
        sources = Set.map Tuple.first edges2
        dict = toDict edges2
      in
        sources
          |> Set.toList
          |> List.concatMap
              (\n ->
                {- Only follow nodes >= n; this is an optimization ensuring we
                only look for cycles in canonical order (i.e. the path starts
                and ends with the least node).
                -}
                filterPathsFrom ((<=) n) n dict
                  |> List.filter isCanonicalCycle
              )


isCanonicalCycle : Path -> Bool
isCanonicalCycle path =
  Maybe.map2
    (==)
    (List.head path)
    (last path)
  |> Maybe.withDefault False


-- examples

example : Set Edge
example =
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


example2 : Set Edge
example2 =
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

example3 : Set Edge
example3 =
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


ranked =
  topologicalRank example2


paths =
  example3
    |> toDict
    |> pathsFrom 1


cycles =
  findCycles example
