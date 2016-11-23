module Digraph exposing
  ( Node, Edge, Path, AdjacencyList
  , topologicalRank
  , toAdjacencyList
  , fromAdjacencyList
  , transpose
  , pathsFrom
  , findCycles
  )

import Dict exposing (Dict)
import Set exposing (Set)


type alias Node =
  Int


type alias Edge =
  (Node, Node)


type alias Path =
  List Node


type alias AdjacencyList =
  Dict Node (Set Node)


{-| From a set of edges, get the set of nodes with no incoming edges.
-}
sourceNodes : Set Edge -> Set Node
sourceNodes edges =
  Set.diff
    (edges |> Set.map Tuple.first)
    (edges |> Set.map Tuple.second)


{-| From a set of edges, get a dictionary of (node -> topological rank) if the
edges are acyclic.
-}
topologicalRank : Set Edge -> Result String (Dict Node Int)
topologicalRank edges =
  let
    (remainingEdges, rankedNodes) =
      topologicalRankHelp
        1
        (sourceNodes edges)
        edges
        Dict.empty
  in
    if Set.isEmpty remainingEdges then
      Ok rankedNodes
    else
      Err "Graph must be acyclic to be topologically ranked"


topologicalRankHelp : Int -> Set Node -> Set Edge -> Dict Node Int -> (Set Edge, Dict Node Int)
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


{-| Convert a set of edges to a mapping of (x node -> set of y nodes).
-}
toAdjacencyList : Set Edge -> AdjacencyList
toAdjacencyList =
  Set.foldl
    (\(x, y) ->
      Dict.update
        x
        (Maybe.withDefault Set.empty >> Set.insert y >> Just)
    )
    Dict.empty


fromAdjacencyList : AdjacencyList -> Set Edge
fromAdjacencyList =
  Dict.foldl
    (\x ys ->
      Set.union
        (Set.map ((,) x) ys)
    )
    Set.empty


transpose : AdjacencyList -> AdjacencyList
transpose =
  Dict.foldl
    (\x ys xsByY ->
      Set.foldl
        (\y ->
          Dict.update
            y
            (Maybe.withDefault Set.empty >> Set.insert x >> Just)
        )
        xsByY
        ys
    )
    Dict.empty


{-| List all paths in the graph from a given node. Cyclic paths are included.
-}
pathsFrom : Node -> AdjacencyList -> List Path
pathsFrom =
  filterPathsFrom (always True)


{-| Like `pathsFrom`, but accepts a predicate function used to determine which
nodes to follow.
-}
filterPathsFrom : (Node -> Bool) -> Node -> AdjacencyList -> List Path
filterPathsFrom pred n ysByX =
  filterPathsFromHelp pred ysByX [] n
    |> List.map List.reverse


filterPathsFromHelp : (Node -> Bool) -> AdjacencyList -> Path -> Node -> List Path
filterPathsFromHelp pred ysByX prePath n =
  let
    path =
      n :: prePath
  in
    if List.member n prePath then
      -- path has a cycle; stop following
      [ path ]
    else
      Dict.get n ysByX
        |> Maybe.map
            -- follow each node in set
            (Set.filter pred
              >> Set.toList
              >> List.concatMap (filterPathsFromHelp pred ysByX path))
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
    xs = edges |> Set.map Tuple.first
    ys = edges |> Set.map Tuple.second
    edges2 =
      Set.filter
        (\(x, y) ->
          Set.member x ys && Set.member y xs
        )
        edges
  in
    if Set.isEmpty edges2 || Set.size edges2 == Set.size edges then
      edges2
    else
      degenerate edges2


{-| List all unique simple cycles.
-}
findCycles : Set Edge -> List Path
findCycles edges =
  let
    edges2 = degenerate edges
  in
    if Set.isEmpty edges2 then
      []
    else
      let
        xs = Set.map Tuple.first edges2
        ysByX = toAdjacencyList edges2
      in
        xs
          |> Set.toList
          |> List.concatMap
              (\x ->
                {- Only follow nodes >= x; this is an optimization ensuring we
                only look for cycles in a canonical order (i.e. the path starts
                and ends with the least node).
                -}
                filterPathsFrom ((<=) x) x ysByX
                  |> List.filter isSimpleCycle
              )


{-| Does path start and end at the same node? (This does not check for any
repeated nodes in between.)
-}
isSimpleCycle : Path -> Bool
isSimpleCycle path =
  Maybe.map2
    (==)
    (List.head path)
    (last path)
  |> Maybe.withDefault False


-- List extra

last : List a -> Maybe a
last list =
  list
    |> List.drop (List.length list - 1)
    |> List.head
