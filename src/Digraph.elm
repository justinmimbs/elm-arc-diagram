module Digraph exposing
  ( Node, Edge, Path, AdjacencyList
  , toAdjacencyList
  , fromAdjacencyList
  , transpose
  , pathsFrom
  , distancesFrom
  , findCycles
  , degree
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


-- TODO organize this module better in regard to functions that work on edges vs adjList


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
    (\x ys adjListT ->
      Set.foldl
        (\y ->
          Dict.update
            y
            (Maybe.withDefault Set.empty >> Set.insert x >> Just)
        )
        adjListT
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
filterPathsFrom pred n adjList =
  filterPathsFromHelp pred adjList [] n
    |> List.map List.reverse


filterPathsFromHelp : (Node -> Bool) -> AdjacencyList -> Path -> Node -> List Path
filterPathsFromHelp pred adjList prePath n =
  let
    path =
      n :: prePath
  in
    if List.member n prePath then
      -- path has a cycle; stop following
      [ path ]
    else
      Dict.get n adjList
        |> Maybe.map
            -- follow each node in set
            (Set.filter pred
              >> Set.toList
              >> List.concatMap (filterPathsFromHelp pred adjList path))
        |> Maybe.withDefault
            -- path has reached terminal node
            [ path ]


{-| Find the k-degenerate subgraph where k = 2 (i.e. the 2-core). Recursively
remove edges connecting nodes of 1 degree (sinks and sources). An acyclic graph
has no 2-core.
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


degree : AdjacencyList -> Node -> Int
degree adjList =
  (flip Dict.get) adjList >> Maybe.map Set.size >> Maybe.withDefault 0


-- TODO flip args, like `degree`, for convenience of typical use over convention? otherwise, flip `degree` for consistency
successors : Node -> AdjacencyList -> Set Node
successors n =
  Dict.get n >> Maybe.withDefault Set.empty


distancesFrom : Node -> AdjacencyList -> Dict Node Int
distancesFrom n =
  distancesFromHelp
    Dict.empty
    0
    (Set.singleton n)


distancesFromHelp : Dict Node Int -> Int -> Set Node -> AdjacencyList -> Dict Node Int
distancesFromHelp prevResult distance nodes adjList =
  let
    result =
      Set.foldl
        ((flip Dict.insert) distance)
        prevResult
        nodes

    nextNodes =
      nodes
        |> Set.foldl
            (\n ->
              Set.union
                (successors n adjList)
            )
            Set.empty
        |> (flip Set.diff)
            (Dict.keys result |> Set.fromList)
  in
    if Set.isEmpty nextNodes then
      result
    else
      distancesFromHelp result (distance + 1) nextNodes adjList


-- List extra

last : List a -> Maybe a
last list =
  list
    |> List.drop (List.length list - 1)
    |> List.head
