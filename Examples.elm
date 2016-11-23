module Examples exposing (..)

import Dict exposing (Dict)
import Digraph exposing (..)
import Set exposing (Set)


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
  topologicalRank example

-- [(2,4),(3,1),(4,2),(5,1),(7,1),(8,2),(9,3),(10,3),(11,2)]


paths =
  example3
    |> toAdjacencyList
    |> pathsFrom 1

-- [[1,3,5],[1,2,6,8],[1,2,6,7],[1,2,4,1]]


cycles =
  findCycles example2

-- [[2,3,9,5,11,2],[2,3,9,2],[2,3,8,9,5,11,2],[2,3,8,9,2],[5,11,9,5]]
