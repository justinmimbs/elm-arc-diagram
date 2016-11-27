module Explorer exposing (..)

import Dict exposing (Dict)
import Digraph exposing (..)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)
import View exposing (view)


objects : Dict Node String
objects =
  Dict.fromList
    [ (1, "Alfa")
    , (2, "Bravo")
    , (3, "Charlie")
    , (4, "Delta")
    , (5, "Echo")
    , (6, "Foxtrot")
    , (7, "Golf")
    , (8, "Hotel")
    , (9, "India")
    ]


dependencies : Set Edge
dependencies =
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


main : Html a
main =
  let
    edges = dependencies |> Set.map (\(a, b) -> (b, a)) -- reverse edges
    
  in
    Html.div
      [ Html.Attributes.style [ ("margin", "40px") ]
      ]
      [ edges
          |> topologicalRank
          |> unpack
              Html.text
              (view ((flip Dict.get) objects >> Maybe.withDefault "") edges)
      ]


unpack : (e -> x) -> (a -> x) -> Result e a -> x
unpack fromErr fromOk result =
  case result of
    Err e -> fromErr e
    Ok a  -> fromOk a

