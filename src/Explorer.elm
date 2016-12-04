module Explorer exposing (..)

import Diagram
import Dict exposing (Dict)
import Digraph exposing (..)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)

import Json.Decode exposing (Decoder)


input : String
input =
  """{"VirtualDom.Report": {"imports": [], "package": "elm-lang/virtual-dom"}, "RoseTree": {"imports": ["Lazy.List"], "package": "elm-community/elm-test"}, "VirtualDom.Metadata": {"imports": ["Array", "Dict", "Json.Decode", "Json.Encode", "VirtualDom.Report"], "package": "elm-lang/virtual-dom"}, "TestResult": {"imports": ["Expect", "Html", "Html.Attributes", "String", "Test.Runner"], "package": "justinmimbs/elm-date-extra"}, "VirtualDom.Debug": {"imports": ["Json.Decode", "Json.Encode", "Task", "VirtualDom.Expando", "VirtualDom.Helpers", "VirtualDom.History", "VirtualDom.Metadata", "VirtualDom.Overlay", "VirtualDom.Report"], "package": "elm-lang/virtual-dom"}, "Platform": {"imports": ["Basics", "Platform.Cmd", "Platform.Sub"], "package": "elm-lang/core"}, "Html": {"imports": ["VirtualDom"], "package": "elm-lang/html"}, "VirtualDom.Expando": {"imports": ["Dict", "Json.Decode", "VirtualDom.Helpers"], "package": "elm-lang/virtual-dom"}, "Test.Convert": {"imports": ["Date", "Date.Extra", "Expect", "Regex", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Date.Extra.Facts": {"imports": ["Date"], "package": "justinmimbs/elm-date-extra"}, "Test": {"imports": ["Test.Internal", "Expect", "Fuzz"], "package": "elm-community/elm-test"}, "Shrink": {"imports": ["Lazy.List", "Lazy", "List", "Array", "Char", "String"], "package": "elm-community/shrink"}, "VirtualDom.History": {"imports": ["Array", "Json.Decode", "Json.Encode", "VirtualDom.Helpers", "VirtualDom.Metadata"], "package": "elm-lang/virtual-dom"}, "Util": {"imports": ["Random.Pcg", "Array", "String"], "package": "elm-community/elm-test"}, "Tests": {"imports": ["Html", "Random.Pcg", "Test", "Test.Convert", "Test.Create", "Test.Examples", "Test.Extract", "Test.Math", "Test.Runner", "TestResult"], "package": "justinmimbs/elm-date-extra"}, "Test.Runner": {"imports": ["Test", "Test.Internal", "Expect", "Random.Pcg", "String"], "package": "elm-community/elm-test"}, "String": {"imports": ["Char", "Maybe", "Result"], "package": "elm-lang/core"}, "Tuple": {"imports": [], "package": "elm-lang/core"}, "Json.Encode": {"imports": ["Array"], "package": "elm-lang/core"}, "Platform.Sub": {"imports": [], "package": "elm-lang/core"}, "Regex": {"imports": ["Maybe"], "package": "elm-lang/core"}, "Date.Internal.Parse": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Core", "Regex"], "package": "justinmimbs/elm-date-extra"}, "Test.Expectation": {"imports": [], "package": "elm-community/elm-test"}, "VirtualDom": {"imports": ["Json.Decode", "VirtualDom.Debug"], "package": "elm-lang/virtual-dom"}, "Task": {"imports": ["Basics", "List", "Maybe", "Platform", "Platform.Cmd", "Result"], "package": "elm-lang/core"}, "Lazy": {"imports": [], "package": "elm-lang/lazy"}, "Date": {"imports": ["Task", "Time", "Result"], "package": "elm-lang/core"}, "Test.Internal": {"imports": ["Random.Pcg", "Test.Expectation", "Dict", "Shrink", "Fuzz", "Fuzz.Internal", "RoseTree", "Lazy.List"], "package": "elm-community/elm-test"}, "Expect": {"imports": ["Test.Expectation", "Dict", "Set", "String"], "package": "elm-community/elm-test"}, "Basics": {"imports": [], "package": "elm-lang/core"}, "Date.Extra": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Core", "Date.Internal.Extract", "Date.Internal.Format", "Date.Internal.Parse"], "package": "justinmimbs/elm-date-extra"}, "Maybe": {"imports": [], "package": "elm-lang/core"}, "Random.Pcg": {"imports": ["Bitwise", "Json.Encode", "Json.Decode", "Task", "Tuple", "Time"], "package": "mgold/elm-random-pcg"}, "List": {"imports": ["Basics", "Maybe", "Maybe"], "package": "elm-lang/core"}, "Lazy.List": {"imports": ["Array", "List", "Random", "Lazy"], "package": "elm-community/lazy-list"}, "Test.Extract": {"imports": ["Date", "Date.Extra", "Date.Extra.Facts", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Fuzz.Internal": {"imports": ["RoseTree", "Random.Pcg"], "package": "elm-community/elm-test"}, "Platform.Cmd": {"imports": [], "package": "elm-lang/core"}, "Date.Internal.Core": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.RataDie"], "package": "justinmimbs/elm-date-extra"}, "Time": {"imports": ["Basics", "Dict", "List", "Maybe", "Platform", "Platform.Sub", "Task"], "package": "elm-lang/core"}, "Date.Internal.Extract": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Core"], "package": "justinmimbs/elm-date-extra"}, "Date.Internal.Format": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Extract", "Regex", "String"], "package": "justinmimbs/elm-date-extra"}, "VirtualDom.Overlay": {"imports": ["Json.Decode", "Json.Encode", "VirtualDom.Helpers", "VirtualDom.Metadata", "VirtualDom.Report"], "package": "elm-lang/virtual-dom"}, "Test.Examples": {"imports": ["Date", "Date.Extra", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Test.Create": {"imports": ["Date", "Date.Extra", "Regex", "String", "Test", "Test.Utilities", "Tuple"], "package": "justinmimbs/elm-date-extra"}, "Json.Decode": {"imports": ["Array", "Dict", "Json.Encode", "List", "Maybe", "Result"], "package": "elm-lang/core"}, "Test.Utilities": {"imports": ["Date", "Date.Extra", "Date.Extra.Facts", "Expect", "Test"], "package": "justinmimbs/elm-date-extra"}, "Set": {"imports": ["Basics", "Dict", "List"], "package": "elm-lang/core"}, "Test.Math": {"imports": ["Date", "Date.Extra", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Random": {"imports": ["Basics", "List", "Platform", "Platform.Cmd", "Task", "Time", "Tuple"], "package": "elm-lang/core"}, "Bitwise": {"imports": [], "package": "elm-lang/core"}, "Char": {"imports": ["Basics"], "package": "elm-lang/core"}, "Date.Internal.RataDie": {"imports": ["Date", "Date.Extra.Facts"], "package": "justinmimbs/elm-date-extra"}, "Fuzz": {"imports": ["Array", "Char", "Util", "Lazy.List", "Shrink", "RoseTree", "Random.Pcg", "Fuzz.Internal"], "package": "elm-community/elm-test"}, "VirtualDom.Helpers": {"imports": ["Json.Decode", "Json.Encode"], "package": "elm-lang/virtual-dom"}, "Result": {"imports": ["Maybe"], "package": "elm-lang/core"}, "Html.Attributes": {"imports": ["Html", "Json.Encode", "VirtualDom"], "package": "elm-lang/html"}, "Dict": {"imports": ["Basics", "Maybe", "List", "String"], "package": "elm-lang/core"}, "Array": {"imports": ["Basics", "Maybe", "List"], "package": "elm-lang/core"}}"""


type alias ModuleData =
  { imports : Set String
  , package : String
  }


decodeInput : Decoder (Dict String ModuleData)
decodeInput =
  Json.Decode.dict <|
    Json.Decode.map2
      ModuleData
      (Json.Decode.field "imports" <| Json.Decode.map Set.fromList <| Json.Decode.list Json.Decode.string)
      (Json.Decode.field "package" Json.Decode.string)


toNodes : Dict comparable (Set comparable) -> Set comparable
toNodes =
  Dict.foldl
    (\x ys nodes ->
      Set.foldl
        Set.insert
        nodes
        (Set.insert x ys)
    )
    Set.empty


toEdges : Dict comparable (Set comparable) -> Set (comparable, comparable)
toEdges =
  Dict.foldl
    (\x ys ->
      Set.union
        (Set.map ((,) x) ys)
    )
    Set.empty


{-| Given a Dict x y, create a Dict y x. Assume the Dict represents a
bijective mapping.
-}
invertDict : Dict comparable comparable1 -> Dict comparable1 comparable
invertDict =
  Dict.foldl
    (\k v -> Dict.insert v k)
    Dict.empty


main : Html a
main =
  let
    modules =
      Json.Decode.decodeString decodeInput input
        |> Result.withDefault Dict.empty

    adjList =
      modules |> Dict.map (\_ v -> v.imports)

    mapNameToId =
      adjList
        |> toNodes
        |> Set.toList
        |> List.indexedMap (flip (,))
        |> Dict.fromList

    idFromName = (flip Dict.get) mapNameToId >> Maybe.withDefault -1
    nameFromId = (flip Dict.get) (invertDict mapNameToId) >> Maybe.withDefault ""
    packageFromName = (flip Dict.get) modules >> Maybe.map .package >> Maybe.withDefault ""
    labelFromId =
      (\id ->
        let
          name = nameFromId id
        in
          name ++ " (" ++ packageFromName name ++ ")"
      )

    edges =
      adjList
        |> toEdges
        |> Set.map (\(x, y) -> (idFromName y, idFromName x))

  in
    Html.div
      [ Html.Attributes.style [ ("margin", "40px") ]
      ]
      [ edges
          |> topologicalRank
          |> unpack
              Html.text
              (Diagram.view labelFromId edges)
      ]


unpack : (e -> x) -> (a -> x) -> Result e a -> x
unpack fromErr fromOk result =
  case result of
    Err e -> fromErr e
    Ok a  -> fromOk a


{-
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
-}
