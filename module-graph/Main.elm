module Main exposing (..)

import AcyclicDigraph exposing (AcyclicDigraph)
import Diagram
import DiagramConnectivity
import Dict exposing (Dict)
import Digraph exposing (Node, Edge)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes

import Json.Decode exposing (Decoder)


type alias Model =
  { moduleGraph : ModuleGraph
  , selectedNode : Maybe Node
  }


input : String
input =
  """{"VirtualDom.Report": {"imports": [], "package": "elm-lang/virtual-dom"}, "RoseTree": {"imports": ["Lazy.List"], "package": "elm-community/elm-test"}, "VirtualDom.Metadata": {"imports": ["Array", "Dict", "Json.Decode", "Json.Encode", "VirtualDom.Report"], "package": "elm-lang/virtual-dom"}, "TestResult": {"imports": ["Expect", "Html", "Html.Attributes", "String", "Test.Runner"], "package": "justinmimbs/elm-date-extra"}, "VirtualDom.Debug": {"imports": ["Json.Decode", "Json.Encode", "Task", "VirtualDom.Expando", "VirtualDom.Helpers", "VirtualDom.History", "VirtualDom.Metadata", "VirtualDom.Overlay", "VirtualDom.Report"], "package": "elm-lang/virtual-dom"}, "Platform": {"imports": ["Basics", "Platform.Cmd", "Platform.Sub"], "package": "elm-lang/core"}, "Html": {"imports": ["VirtualDom"], "package": "elm-lang/html"}, "VirtualDom.Expando": {"imports": ["Dict", "Json.Decode", "VirtualDom.Helpers"], "package": "elm-lang/virtual-dom"}, "Test.Convert": {"imports": ["Date", "Date.Extra", "Expect", "Regex", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Date.Extra.Facts": {"imports": ["Date"], "package": "justinmimbs/elm-date-extra"}, "Test": {"imports": ["Test.Internal", "Expect", "Fuzz"], "package": "elm-community/elm-test"}, "Shrink": {"imports": ["Lazy.List", "Lazy", "List", "Array", "Char", "String"], "package": "elm-community/shrink"}, "VirtualDom.History": {"imports": ["Array", "Json.Decode", "Json.Encode", "VirtualDom.Helpers", "VirtualDom.Metadata"], "package": "elm-lang/virtual-dom"}, "Util": {"imports": ["Random.Pcg", "Array", "String"], "package": "elm-community/elm-test"}, "Tests": {"imports": ["Html", "Random.Pcg", "Test", "Test.Convert", "Test.Create", "Test.Examples", "Test.Extract", "Test.Math", "Test.Runner", "TestResult"], "package": "justinmimbs/elm-date-extra"}, "Test.Runner": {"imports": ["Test", "Test.Internal", "Expect", "Random.Pcg", "String"], "package": "elm-community/elm-test"}, "String": {"imports": ["Char", "Maybe", "Result"], "package": "elm-lang/core"}, "Tuple": {"imports": [], "package": "elm-lang/core"}, "Json.Encode": {"imports": ["Array"], "package": "elm-lang/core"}, "Platform.Sub": {"imports": [], "package": "elm-lang/core"}, "Regex": {"imports": ["Maybe"], "package": "elm-lang/core"}, "Date.Internal.Parse": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Core", "Regex"], "package": "justinmimbs/elm-date-extra"}, "Test.Expectation": {"imports": [], "package": "elm-community/elm-test"}, "VirtualDom": {"imports": ["Json.Decode", "VirtualDom.Debug"], "package": "elm-lang/virtual-dom"}, "Task": {"imports": ["Basics", "List", "Maybe", "Platform", "Platform.Cmd", "Result"], "package": "elm-lang/core"}, "Lazy": {"imports": [], "package": "elm-lang/lazy"}, "Date": {"imports": ["Task", "Time", "Result"], "package": "elm-lang/core"}, "Test.Internal": {"imports": ["Random.Pcg", "Test.Expectation", "Dict", "Shrink", "Fuzz", "Fuzz.Internal", "RoseTree", "Lazy.List"], "package": "elm-community/elm-test"}, "Expect": {"imports": ["Test.Expectation", "Dict", "Set", "String"], "package": "elm-community/elm-test"}, "Basics": {"imports": [], "package": "elm-lang/core"}, "Date.Extra": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Core", "Date.Internal.Extract", "Date.Internal.Format", "Date.Internal.Parse"], "package": "justinmimbs/elm-date-extra"}, "Maybe": {"imports": [], "package": "elm-lang/core"}, "Random.Pcg": {"imports": ["Bitwise", "Json.Encode", "Json.Decode", "Task", "Tuple", "Time"], "package": "mgold/elm-random-pcg"}, "List": {"imports": ["Basics", "Maybe", "Maybe"], "package": "elm-lang/core"}, "Lazy.List": {"imports": ["Array", "List", "Random", "Lazy"], "package": "elm-community/lazy-list"}, "Test.Extract": {"imports": ["Date", "Date.Extra", "Date.Extra.Facts", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Fuzz.Internal": {"imports": ["RoseTree", "Random.Pcg"], "package": "elm-community/elm-test"}, "Platform.Cmd": {"imports": [], "package": "elm-lang/core"}, "Date.Internal.Core": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.RataDie"], "package": "justinmimbs/elm-date-extra"}, "Time": {"imports": ["Basics", "Dict", "List", "Maybe", "Platform", "Platform.Sub", "Task"], "package": "elm-lang/core"}, "Date.Internal.Extract": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Core"], "package": "justinmimbs/elm-date-extra"}, "Date.Internal.Format": {"imports": ["Date", "Date.Extra.Facts", "Date.Internal.Extract", "Regex", "String"], "package": "justinmimbs/elm-date-extra"}, "VirtualDom.Overlay": {"imports": ["Json.Decode", "Json.Encode", "VirtualDom.Helpers", "VirtualDom.Metadata", "VirtualDom.Report"], "package": "elm-lang/virtual-dom"}, "Test.Examples": {"imports": ["Date", "Date.Extra", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Test.Create": {"imports": ["Date", "Date.Extra", "Regex", "String", "Test", "Test.Utilities", "Tuple"], "package": "justinmimbs/elm-date-extra"}, "Json.Decode": {"imports": ["Array", "Dict", "Json.Encode", "List", "Maybe", "Result"], "package": "elm-lang/core"}, "Test.Utilities": {"imports": ["Date", "Date.Extra", "Date.Extra.Facts", "Expect", "Test"], "package": "justinmimbs/elm-date-extra"}, "Set": {"imports": ["Basics", "Dict", "List"], "package": "elm-lang/core"}, "Test.Math": {"imports": ["Date", "Date.Extra", "Test", "Test.Utilities"], "package": "justinmimbs/elm-date-extra"}, "Random": {"imports": ["Basics", "List", "Platform", "Platform.Cmd", "Task", "Time", "Tuple"], "package": "elm-lang/core"}, "Bitwise": {"imports": [], "package": "elm-lang/core"}, "Char": {"imports": ["Basics"], "package": "elm-lang/core"}, "Date.Internal.RataDie": {"imports": ["Date", "Date.Extra.Facts"], "package": "justinmimbs/elm-date-extra"}, "Fuzz": {"imports": ["Array", "Char", "Util", "Lazy.List", "Shrink", "RoseTree", "Random.Pcg", "Fuzz.Internal"], "package": "elm-community/elm-test"}, "VirtualDom.Helpers": {"imports": ["Json.Decode", "Json.Encode"], "package": "elm-lang/virtual-dom"}, "Result": {"imports": ["Maybe"], "package": "elm-lang/core"}, "Html.Attributes": {"imports": ["Html", "Json.Encode", "VirtualDom"], "package": "elm-lang/html"}, "Dict": {"imports": ["Basics", "Maybe", "List", "String"], "package": "elm-lang/core"}, "Array": {"imports": ["Basics", "Maybe", "List"], "package": "elm-lang/core"}}"""


type alias ModuleData =
  { id : Node
  , name : String
  , imports : Set Node
  , package : String
  }


type alias ModuleGraph =
  Dict Node ModuleData


decodeInput : Decoder ModuleGraph
decodeInput =
  Json.Decode.dict
    (Json.Decode.map2
      (,)
      (Json.Decode.field "imports" <| Json.Decode.map Set.fromList <| Json.Decode.list Json.Decode.string)
      (Json.Decode.field "package" Json.Decode.string)
    )
  |> Json.Decode.map moduleGraphFromInput


moduleGraphFromInput : Dict String (Set String, String) -> ModuleGraph
moduleGraphFromInput dict =
  let
    nameToId = Dict.keys dict |> List.indexedMap (flip (,)) |> Dict.fromList
    idFromName = (flip Dict.get) nameToId >> Maybe.withDefault -1
  in
    Dict.foldl
      (\name (imports, package) dict ->
        let
          id = idFromName name
        in
          Dict.insert
            id
            (ModuleData id name (Set.map idFromName imports) package)
            dict
      )
      Dict.empty
      dict


toggleMaybe : a -> Maybe a -> Maybe a
toggleMaybe a ma =
  if ma == Just a then
    Nothing
  else
    Just a


isNothing : Maybe a -> Bool
isNothing m =
  case m of
    Just _  -> False
    Nothing -> True


unpack : (e -> x) -> (a -> x) -> Result e a -> x
unpack fromErr fromOk result =
  case result of
    Err e ->
      fromErr e
    Ok a ->
      fromOk a


toEdges : Dict comparable (Set comparable) -> Set (comparable, comparable)
toEdges =
  Dict.foldl
    (\x ys ->
      Set.union
        (Set.map ((,) x) ys)
    )
    Set.empty


-- view

defaultOptions = Diagram.defaultOptions
defaultOptionsConnectivity = DiagramConnectivity.defaultOptions


view : (ModuleGraph, Maybe Node) -> Html Node
view (modules, mSelectedNode) =
  let
    edges : Set Edge
    edges =
      modules
        |> Dict.map (\_ v -> v.imports)
        |> toEdges
        |> Set.map (\(x, y) -> (y, x))
  in
    Html.div
      [ Html.Attributes.style [ ("margin", "40px") ]
      ]
      [ AcyclicDigraph.fromEdges edges
          |> unpack
              (always <| Html.text "Graph contains cycles")
              (viewDiagram modules mSelectedNode)
      ]


viewDiagram : ModuleGraph -> Maybe Node -> AcyclicDigraph -> Html Node
viewDiagram modules mSelectedNode graph =
  let
    modulePackageFromNode : Node -> (String, String)
    modulePackageFromNode =
      (flip Dict.get) modules >> Maybe.map (\m -> (m.name, m.package)) >> Maybe.withDefault ("<module name>", "<package name>")
  in
    case mSelectedNode of
      Just node ->
        DiagramConnectivity.viewWithOptions
          { defaultOptionsConnectivity | viewLabel = \d n -> viewLabel (isNothing d) (modulePackageFromNode n) }
          node
          graph

      Nothing ->
        Diagram.viewWithOptions
          { defaultOptions | viewLabel = viewLabel False << modulePackageFromNode }
          graph


viewLabel : Bool -> (String, String) -> Svg a
viewLabel isDimmed (moduleName, packageName) =
  Svg.text_
    [ Svg.Attributes.x "4px"
    , Svg.Attributes.fontFamily "Helvetica, Arial"
    , Svg.Attributes.fontSize "12px"
    , Svg.Attributes.dominantBaseline "middle"
    ]
    [ Svg.tspan
        [ Svg.Attributes.fill (if isDimmed then "rgb(200, 200, 200)" else "black")
        ]
        [ Svg.text moduleName
        ]
    , Svg.tspan
        [ Svg.Attributes.fill "rgb(200, 200, 200)"
        ]
        [ Svg.text <| " (" ++ packageName ++ ")"
        ]
    ]


main : Program Never (ModuleGraph, Maybe Node) Node
main =
  Html.beginnerProgram
    { model = input |> Json.Decode.decodeString decodeInput |> Result.withDefault Dict.empty |> (flip (,)) Nothing
    , update =
        (\node (modules, mSelectedNode) ->
          let
            _ = Debug.log "node" node
          in
            (modules, mSelectedNode |> toggleMaybe node)
        )
    , view = view
    }
