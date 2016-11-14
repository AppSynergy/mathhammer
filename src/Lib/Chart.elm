module Lib.Chart exposing (..)

import Json.Encode as Json

import Model exposing (..)


type alias HasChart a =
  { a | chartId : String, results : List Chance }


sendData : HasChart a -> Msg
sendData pool =
  (DrawChart pool.chartId (convert pool.results))


options : Json.Value
options =
  Json.object
    [ ("legend", Json.object
      [ ("display", Json.bool False)
      ] )
    , ("tooltips", Json.object
      [ ("enabled", Json.bool False)
      ] )
    ]


convert : List Chance -> Json.Value
convert xs =
  let
    labels = List.map (\(a, _) -> Json.int a) xs
    data = List.map (\(_, b) -> Json.float b) xs
    datasets =
      [ Json.object
        [ ("label", Json.string "basic")
        , ("data", Json.list data)
        , ("backgroundColor", Json.string "rgba(153,255,51,0.4)")
      ] ]
  in
  Json.object
    [ ("labels", Json.list labels)
    , ("datasets", Json.list datasets)
    ]
