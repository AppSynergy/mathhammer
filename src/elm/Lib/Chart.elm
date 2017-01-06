module Lib.Chart exposing (..)

import Json.Encode as Json

import Lib.Math as Math
import Model exposing (..)


type alias HasChart a =
  { a | chartId : String, name : String, results : List Chance }


sendData : HasChart a -> Msg
sendData pool =
  Math.accumulate pool.results
    |> convert
    |> DrawChart pool.chartId pool.name


options : Json.Value
options =
  let obj = Json.object in
  Json.object
    [ ("legend", obj
      [ ("display", Json.bool False)
      ] )
    , ("tooltips", obj
      [ ("enabled", Json.bool True)
      , ("callbacks", obj [

        ] )
      ] )
    ]


convert : List (Int, Float, Float) -> Json.Value
convert xs =
  let
    labels = List.map (\(a, _, _) -> Json.int a) xs
    data = List.map (\(_, b, _) -> Json.float b) xs
    dataAccum = List.map (\(_, _, c) -> Json.float c) xs
    datasets =
      [ Json.object
        [ ("label", Json.string "Probability")
        , ("data", Json.list data)
        , ("backgroundColor", Json.string "#009040")
        ]
      , Json.object
        [ ("label", Json.string "Accumulated Probability")
        , ("data", Json.list dataAccum)
        , ("backgroundColor", Json.string "#CA6C00")
        ]
      ]
  in
  Json.object
    [ ("labels", Json.list labels)
    , ("datasets", Json.list datasets)
    ]
