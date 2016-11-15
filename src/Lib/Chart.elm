module Lib.Chart exposing (..)

import Json.Encode as Json

import Lib.Math as Math
import Model exposing (..)


type alias HasChart a =
  { a | chartId : String, results : List Chance }


sendData : HasChart a -> Msg
sendData pool =
  Math.accumulate pool.results
    |> convert
    |> DrawChart pool.chartId


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


convert : List (Int, Float, Float) -> Json.Value
convert xs =
  let
    debug = Debug.log "convert 4 chart:" datasets
    labels = List.map (\(a, _, _) -> Json.int a) xs
    data = List.map (\(_, b, _) -> Json.float b) xs
    dataAccum = List.map (\(_, _, c) -> Json.float c) xs
    datasets =
      [ Json.object
        [ ("label", Json.string "Probability")
        , ("data", Json.list data)
        , ("backgroundColor", Json.string "rgba(153,255,51,0.4)")
        ]
      , Json.object
        [ ("label", Json.string "Accumulated Probability")
        , ("data", Json.list dataAccum)
        , ("backgroundColor", Json.string "rgba(73,133,51,0.4)")
        ]
      ]
  in
  Json.object
    [ ("labels", Json.list labels)
    , ("datasets", Json.list datasets)
    ]
