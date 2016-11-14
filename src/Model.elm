module Model exposing (..)

import Json.Encode as Json

-- MODEL

type Msg
  = NoOp
  | Boot
  | DrawChart String Json.Value
  | UpdateCharts
  | UpdateStat String String

type alias Chance = 
  (Int, Float)

type alias AccumChance =
  (Int, Float, Float)

type alias Stat =
  { value : Int
  , range : List Int
  }
