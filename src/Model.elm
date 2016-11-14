module Model exposing (..)

import Json.Encode as Json

-- MODEL

type Msg
  = NoOp
  | Boot
  | DrawChart String Json.Value
  | UpdateCharts
  | UpdateStat Stat String

type alias Chance = (Int, Float)
type alias AccumChance = (Int, Float, Float)


type Player
  = Attacker | Defender

type StatTag
  = N | WS | BS | S | T | W | I | A | Ld | AP | Sv

type Stat = Playerstat Player StatTag
