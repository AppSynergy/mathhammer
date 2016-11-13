module Mathhammer.Model exposing (..)

-- MODEL

type Msg
  = NoOp
  | Boot
  | UpdateStat Stat String

type Player
  = Attacker | Defender

type StatTag
  = N | WS | BS | S | T | W | I | A | Ld | AP | Sv

type Stat = Playerstat Player StatTag
