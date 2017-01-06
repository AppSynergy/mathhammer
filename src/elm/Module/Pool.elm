module Module.Pool exposing (Model, init, update)

import Dict exposing (Dict)
import Lib.Dice as Dice
import Model exposing (Msg, Chance, Stat)


-- MODEL

type alias Model = Dice.Pool
  { att : Int
  , def : Int
  }


-- INIT

init : (Int, Int) -> List Chance -> Model
init (att, def) input =
  { att = 0
  , def = def
  , input = input
  , results = []
  , chartId = ""
  , name = ""
  , plural = ""
  }


-- UPDATE

update : (Model -> List Chance) -> (Int, Int) -> List Chance -> Model -> Model
update f (att, def) results model =
  let
    model' = { model | att = att , def = def , input = results }
  in
  { model' | results = f model' }
