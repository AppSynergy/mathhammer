module Module.WoundPool exposing (Model,init,update)

import Dict exposing (Dict)

import Lib.Dice as Dice
import Model exposing (Msg,Chance,Stat)


-- MODEL

type alias Model = Dice.Pool
  { s : Int
  , t : Int
  }


init : (Int, Int) -> List Chance -> Model
init (s, t) input =
  { s = s
  , t = t
  , input = input
  , results = []
  , chartId = "toWoundChart"
  , name = "Wounds"
  }


-- UPDATE

update : (Int, Int) -> List Chance -> Model -> Model
update (s, t) results model =
  let
    model' = { model | s = s , t = t , input = results }
  in
  { model' | results = updateChances model' }


updateChances : Model -> List Chance
updateChances model =
  let
    statDiff = model.s - model.t
  in
  case Dict.get statDiff Dice.toWoundChance of
    Just x -> Dice.expand x model.input
    Nothing ->
      -- Wound on a 2+
      if statDiff > 2 then Dice.expand (5/6) model.input
      -- Impossible to wound
      else [(0, 1.0)]
