module Module.WoundPool exposing (init, update)

import Dict exposing (Dict)
import Lib.Dice as Dice
import Model exposing (Chance)
import Module.Pool exposing (Model)


-- MODEL

init : Model -> Model
init model =
  { model
  | chartId = "toWoundChart"
  , name = "Wounds"
  , plural = "wounds"
  }


-- UPDATE

update : Model -> List Chance
update model =
  let
    statDiff = model.att - model.def
  in
  case Dict.get statDiff Dice.toWoundChance of
    Just x -> Dice.expand x model.input
    Nothing ->
      -- Wound on a 2+
      if statDiff > 2 then Dice.expand (5/6) model.input
      -- Impossible to wound
      else [(0, 1.0)]
