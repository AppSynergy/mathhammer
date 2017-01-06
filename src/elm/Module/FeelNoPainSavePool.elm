module Module.FeelNoPainSavePool exposing (init, update)

import Dict exposing (Dict)
import Lib.Dice as Dice
import Model exposing (Chance)
import Module.Pool exposing (Model)


-- MODEL

init : Model -> Model
init model =
  { model
  | chartId = "fnpSaveChart"
  , name = "Failed FNP Saves"
  , plural = "failed FNP saves"
  }

-- UPDATE

update : Model -> List Chance
update model =
  case Dict.get model.def Dice.toFailSaveChance of
    Just x -> Dice.expand x model.input
    Nothing -> model.input
