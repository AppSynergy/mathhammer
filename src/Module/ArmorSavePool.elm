module Module.ArmorSavePool exposing (init, update)

import Dict exposing (Dict)
import Lib.Dice as Dice
import Model exposing (Chance)
import Module.Pool exposing (Model)


-- MODEL

init : Model -> Model
init model =
  { model
  | chartId = "armorSaveChart"
  , name = "Failed Armor Saves"
  , plural = "failed saves"
  }


-- UPDATE

update : Model -> List Chance
update model =
  if model.att <= model.def
    -- Save negated
    then model.input
    else case Dict.get model.def Dice.toFailSaveChance of
      Just x -> Dice.expand x model.input
      Nothing -> model.input
