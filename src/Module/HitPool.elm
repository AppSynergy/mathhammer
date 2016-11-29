module Module.HitPool exposing (init,update)

import Dict exposing (Dict)
import Lib.Dice as Dice
import Model exposing (Chance)
import Module.Pool exposing (Model)


-- MODEL

init : Model -> Model
init model =
  { model
  | chartId = "toHitChart"
  , name = "Hits"
  , plural = "hits"
  }


-- UPDATE

update : Model -> List Chance
update model =
  case Dict.get model.att Dice.toHitChance of
    Just x -> Dice.binomial model.def x (1 - x) -- jamming n as .def
    Nothing -> model.results
