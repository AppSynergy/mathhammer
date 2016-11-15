module Module.SavePool exposing (Model,init,update)

import Dict exposing (Dict)

import Lib.Dice as Dice
import Model exposing (Msg,Chance,Stat)


-- MODEL

type alias Model = Dice.Pool
  { ap : Int
  , sv : Int
  }


type alias HasApSv a =
  { a
  | attacker_ap : Stat
  , defender_sv : Stat
  }


init : Int -> Int -> List Chance -> Model
init ap sv input =
  { ap = ap
  , sv = sv
  , input = input
  , results = []
  , chartId = "toSaveChart"
  , name = "Armor Saves"
  }


-- UPDATE

update : HasApSv a -> List Chance -> Model -> Model
update stats results model =
  let
    debug = Debug.log "update" model'
    model' =
      { model
      | ap = stats.attacker_ap.value
      , sv = stats.defender_sv.value
      , input = results
      }
  in
  { model' | results = updateChances model' }


updateChances : Model -> List Chance
updateChances model =
  if model.ap <= model.sv
    then model.input
    else case Dict.get model.sv Dice.toFailSaveChance of
      Just x -> Dice.expand x model.input
      Nothing -> model.input
