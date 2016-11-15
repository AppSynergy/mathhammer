module Module.SavePool exposing (Model,init,update)

import Dict exposing (Dict)

import Lib.Dice as Dice
import Model exposing (Msg,Chance,Stat)


-- MODEL

type alias Model = Dice.Pool
  { ap : Int
  , sv : Int
  }


init : (Int, Int) -> List Chance -> Model
init (ap, sv) input =
  { ap = ap
  , sv = sv
  , input = input
  , results = []
  , chartId = "toSaveChart"
  , name = "Armor Saves"
  }


-- UPDATE

update : (Int, Int) -> List Chance -> Model -> Model
update (ap, sv) results model =
  let
    model' = { model | ap = ap , sv = sv , input = results }
  in
  { model' | results = updateChances model' }


updateChances : Model -> List Chance
updateChances model =
  if model.ap <= model.sv
    -- Save negated
    then model.input
    else case Dict.get model.sv Dice.toFailSaveChance of
      Just x -> Dice.expand x model.input
      Nothing -> model.input
