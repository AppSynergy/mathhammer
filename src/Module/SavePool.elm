module Module.SavePool exposing (Model,init,update)

import Dict exposing (Dict)
import List.Extra

import Lib.Dice as Dice
import Model exposing (Msg,Chance,Stat)


-- MODEL

type alias Model =
  { ap : Int
  , sv : Int
  , input : List Chance
  , results : List Chance
  , chartId : String
  , name : String
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
  let model' =
    { model
    | ap = stats.attacker_ap.value
    , sv = stats.defender_sv.value
    , input = results
    }
  in
  { model' | results = updateChances model' }


updateChances : Model -> List Chance
updateChances model =
  List.map (\(x,y) -> (x, y*0.5)) model.input
