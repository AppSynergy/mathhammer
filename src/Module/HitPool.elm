module Module.HitPool exposing (Model,init,update)

import Dict exposing (Dict)

import Lib.Dice as Dice
import Model exposing (Msg,Chance,Stat)


-- MODEL

type alias Model =
  { n: Int
  , bs : Int
  , results : List Chance
  , chartId : String
  , name : String
  }


type alias HasBSN a =
  { a
  | attacker_bs : Stat
  , attacker_n : Stat
  }


init : Int -> Int -> Model
init n bs =
  { n = n
  , bs = bs
  , results = []
  , chartId = "toHitChart"
  , name = "Hits"
  }


-- UPDATE

update : HasBSN a -> Model -> Model
update stats model =
  let model' =
    { model
    | n = stats.attacker_n.value
    , bs = stats.attacker_bs.value
    }
  in
  { model' | results = updateChances model' }


updateChances : Model -> List Chance
updateChances model =
  case Dict.get model.bs Dice.toHitChance of
    Just x -> Dice.binomial model.n x (1 - x)
    Nothing -> model.results
