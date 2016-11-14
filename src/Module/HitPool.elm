module Module.HitPool exposing (Model,init,update,view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr

import Lib.Dice as Dice
import Model exposing (Msg,Chance)
import View.ResultsTable as ResultsTable


-- MODEL

type alias Model =
  { n: Int
  , bs : Int
  , results : List Chance
  }


type alias HasBSN a =
  { a | attacker_bs : Int, attacker_n : Int }


init : Int -> Int -> Model
init n bs =
  { n = n
  , bs = bs
  , results = []
  }


-- UPDATE

update : HasBSN a -> Model -> Model
update stats model =
  let model' =
    { model | n = stats.attacker_n , bs = stats.attacker_bs }
  in
  { model' | results = updateChances model' }


updateChances : Model -> List Chance
updateChances model =
  case Dict.get model.bs Dice.toHitChance of
    Just x -> Dice.binomial model.n x (1 - x)
    Nothing -> model.results


-- VIEW

view : Model -> Html Msg
view model =
  ResultsTable.view model
