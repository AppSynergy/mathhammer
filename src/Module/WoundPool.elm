module Module.WoundPool exposing (Model,init,update)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr

import List.Extra

import Lib.Dice as Dice
import Model exposing (Msg,Chance,Stat)

-- MODEL

type alias Model =
  { s : Int
  , t : Int
  , input : List Chance
  , results : List Chance
  , chartId : String
  , name : String
  }


type alias HasStTo a =
  { a
  | attacker_s : Stat
  , defender_t : Stat
  }


init : Int -> Int -> List Chance  -> Model
init s t input =
  { s = s
  , t = t
  , input = input
  , results = []
  , chartId = "toWoundChart"
  , name = "Wounds"
  }


-- UPDATE

update : HasStTo a -> List Chance -> Model -> Model
update stats results model =
  let
    model' =
      { model
      | s = stats.attacker_s.value
      , t = stats.defender_t.value
      , input = results
      }
  in
  { model' | results = updateChances model' }


updateChances : Model -> List Chance
updateChances model =
  let
    statDiff = model.s - model.t
  in
  case Dict.get statDiff Dice.toWoundChance of
    Just x -> expand x model.input
    Nothing ->
      -- Wound on a 2+
      if statDiff > 2 then expand (5/6) model.input
      -- Impossible to wound
      else [(0, 1.0)]


expand : Float -> List Chance -> List Chance
expand toWound input =
  let
    calcToWound : Chance -> List Chance
    calcToWound (a,b) =
      Dice.binomial a toWound (1 - toWound)
        |> List.map (\(c,d) -> (c, b * d))

    sumByIndex : List Chance -> Chance
    sumByIndex xs =
      List.foldr (\(a,b) (c,d) -> (a, b + d)) (0, 0.0) xs
  in
  input
    |> List.concatMap calcToWound
    |> List.sort
    |> List.Extra.groupWhile (\(a,b) (c,d) -> a == c)
    |> List.map sumByIndex
