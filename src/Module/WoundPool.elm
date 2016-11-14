module Module.WoundPool exposing (Model,init,update,view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import String

import Model exposing (Msg,Chance)
import View.ResultsTable as ResultsTable


-- MODEL

type alias Model =
  { s : Int
  , t : Int
  , input : List Chance
  , results : List Chance
  }


type alias HasStTo a =
  { a | attacker_s : Int, defender_t : Int }


init : Int -> Int -> List Chance  -> Model
init s t input =
  { s = s
  , t = t
  , input = input
  , results = input
  }


-- UPDATE

update : HasStTo a -> List Chance -> Model -> Model
update stats results model =
  let
    deb = Debug.log "wp update" model'
    model' =
      { model | s = stats.attacker_s, t = stats.defender_t, results = results }
  in
  { model' | results = updateChances model' }


updateChances : Model -> List Chance
updateChances model =
  model.results


-- VIEW

view : Model -> Html Msg
view model =
  ResultsTable.view model
