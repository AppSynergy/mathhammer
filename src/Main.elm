port module Main exposing (main)

import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App
import Task

import Lib.Chart as Chart
import Model exposing (..)
import Module.HitPool as HitPool
import Module.WoundPool as WoundPool
import View.StatTable as StatTable


-- MAIN PROGRAM

main : Program Never
main = Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- SUBSCRIPTIONS

port drawChart : (String, Json.Value, Json.Value) -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch []


-- MODEL

type alias Model =
  { statTable : StatTable.Model
  , hitPool : HitPool.Model
  , woundPool : WoundPool.Model
  }


init : (Model, Cmd Msg)
init =
  let
    stats = StatTable.init
    hits = HitPool.init stats.attacker_n.value stats.attacker_bs.value
    wounds = WoundPool.init stats.attacker_s.value stats.defender_t.value hits.results
  in
  { statTable = stats
  , hitPool = hits
  , woundPool = wounds
  } |> update Boot


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Boot ->
      let
        stats = model.statTable
        hits =  HitPool.update stats model.hitPool
        wounds = WoundPool.update stats hits.results model.woundPool
      in
      { model
      | hitPool = hits
      , woundPool = wounds
      } |> update UpdateCharts

    UpdateStat stat value ->
      let
        stats = StatTable.update stat value model.statTable
        hits =  HitPool.update stats model.hitPool
        wounds = WoundPool.update stats hits.results model.woundPool
      in
      { model
      | statTable = stats
      , hitPool = hits
      , woundPool = wounds
      } |> update UpdateCharts

    UpdateCharts ->
      let
        hits = model.hitPool
        wounds = model.woundPool
        (_, graph1) = update (Chart.sendData hits) model
        (_, graph2) = update (Chart.sendData wounds) model
      in
      (model, Cmd.batch [graph1, graph2])

    DrawChart id data ->
      (model, drawChart (id, data, Chart.options))

    _ ->
      (model, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  Html.div [Attr.class "container"]
    [ StatTable.view model.statTable
    , HitPool.view model.hitPool
    , WoundPool.view model.woundPool
    ]
