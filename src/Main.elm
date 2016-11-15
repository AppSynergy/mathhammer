port module Main exposing (main)

import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App

import Lib.Chart as Chart
import Model exposing (..)
import Module.HitPool as HitPool
import Module.WoundPool as WoundPool
import Module.SavePool as SavePool
import View.StatTable as StatTable
import View.ResultsTable as ResultsTable


-- MAIN PROGRAM

main : Program Never
main = Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\model -> Sub.none)
    }


-- INTEROP

port drawChart : (String, Json.Value, Json.Value) -> Cmd msg


-- MODEL

type alias Model =
  { statTable : StatTable.Model
  , hitPool : HitPool.Model
  , woundPool : WoundPool.Model
  , savePool : SavePool.Model
  }


init : (Model, Cmd Msg)
init =
  initPools |> update Boot


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Boot ->
      model |> updatePools >> update UpdateCharts

    UpdateStat stat value ->
      let
        table = StatTable.update stat value model.statTable
      in
      { model | statTable = table } |> update Boot

    UpdateCharts ->
      let
        sendData : Chart.HasChart b -> (Model, Cmd Msg)
        sendData pool = update (Chart.sendData pool) model
        (_, graph1) = sendData model.hitPool
        (_, graph2) = sendData model.woundPool
        (_, graph3) = sendData model.savePool
      in
      (model, Cmd.batch [graph1, graph2, graph3])

    DrawChart id data ->
      (model, drawChart (id, data, Chart.options))

    _ ->
      (model, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  Html.div [Attr.class "container"]
    [ StatTable.view model.statTable
    , ResultsTable.view model.hitPool
    , ResultsTable.view model.woundPool
    , ResultsTable.view model.savePool
    ]


-- POOL HELPERS

initPools : Model
initPools =
  let
    stats = StatTable.init
    (n, bs, s, ap, _, t, sv) = StatTable.fetch stats
    hits = HitPool.init (n, bs)
    wounds = WoundPool.init (s, t) hits.results
    saves = SavePool.init (ap, sv) wounds.results
  in
    { statTable = stats
    , hitPool = hits
    , woundPool = wounds
    , savePool = saves
    }


updatePools : Model -> Model
updatePools model =
  let
    (n, bs, s, ap, _, t, sv) = StatTable.fetch model.statTable
    hits =  HitPool.update (n, bs) model.hitPool
    wounds = WoundPool.update (s, t) hits.results model.woundPool
    saves = SavePool.update (ap,sv) wounds.results model.savePool
  in
    { model
    | hitPool = hits
    , woundPool = wounds
    , savePool = saves
    }
