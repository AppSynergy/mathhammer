port module Main exposing (main)

import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App

import Lib.Chart as Chart
import Model exposing (..)
import Module.HitPool as HitPool
import Module.WoundPool as WoundPool
import Module.ArmorSavePool as APool
import Module.FeelNoPainSavePool as FNPool
import Module.Pool as Pool
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

port drawChart : (String, String, Json.Value) -> Cmd msg


-- MODEL

type alias Model =
  { statTable : StatTable.Model
  , hitPool : Pool.Model
  , woundPool : Pool.Model
  , aPool : Pool.Model
  , fnpPool : Pool.Model
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
        (_, graph3) = sendData model.aPool
        (_, graph4) = sendData model.fnpPool
      in
      (model, Cmd.batch [graph1, graph2, graph3, graph4])

    DrawChart id data name ->
      (model, drawChart (id, data, name))

    _ ->
      (model, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  Html.div [Attr.class "container"]
    [ StatTable.view model.statTable
    , Html.div [Attr.class "nice-results row"]
      [ ResultsTable.view model.hitPool
      , ResultsTable.view model.woundPool
      , ResultsTable.view model.aPool
      , ResultsTable.view model.fnpPool
      ]
    , Html.div [Attr.class "table-results"]
      [ ResultsTable.viewTable model.hitPool
      , ResultsTable.viewTable model.woundPool
      , ResultsTable.viewTable model.aPool
      , ResultsTable.viewTable model.fnpPool
      ]
    ]


-- POOL HELPERS

initPools : Model
initPools =
  let
    stats = StatTable.init
    (n, bs, s, ap, _, t, sv, fnp) = StatTable.fetch stats
    hits = Pool.init (bs, n) []
    wounds = Pool.init (s, t) hits.results
    aSaves = Pool.init (ap, sv) wounds.results
    fnpSaves = Pool.init (0, fnp) aSaves.results
  in
    { statTable = stats
    , hitPool = HitPool.init hits
    , woundPool = WoundPool.init wounds
    , aPool = APool.init aSaves
    , fnpPool = FNPool.init fnpSaves
    }


updatePools : Model -> Model
updatePools model =
  let
    (n, bs, s, ap, _, t, sv, fnp) = StatTable.fetch model.statTable
    hits =  Pool.update HitPool.update (bs, n) [] model.hitPool
    wounds = Pool.update WoundPool.update (s, t) hits.results model.woundPool
    aSaves = Pool.update APool.update (ap,sv) wounds.results model.aPool
    fnpSaves = Pool.update FNPool.update (0, fnp) aSaves.results model.fnpPool
  in
    { model
    | hitPool = hits
    , woundPool = wounds
    , aPool = aSaves
    , fnpPool = fnpSaves
    }
