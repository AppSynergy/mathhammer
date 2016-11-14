module Main exposing (main)

import Task
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App

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
    , subscriptions = (\model -> Sub.none)
    }


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
    hits = HitPool.init stats.attacker_n stats.attacker_bs
    wounds = WoundPool.init stats.attacker_s stats.defender_t hits.results
  in
  ( { statTable = stats
    , hitPool = hits
    , woundPool = wounds
    }
  , Task.perform (\_ -> NoOp) (\_ -> Boot) (Task.succeed "ok")
  )


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NoOp ->
      (model, Cmd.none)

    Boot ->
      let
        stats = model.statTable
        hits =  HitPool.update stats model.hitPool
        wounds = WoundPool.update stats hits.results model.woundPool
      in
      ( { model
        | hitPool = hits
        , woundPool = wounds
        }
      , Cmd.none
      )

    UpdateStat stat value ->
      let
        stats = StatTable.update stat value model.statTable
        hits =  HitPool.update stats model.hitPool
        wounds = WoundPool.update stats hits.results model.woundPool
      in
      ( { model
        | statTable = stats
        , hitPool = hits
        , woundPool = wounds
        }
      , Cmd.none
      )


-- VIEW

view : Model -> Html Msg
view model =
  Html.div [Attr.class "container"]
    [ StatTable.view model.statTable
    , HitPool.view model.hitPool
    , WoundPool.view model.woundPool
    ]
