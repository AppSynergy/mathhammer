module Main exposing (main)

import Task
import Html exposing (Html)
import Html.Attributes as Attr
import Html.App

import Mathhammer.Model exposing (..)
import Mathhammer.Logic.HitPool as HitPool
import Mathhammer.View.StatTable as StatTable


-- MAIN PROGRAM

main : Program Never
main = Html.App.program
    { init = (init, Cmd.none)
    , view = view
    , update = update
    , subscriptions = (\model -> Sub.none)
    }


-- MODEL

type alias Model =
  { statTable : StatTable.Model
  , hitPool : HitPool.Model
  }


init : Model
init =
  let
    stats = StatTable.init
  in
  { statTable = stats
  , hitPool = HitPool.init stats.attacker_n stats.attacker_bs
  }


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NoOp ->
      (model, Cmd.none)

    Boot ->
      ( { model | hitPool = HitPool.update model.statTable model.hitPool
        }
      , Cmd.none
      )

    UpdateStat stat value ->
      let stats = StatTable.update stat value model.statTable in
      ( { model
        | statTable = stats
        , hitPool = HitPool.update stats model.hitPool
        }
      , Cmd.none
      )


-- VIEW

view : Model -> Html Msg
view model =
  Html.div [Attr.class "container"]
    [ StatTable.view model.statTable
    , HitPool.view model.hitPool
    ]
