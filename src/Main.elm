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
  { statTable = StatTable.init
  , hitPool = HitPool.init 4 4
  }


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    UpdateStat stat value ->
      ( { model
        | statTable = StatTable.update stat value model.statTable
        , hitPool = HitPool.update (stat, value) model.hitPool
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
