module Mathhammer.View.StatTable exposing (Model,init,update,view)

import Dict exposing (Dict)
import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import String

import Mathhammer.Model exposing (..)

-- MODEL

type alias Model =
  { attacker_bs : Int
  , attacker_n : Int
  , attacker_s : Int
  , defender_t : Int
  }

type Config = StatConf Player StatTag (List Int) Int

configs : List Config
configs =
  [ StatConf Attacker N [1..20] 5
  , StatConf Attacker BS [2..5] 4
  , StatConf Attacker S [1..10] 4
  , StatConf Attacker AP [1..6] 5
  , StatConf Defender N [1..20] 10
  , StatConf Defender T [2..7] 3
  , StatConf Defender Sv [2..6] 5
  ]


init : Model
init =
  { attacker_bs = 4
  , attacker_n = 5
  , attacker_s = 4
  , defender_t = 3
  }


-- UPDATE


update stat value model =
  let
    --debug = Debug.log "statTable::model" model
    intVal = case (String.toInt value) of
      Ok i -> i
      Err str -> 0
  in
  case stat of
    Playerstat Attacker N -> { model | attacker_n = intVal }
    Playerstat Attacker BS -> { model | attacker_bs = intVal }
    Playerstat Attacker S -> { model | attacker_s = intVal }
    Playerstat Defender T -> { model | defender_t = intVal }
    _ -> model

-- VIEW

view : Model -> Html Msg
view model =
  let text = Html.text in
  Html.table [Attr.class "table"]
  [ throw
    [ text "No. of Attackers", text "BS"
    , text "Strength", text "AP"
    , text "No. of Defenders"
    , text "Toughness", text "Armor Save"
    ]
  , tdrow <| List.map toTSel configs
  ]


toTSel config =
  case config of
    StatConf player statTag values default ->
      viewSelector player statTag values default


throw : List (Html Msg) -> Html Msg
throw vals =
  Html.tr [] <| List.map (\x -> Html.th [] [x]) vals


tdrow vals =
  Html.tr [] <| List.map (\x -> Html.td [] [x]) vals


viewSelector : Player -> StatTag -> List Int -> Int -> Html Msg
viewSelector player statTag values default =
  let
    stat = Playerstat player statTag
    opt x =
      Html.option
        [ Attr.value <| toString x
        , Attr.selected <| default == x
        ]
        [ Html.text <|toString x ]
  in
  List.map opt values
    |> Html.select [onChange (\x -> UpdateStat stat x)]


onChange : (String -> Msg) -> Html.Attribute Msg
onChange handler =
  Json.at ["target", "value"] Json.string
    |> Json.map handler
    |> Event.on "change"
