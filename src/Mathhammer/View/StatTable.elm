module Mathhammer.View.StatTable exposing (Model,init,update,view)

import Dict exposing (Dict)
import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event

import Mathhammer.Model exposing (..)

-- MODEL

type alias Model =
  { dict : Dict String String
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


init =
  let f x = case x of
    StatConf player statTag _ default ->
      (toKey <| Playerstat player statTag, toString default)
  in
  { dict = List.map f configs |> Dict.fromList }


-- UPDATE

toKey x = case x of
  Playerstat player statTag -> toString player ++ toString statTag


update stat value model =
  { model | dict = Dict.insert (toKey stat) (value) model.dict }


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
