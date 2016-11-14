module View.StatTable exposing (Model,init,update,view)

import Dict exposing (Dict)
import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import String

import Model exposing (..)

-- MODEL

type alias Model =
  { attacker_bs : { value : Int }
  , attacker_n : { value : Int }
  , attacker_s : { value : Int }
  , defender_t : { value : Int }
  }


init : Model
init =
  { attacker_bs = { value = 4 }
  , attacker_n = { value = 4 }
  , attacker_s = { value = 4 }
  , defender_t = { value = 4 }
  }


-- UPDATE

update stat value model =
  let
    intVal = case (String.toInt value) of
      Ok i -> { value = i }
      Err str -> { value = 0 }
  in
  case stat of
    "attacker_n" -> { model | attacker_n = intVal }
    "attacker_bs" -> { model | attacker_bs = intVal }
    "attacker_s" -> { model | attacker_s = intVal }
    "defender_t" -> { model | defender_t = intVal }
    _ -> model


-- VIEW

view : Model -> Html Msg
view model =
  let
    repeatRow ele vals =
      Html.tr [] <| List.map (\x -> ele [] [x]) vals
  in
  Html.table [Attr.class "table"]
    [ (repeatRow Html.th)
      [ Html.text "No. of Attackers"
      , Html.text "BS"
      , Html.text "Strength"
      , Html.text "AP"
      , Html.text "No. of Defenders"
      , Html.text "Toughness"
      , Html.text "Armor Save"
      ]
    , (repeatRow Html.td) <| toTSel model
    ]


toTSel : Model -> List (Html Msg)
toTSel model =
  [ viewSelect "attacker_n" [1..20] model.attacker_n.value
  , viewSelect "attacker_bs" [1..6] model.attacker_bs.value
  , viewSelect "attacker_s" [1..10] model.attacker_s.value
  , viewSelect "defender_t" [1..10] model.defender_t.value
  ]


viewSelect : String -> List Int -> Int -> Html Msg
viewSelect label values default =
  let
    opt x =
      Html.option
        [ Attr.value <| toString x
        , Attr.selected <| default == x
        ]
        [ Html.text <|toString x ]
  in
  List.map opt values
    |> Html.select [onChange (\x -> UpdateStat label x)]


onChange : (String -> Msg) -> Html.Attribute Msg
onChange handler =
  Json.at ["target", "value"] Json.string
    |> Json.map handler
    |> Event.on "change"
