module View.StatTable exposing (Model,init,update,view)

import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import String

import Model exposing (Stat, Msg)

-- MODEL

type alias Model =
  { attacker_bs : Stat
  , attacker_n : Stat
  , attacker_s : Stat
  , attacker_ap : Stat
  , defender_t : Stat
  }


init : Model
init =
  { attacker_bs =
    { value = 1
    , range = [1..5]
    }
  , attacker_n =
    { value = 3
    , range = [1..20]
    }
  , attacker_s =
    { value = 4
    , range = [1..10]
    }
  , attacker_ap =
    { value = 5
    , range = [1..6]
    }
  , defender_t =
    { value = 3
    , range = [1..10]
    }
  }


-- UPDATE

update stat value model =
  let
    intVal s = case (String.toInt value) of
      Ok i -> { s | value = i }
      Err str -> { s | value = 0 }
  in
  case stat of
    "attacker_n" -> { model | attacker_n = intVal model.attacker_n }
    "attacker_bs" -> { model | attacker_bs = intVal model.attacker_bs }
    "attacker_s" -> { model | attacker_s = intVal model.attacker_s }
    "attacker_ap" -> { model | attacker_ap = intVal model.attacker_ap }
    "defender_t" -> { model | defender_t = intVal model.defender_t }
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
    , (repeatRow Html.td) <| viewControls model
    ]


viewControls : Model -> List (Html Msg)
viewControls model =
  [ viewSelect "attacker_n" model.attacker_n.range model.attacker_n.value
  , viewSelect "attacker_bs" model.attacker_bs.range model.attacker_bs.value
  , viewSelect "attacker_s" model.attacker_s.range model.attacker_s.value
  , viewSelect "attacker_ap" model.attacker_ap.range model.attacker_ap.value
  , viewSelect "defender_t" model.defender_t.range model.defender_t.value
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
    |> Html.select [onChange (\x -> Model.UpdateStat label x)]


onChange : (String -> Msg) -> Html.Attribute Msg
onChange handler =
  Json.at ["target", "value"] Json.string
    |> Json.map handler
    |> Event.on "change"
