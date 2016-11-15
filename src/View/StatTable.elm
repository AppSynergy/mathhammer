module View.StatTable exposing (Model,fetch,init,update,view)

import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import String

import Model exposing (Stat, Msg)

update2 : (a -> b) -> (b -> a -> a) -> (b -> b) -> a -> a
update2 get set f x = set (f (get x)) x


-- MODEL

type alias Model =
  { attacker_bs : Stat
  , attacker_n : Stat
  , attacker_s : Stat
  , attacker_ap : Stat
  , defender_n : Stat
  , defender_t : Stat
  , defender_sv : Stat
  }


fetch : Model -> (Int, Int, Int, Int, Int, Int, Int)
fetch stats =
  let
    n = stats.attacker_n.value
    bs = stats.attacker_bs.value
    s = stats.attacker_s.value
    ap = stats.attacker_ap.value
    n2 = stats.defender_n.value
    t = stats.defender_t.value
    sv = stats.defender_sv.value
  in
  (n, bs, s, ap, n2, t, sv)


init : Model
init =
  { attacker_bs =
    { value = 4
    , range = [1..5]
    }
  , attacker_n =
    { value = 5
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
  , defender_n =
    { value = 5
    , range = [1..20]
    }
  , defender_t =
    { value = 3
    , range = [1..10]
    }
  , defender_sv =
    { value = 5
    , range = [1..6]
    }
  }


-- UPDATE

update : String -> String -> Model -> Model
update stat v model =
  case stat of
    "attacker_n" -> { model | attacker_n = intVal v model.attacker_n }
    "attacker_bs" -> { model | attacker_bs = intVal v model.attacker_bs }
    "attacker_s" -> { model | attacker_s = intVal v model.attacker_s }
    "attacker_ap" -> { model | attacker_ap = intVal v model.attacker_ap }
    "defender_n" -> { model | defender_n = intVal v model.defender_n }
    "defender_t" -> { model | defender_t = intVal v model.defender_t }
    "defender_sv" -> { model | defender_sv = intVal v model.defender_sv }
    _ -> model


viewHeaders : List (Html Msg)
viewHeaders =
  [ Html.text "No. of Attackers"
  , Html.text "BS"
  , Html.text "Strength"
  , Html.text "AP"
  , Html.text "No. of Defenders"
  , Html.text "Toughness"
  , Html.text "Armor Save"
  ]


viewControls : Model -> List (Html Msg)
viewControls model =
  [ viewSelect "attacker_n" model.attacker_n.range model.attacker_n.value
  , viewSelect "attacker_bs" model.attacker_bs.range model.attacker_bs.value
  , viewSelect "attacker_s" model.attacker_s.range model.attacker_s.value
  , viewSelect "attacker_ap" model.attacker_ap.range model.attacker_ap.value
  , viewSelect "defender_n" model.defender_n.range model.defender_n.value
  , viewSelect "defender_t" model.defender_t.range model.defender_t.value
  , viewSelect "defender_sv" model.defender_sv.range model.defender_sv.value
  ]


-- SENSIBLE LAND

intVal : String -> Stat -> Stat
intVal value stat =
  case (String.toInt value) of
  Ok i -> { stat | value = i }
  Err str -> stat


-- VIEW

view : Model -> Html Msg
view model =
  let
    repeatRow ele vals =
      Html.tr [] <| List.map (\x -> ele [] [x]) vals
  in
  Html.table [Attr.class "table"]
    [ viewHeaders |> repeatRow Html.th
    , viewControls model |> repeatRow Html.td
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
