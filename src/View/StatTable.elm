module View.StatTable exposing (Model,fetch,init,update,view)

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
  , defender_n : Stat
  , defender_t : Stat
  , defender_sv : Stat
  , defender_fnp : Stat
  , iterable : Iterable
  }


type Iterable =
  Iterable (List (String, Model -> Stat))


fetch : Model -> (Int, Int, Int, Int, Int, Int, Int, Int)
fetch model =
  let
    n = model.attacker_n.value
    bs = model.attacker_bs.value
    s = model.attacker_s.value
    ap = model.attacker_ap.value
    n2 = model.defender_n.value
    t = model.defender_t.value
    sv = model.defender_sv.value
    fnp = model.defender_fnp.value
  in
  (n, bs, s, ap, n2, t, sv, fnp)


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
    { value = 4
    , range = [1..6]
    }
  , defender_fnp =
    { value = 5
    , range = [1..6]
    }
  , iterable = Iterable
    [ ("attacker_n", .attacker_n)
    , ("attacker_bs", .attacker_bs)
    , ("attacker_s", .attacker_s)
    , ("attacker_ap", .attacker_ap)
    , ("defender_n", .defender_n)
    , ("defender_t", .defender_t)
    , ("defender_sv", .defender_sv)
    , ("defender_fnp", .defender_fnp)
    ]
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
    "defender_fnp" -> { model | defender_fnp = intVal v model.defender_fnp }
    _ -> model


viewAttackerHeaders : List (Html Msg)
viewAttackerHeaders =
  [ Html.text "Number"
  , Html.text "BS"
  , Html.text "S"
  , Html.text "AP"
  ]


viewDefenderHeaders : List (Html Msg)
viewDefenderHeaders =
  [ Html.text "Number"
  , Html.text "T"
  , Html.text "Save"
  , Html.text "Feel No Pain"
  ]


viewAttackerInput : Model -> List (Html Msg)
viewAttackerInput model =
  [ viewSelect "attacker_n" model.attacker_n.range model.attacker_n.value
  , viewSelect "attacker_bs" model.attacker_bs.range model.attacker_bs.value
  , viewSelect "attacker_s" model.attacker_s.range model.attacker_s.value
  , viewSelect "attacker_ap" model.attacker_ap.range model.attacker_ap.value
  ]


viewDefenderInput : Model -> List (Html Msg)
viewDefenderInput model =
  [ viewSelect "defender_n" model.defender_n.range model.defender_n.value
  , viewSelect "defender_t" model.defender_t.range model.defender_t.value
  , viewSelect "defender_sv" model.defender_sv.range model.defender_sv.value
  , viewSelect "defender_fnp" model.defender_fnp.range model.defender_fnp.value
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
  Html.div [Attr.class "input-row row"]
    [ Html.div [Attr.class "col col-xs-6"]
      [ Html.h2 [] [Html.text "Attacker"]
      , Html.table [Attr.class "table stat-table"]
        [ Html.tbody []
          [ viewAttackerHeaders |> repeatRow Html.th
          , viewAttackerInput model |> repeatRow Html.td
          ]
        ]
      ]
    , Html.div [Attr.class "col col-xs-6"]
      [ Html.h2 [] [Html.text "Defender"]
      , Html.table [Attr.class "table stat-table"]
        [ Html.tbody []
          [ viewDefenderHeaders |> repeatRow Html.th
          , viewDefenderInput model |> repeatRow Html.td
          ]
        ]
      ]
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
