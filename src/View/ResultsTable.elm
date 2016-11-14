module View.ResultsTable exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr
import String

import Lib.Math as Math
import Model exposing (Msg,Chance)


-- MODEL

type alias Model a =
  { a
  | results : List Chance
  , chartId : String
  }


-- VIEW

view : Model a -> Html Msg
view model =
  let
    rows = List.map viewChance <| Math.accumulate model.results
  in
  Html.div [Attr.class "well row"]
    [ Html.div [Attr.class "col col-xs-6 table-col"]
      [ Html.table [Attr.class "table table-striped"]
        [ Html.tbody [] rows
        ]
      ]
    , Html.div [Attr.class "col col-xs-6"]
      [ Html.canvas [Attr.id model.chartId] []
      ]
    ]

viewCheckSum : Model a -> Html Msg
viewCheckSum model =
  let
    fsum xs = List.map snd xs |> List.sum
  in
  Html.text <| toString <| fsum model.results


viewChance : (Int, Float, Float) -> Html Msg
viewChance (val, prob, accumProb) =
  let
    percent x = (String.left 5 <| toString (x*100)) ++ "%  "

    strongVal = Html.strong [] [Html.text <| toString val]
  in
  Html.tr []
    [ Html.td [] [strongVal]
    , Html.td [] [Html.text <| percent prob]
    , Html.td [] [Html.text <| percent accumProb]
    ]
