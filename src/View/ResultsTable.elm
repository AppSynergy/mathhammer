module View.ResultsTable exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr
import String

import Lib.Math as Math
import Model exposing (Msg,Chance,AccumChance)


-- MODEL

type alias Model a =
  { a
  | results : List Chance
  , name : String
  , chartId : String
  }


-- VIEW

view : Model a -> Html Msg
view model =
  let
    fullResults = Math.accumulate model.results
    rows = List.map viewChance fullResults
    header =
      [ Html.th [] [Html.text model.name]
      , Html.th [] [Html.text "Exact %"]
      , Html.th [] [Html.text "At least %"]
      ]
    expect : Float
    expect = Math.expectation model.results
    expect_str : String
    expect_str = "Expect : " ++ (String.left 4 <| toString expect)

  in
  Html.div [Attr.class "well row"]
    [ Html.div [Attr.class "col col-xs-6 table-col"]
      [ Html.table [Attr.class "table table-striped"]
        [ Html.thead [] header
        , Html.tbody [] rows
        ]
      ]
    , Html.div [Attr.class "col col-xs-6"]
      [ Html.div []
        [ Html.canvas [Attr.id model.chartId] []
        ]
      , Html.div [] [Html.text expect_str]
      ]
    ]


viewChance : AccumChance -> Html Msg
viewChance (val, prob, accumProb) =
  let
    strongVal = Html.strong [] [Html.text <| toString val]
  in
  Html.tr []
    [ Html.td [] [strongVal]
    , Html.td [] [Html.text <| viewPercent prob]
    , Html.td [] [Html.text <| viewPercent accumProb]
    ]


viewPercent : Float -> String
viewPercent x =
  (String.left 4 <| toString (x * 100)) ++ "%  "


viewCheckSum : Model a -> Html Msg
viewCheckSum model =
  let
    fsum xs = List.map snd xs |> List.sum
  in
  Html.text <| toString <| fsum model.results
