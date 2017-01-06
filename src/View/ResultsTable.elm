module View.ResultsTable exposing (view, viewTable)

import Html exposing (Html)
import Html.Attributes as Attr
import String

import Lib.Dice as Dice
import Lib.Math as Math
import Model exposing (Msg,Chance,AccumChance)


-- VIEW

view : Dice.Pool a -> Html Msg
view model =
  Html.div [Attr.class "col col-xs-12 col-md-6"]
    [ Html.div [Attr.class "results-row row"]
      [ Html.div [Attr.class "col col-xs-6 table-col"]
        [ Html.h2 [] [Html.text model.name]
        , Html.div [Attr.class "details"]
          [ Html.div [] [viewExpectation model]
          , Html.div [] [viewNonZeroEffect model]
          ]
        ]
      , Html.div [Attr.class "col col-xs-6"]
        [ Html.div [Attr.class "canvas-holder"]
          [ Html.canvas [Attr.id model.chartId] [] ]
        ]
      ]
    ]


viewTable : Dice.Pool a -> Html Msg
viewTable model =
  let
    fullResults = Math.accumulate model.results
    rows = List.map viewChance fullResults
    header =
      [ Html.th [] [Html.text "N"]
      , Html.th [] [Html.text ("Exactly N " ++ model.plural)]
      , Html.th [] [Html.text ("At least N " ++ model.plural)]
      ]
  in
  Html.table [Attr.class "table table-striped"]
    [ Html.thead [] header
    , Html.tbody [] rows
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


viewExpectation : Dice.Pool a -> Html Msg
viewExpectation model =
  let
    str = " " ++ model.plural ++ " expected."
  in
  Math.expectation model.results
    |> viewNumTrunc
    >> viewHighlight
    >> (\x -> Html.p [] [x, Html.text str])


viewNonZeroEffect : Dice.Pool a -> Html Msg
viewNonZeroEffect model =
  let
    str = " chance of one or more " ++ model.plural ++ "."
  in
  Math.nonZeroEffect model.results
    |> (*) 100
    >> viewNumTrunc
    >> (\x -> x ++ "%" )
    >> viewHighlight
    >> (\x -> Html.p [] [x, Html.text str])


viewPercent : Float -> String
viewPercent x =
  x
    |> (*) 100
    >> viewNumTrunc
    >> (\x -> x ++ "%" )


viewHighlight : String -> Html Msg
viewHighlight =
  (\x -> Html.span [Attr.class "highlight"] [Html.text x])


viewNumTrunc : Float -> String
viewNumTrunc =
  toString >> String.left 4


viewCheckSum : Dice.Pool a -> Html Msg
viewCheckSum model =
  let
    fsum xs = List.map snd xs |> List.sum
  in
  Html.text <| toString <| fsum model.results
