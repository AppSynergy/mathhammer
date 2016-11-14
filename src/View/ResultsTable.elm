module View.ResultsTable exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr
import String

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
  Html.div [Attr.class "well row"]
    [ Html.div [Attr.class "col col-xs-6"]
      [ Html.table [Attr.class "table table-striped"]
        [ Html.tbody [] <| List.map viewChance model.results
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


viewChance : Chance -> Html Msg
viewChance (val, prob) =
  let
    percent x = (String.left 5 <| toString (x*100)) ++ "%  "
  in
  Html.tr []
    [ Html.td [] [Html.text <| toString val]
    , Html.td [] [Html.text <| percent prob]
    ]
