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
  List.map viewChance model.results
    |> List.append [viewCheckSum model]
    |> List.append [Html.canvas [Attr.id model.chartId] []]
    |> Html.div [Attr.class "well"]


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
  Html.li []
    [ Html.span [] [Html.text <| percent prob]
    , Html.strong [] [Html.text <| toString val]
    ]
