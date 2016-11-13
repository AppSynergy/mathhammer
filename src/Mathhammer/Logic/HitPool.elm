module Mathhammer.Logic.HitPool exposing (Model,init,update,view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import String

import Mathhammer.Model exposing (Msg)


-- MODEL

type alias Model =
  { n: Int
  , bs : Int
  , results : List Result
  }

type alias Result = (Int, Float)


init : Int -> Int -> Model
init n bs =
  { n = n
  , bs = bs
  , results = []
  }


toHitChance : Dict Int Float
toHitChance =
  [(1, 1/6), (2, 1/3), (3, 1/2), (4, 2/3), (5, 5/6)] |> Dict.fromList


-- UPDATE

update : { a | attacker_bs : Int, attacker_n : Int } -> Model -> Model
update stats model =
  let model' =
    { model | n = stats.attacker_n , bs = stats.attacker_bs }
  in
  { model' | results = updateResults model' }


updateResults : Model -> List Result
updateResults model =
  case Dict.get model.bs toHitChance of
    Just x -> binomial model.n x <| 1 - x
    Nothing -> model.results


binomial : Int -> Float -> Float -> List Result
binomial n a b =
  let
    f x = let
        a' = (^) a <| toFloat x
        b' = (^) b <| toFloat n - toFloat x
      in
      (x, a' * b' * (toFloat <| nChooseK n x))
  in
  List.map f [0..n]


nChooseK : Int -> Int -> Int
nChooseK n k =
  let
    kmin = min k (n-k)
    collect i b = b * ( (toFloat <| n+1-i) / (toFloat i) )
  in
  if kmin < 0 || k > n then 0
  else List.foldr collect 1 [1..kmin] |> round


-- VIEW

view : Model -> Html Msg
view model =
  List.map viewResult model.results
    |> List.append [viewCheckSum model]
    |> Html.div [Attr.class "well"]


viewCheckSum : Model -> Html Msg
viewCheckSum model =
  let
    fsum xs = List.map snd xs |> List.sum
  in Html.text <| toString <| fsum model.results


viewSummary : Model -> Html Msg
viewSummary model =
  Html.text <| toString <| List.map (nChooseK model.n) [0..(model.n)]


viewResult : Result -> Html Msg
viewResult (val, prob) =
  let percent x = (String.left 5 <| toString (x*100)) ++ "%  " in
  Html.li []
    [ Html.span [] [Html.text <| percent prob]
    , Html.strong [] [Html.text <| toString val]
    ]
