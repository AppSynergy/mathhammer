module Mathhammer.Logic.HitPool exposing (Model,init,update,view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr

import Mathhammer.Model exposing (Msg,Chance)
import Mathhammer.View.ResultsTable as ResultsTable

-- MODEL

type alias Model =
  { n: Int
  , bs : Int
  , results : List Chance
  }


type alias HasBSN a =
  { a | attacker_bs : Int, attacker_n : Int }


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

update : HasBSN a -> Model -> Model
update stats model =
  let model' =
    { model | n = stats.attacker_n , bs = stats.attacker_bs }
  in
  { model' | results = updateChances model' }


updateChances : Model -> List Chance
updateChances model =
  case Dict.get model.bs toHitChance of
    Just x -> binomial model.n x <| 1 - x
    Nothing -> model.results


binomial : Int -> Float -> Float -> List Chance
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
  ResultsTable.view model
