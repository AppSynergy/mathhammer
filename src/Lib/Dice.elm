module Lib.Dice exposing (..)

import Dict exposing (Dict)
import List.Extra

import Model exposing (Chance)


type alias Pool hasStats =
  { hasStats
  | input : List Chance
  , results : List Chance
  , chartId : String
  , name : String
  }


toHitChance : Dict Int Float
toHitChance =
  [ (1, 1/6)
  , (2, 1/3)
  , (3, 1/2)
  , (4, 2/3)
  , (5, 5/6)
  ] |> Dict.fromList


toWoundChance : Dict Int Float
toWoundChance =
  [ (-3, 1/6)
  , (-2, 1/6)
  , (-1, 1/3)
  , (0, 1/2)
  , (1, 2/3)
  , (2, 5/6)
  ] |> Dict.fromList


toFailSaveChance : Dict Int Float
toFailSaveChance =
  [ (2, 1/6)
  , (3, 1/3)
  , (4, 1/2)
  , (5, 2/3)
  , (6, 5/6)
  ] |> Dict.fromList


binomial : Int -> Float -> Float -> List Chance
binomial n a b =
  let
    f x =
      let
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


expand : Float -> List Chance -> List Chance
expand toWound input =
  let
    calcToWound : Chance -> List Chance
    calcToWound (a,b) =
      binomial a toWound (1 - toWound)
        |> List.map (\(c,d) -> (c, b * d))

    sumByIndex : List Chance -> Chance
    sumByIndex xs =
      List.foldr (\(a,b) (c,d) -> (a, b + d)) (0, 0.0) xs
  in
  input
    |> List.concatMap calcToWound
    |> List.sort
    |> List.Extra.groupWhile (\(a,b) (c,d) -> a == c)
    |> List.map sumByIndex
