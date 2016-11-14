module Lib.Dice exposing (..)

import Dict exposing (Dict)

import Model exposing (Chance)


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
