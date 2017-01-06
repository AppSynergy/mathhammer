module Lib.Math exposing (..)

import Model exposing (..)


expectation : List Chance -> Float
expectation results =
  let
    f (a, b) = b * (toFloat a)
  in
  List.sum (List.map f results)


nonZeroEffect : List Chance -> Float
nonZeroEffect results =
  let
    x = List.filter (\(a, b) -> a == 0) results
  in
  case List.head x of
    Just (a, b) -> (1.0 - b)
    Nothing -> 0.0


accumulate : List Chance -> List AccumChance
accumulate results =
  let

    f : Chance -> List AccumChance -> List AccumChance
    f (i, p) xs = (i, p, g p xs) :: xs

    g : Float -> List AccumChance -> Float
    g p xs = case xs of
      [] -> p
      [x] -> p + h x
      x::xs -> p + h x

    h : AccumChance -> Float
    h (_, _, c) = c

  in
  List.foldr f [] results
