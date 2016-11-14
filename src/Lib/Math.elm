module Lib.Math exposing (..)

import Model exposing (..)


expectation : List AccumChance -> Float
expectation results =
  let
    foo : Float
    foo = List.sum g
    f (_, b, _) = b
    g : List Float
    g = (List.map f results)
  in
  44

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
