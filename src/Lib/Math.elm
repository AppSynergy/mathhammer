module Lib.Math exposing (..)

import Model exposing (..)

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
    h (a, b, c) = c

  in
  List.foldr f [] results
