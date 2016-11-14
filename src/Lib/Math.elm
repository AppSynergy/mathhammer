module Lib.Math exposing (..)

import Model exposing (..)


expectation : List Chance -> Float
expectation results =
  0.42
  --List.map snd results
  --  |> List.sum
  --  |> Debug.log "summs"


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
