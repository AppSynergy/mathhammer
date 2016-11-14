module Lib.Math exposing (..)

import Model exposing (..)


expectation : List AccumChance -> Float
expectation results =
  let
    foo : Float
    foo = List.sum (List.map f results)
    f (_, b, _) = b
  in
  case List.head results of
    Just (t, p, c) ->
      let debug = Debug.log "head" (t, p, c) in
      p -- TODO wtf
    Nothing -> 0.42

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
