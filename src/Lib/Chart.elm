module Lib.Chart exposing (..)

import Json.Encode as Json

import Model exposing (Chance)


convert : List Chance -> Json.Value
convert x =
  Json.string "whatever"
