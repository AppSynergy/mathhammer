module Mathhammer.Logic.HitPool exposing (Model,init,update,view)

import Html exposing (Html)
import Html.Attributes as Attr
import Mathhammer.Model exposing (Msg)


-- MODEL

type alias Model =
  { bs : Int
  , n : Int
  , results : List Result
  }

type alias Result = (Float, Int)

init : Int -> Int -> Model
init bs n =
  { bs = bs
  , n = n
  , results = []
  }


-- UPDATE

update : a -> Model -> Model
update statTable model =
  { model | results = (0.55, 1) :: model.results}


-- VIEW

view : Model -> Html Msg
view model =
   List.map viewResult model.results
    |> Html.div [Attr.class "well"]


viewResult : Result -> Html Msg
viewResult (prob,val) =
  Html.li [] [Html.text <| toString prob ++ " : " ++ toString val]
