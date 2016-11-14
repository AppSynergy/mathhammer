import Html exposing (text)
import Html.App as App
import Html.Attributes as Attr exposing (contenteditable)
import Html.Events as Event exposing (on)
import Json.Decode as Json
import Json.Encode as Encode


type Msg = HelloUpdate String | WorldUpdate String

update msg model =
  case msg of
    HelloUpdate txt ->
      ( { model | hello = txt }, Cmd.none )

    WorldUpdate txt ->
      ( { model | world = txt }, Cmd.none )

initModel =
  { hello = "hello"
  , world = "world"
  }

view model =
  Html.div
    []
    [ Html.div
        [ contenteditable True
        , onInput HelloUpdate
        ]
        [ text model.hello ]
    , Html.div
        [ contenteditable True
        , onChange WorldUpdate
        ]
        [ text model.world ]
    , Html.text <| toString model
    ]

onInput : (String -> msg) -> Html.Attribute msg
onInput msg =
  on "input" (Json.map msg innerHtmlDecoder)

onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
  Json.at ["target", "innerHTML"] Json.string
    |> Json.map handler
    |> Event.on "input"

innerHtmlDecoder =
  Json.at ["target", "innerHTML"] <| Json.string

main =
    App.program
        { init = (initModel, Cmd.none)
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
