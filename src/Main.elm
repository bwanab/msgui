module Main exposing(..)

import Browser
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode exposing (Decoder, field, string)

main = Browser.element {init = init, update = update, view = view, subscriptions = subscriptions}

type alias Model = { content: String
                   , currently_playing: String
                   }

type alias Parameter = { name: String
                       , val: Float
                       }

type alias Node = { name: String
                  , parameters: Parameter
                  , bus_type: String
                  , control: String
                  , node_id: Int
                  , sc_id: Int
                  , val: Float
                  }

type alias NodeParam = { node_id: Int
                       , param_name: String
                       }

type alias Connection = { bus_id: Int
                        , bus_type: String
                        , desc: String
                        , from_node_param: NodeParam
                        , to_node_param: NodeParam
                        }

init : () -> (Model, Cmd Msg)
init _ = ({ content = ""
          , currently_playing = ""
          }, Cmd.none)

type Msg
    = Change String
    | Play
    | StopPlaying
    | GotPlay (Result Http.Error String)



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Change newContent ->
            ({ model | content = newContent }, Cmd.none)

        Play ->
            ( model
            , Http.get
                  { url = "http://localhost:4000/play/" ++ model.content
                  , expect = Http.expectJson GotPlay playDecoder
                  }
            )

        StopPlaying ->
            ( model
            , Http.get
                  { url = "http://localhost:4000/stop"
                  , expect = Http.expectJson GotPlay playDecoder
                  }
            )

        GotPlay result ->
            case result of
                Ok url ->
                    ({ model | currently_playing = url }, Cmd.none)
                Err _ ->
                    (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div []
    ([ div [] [text ("Currently playing " ++ model.currently_playing) ]
     , input [ placeholder "Example to play", value model.content, onInput Change ] []
     ]
  ++ if String.isEmpty model.content
     then []
     else  [ button [ onClick Play ] [ text "Play" ]
           , button [ onClick StopPlaying ] [ text "Stop" ]
           ]
    )



playDecoder : Decoder String
playDecoder =
    field "playing" string
