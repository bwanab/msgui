module Main exposing(..)

import Browser
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode exposing (Decoder, field, string, map, map2, int, float, list)
import Time
import Debug
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Bootstrap.Text exposing ( alignSmRight )
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)


main = Browser.element {init = init, update = update, view = view, subscriptions = subscriptions}

type alias Model = { content: String
                   , currently_playing: String
                   , connections: List Connection
                   , bus_vals: List BusVal
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

type alias ReturnVal = { currently_playing: String
                       , connections: List Connection
                       }

type alias StopReturn = { currently_playing: String }

type alias BusVals = { bus_vals: List BusVal }

type alias BusVal = { bus_id: Int
                    , bus_val: Float
                    }

init : () -> (Model, Cmd Msg)
init _ = ({ content = ""
          , currently_playing = "stopped"
          , connections = []
          , bus_vals = []
          }, Cmd.none)

type Msg
    = Change String
    | Play
    | StopPlaying
    | GotPlay (Result Http.Error ReturnVal)
    | GotStop (Result Http.Error StopReturn)
    | GotBusVals (Result Http.Error BusVals)
    | Tick Time.Posix



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
                  , expect = Http.expectJson GotStop stopDecoder
                  }
            )

        GotPlay result ->
            case result of
                Ok rval ->
                    ({ model | currently_playing = rval.currently_playing
                             , connections = rval.connections
                     }, Cmd.none)
                Err _ ->
                    (model, Cmd.none)

        GotStop result ->
            case result of
                Ok rval ->
                    ({ model | currently_playing = rval.currently_playing
                     }, Cmd.none)
                Err _ ->
                    (model, Cmd.none)

        Tick _ ->
            let
                bus_ids = String.join "," (List.map busId
                                               (List.filter isControl model.connections))
            in
                ( model
                , if model.currently_playing /= "stopped" then
                      Http.get
                      { url = "http://localhost:4000/bus_vals/" ++ bus_ids
                      , expect = Http.expectJson GotBusVals busValsDecoder
                      }
                  else Cmd.none
                )

        GotBusVals result ->
            case result of
                Ok rval ->
                    ({ model | bus_vals = rval.bus_vals
                     }, Cmd.none
                    )
                Err _ ->
                    (model, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

view : Model -> Html Msg
view model =
    let
        bd = String.isEmpty model.content
        play_primary = if model.currently_playing == "stopped" then Button.primary else Button.secondary
        stop_primary = if model.currently_playing == "stopped" then Button.secondary else Button.primary
        table =
            Table.table
            { options = [ Table.striped, Table.hover, Table.small ]
            , thead =
                Table.simpleThead
                    [ Table.th [] [ text "Bus Id" ]
                    , Table.th [] [ text "Description" ]
                    , Table.th [] [ text "From" ]
                    , Table.th [] [ text "To" ]
                    , Table.th [] [ text "Bus Val" ]
                    ]
            , tbody =
                Table.tbody []
                    (List.map (connectionRow model.bus_vals)
                         (List.filter isControl model.connections))
            }
        tableGrid = if model.currently_playing == "stopped"
                    then
                        Grid.row [ Row.leftXs ] [ Grid.col [Col.xs8] [ text "" ] ]
                    else
                        Grid.row [ Row.leftXs ]
                            [ Grid.col [ Col.xs8 ] [ table ] ]
    in
    div []
        [ CDN.stylesheet
        , Grid.row [ Row.leftXs ]
            [ Grid.col [Col.xs3]
                   [text ("Currently playing " ++ model.currently_playing) ]
            , Grid.col [ Col.xs4 ] [ InputGroup.config
                                         (InputGroup.text [ Input.placeholder "Example to play"
                                                          , Input.onInput Change ])
                                   |> InputGroup.predecessors
                                         [ InputGroup.span [] [ text "@" ] ]
                                   |> InputGroup.view]
            ]
        , Grid.row [ Row.leftXs ]
            [ Grid.col [ Col.xs1 ] [ Button.button [ play_primary, Button.disabled bd, Button.onClick Play ] [ text "Play" ] ]
            , Grid.col [ Col.xs1 ] [ Button.button [ stop_primary, Button.disabled bd, Button.onClick StopPlaying ] [ text "Stop" ] ]
            ]
        , tableGrid
        ]


busId : Connection -> String
busId connection = String.fromInt connection.bus_id

isControl : Connection -> Bool
isControl connection = connection.bus_type == "control"

isBusId : Int -> BusVal -> Bool
isBusId bus_id bus_val =
    bus_id == bus_val.bus_id

findFirst : (a -> Bool) -> List a -> Maybe a
findFirst f vals =
    List.filter f vals |> List.head

connectionRow : List BusVal -> Connection -> Table.Row msg
connectionRow bus_vals connection =
    Table.tr []
        [ Table.td [] [ text (String.fromInt connection.bus_id) ]
        , Table.td [] [ text connection.desc ]
        , Table.td [] [ text connection.from_node_param.param_name ]
        , Table.td [] [ text connection.to_node_param.param_name ]
        , Table.td [ Table.cellAttr <| align "right " ]
            [ text
                  (case (findFirst (isBusId connection.bus_id) bus_vals) of
                       Nothing -> ""
                       Just a -> format { usLocale | decimals = 2 } a.bus_val
                  )]
        ]

stopDecoder : Decoder StopReturn
stopDecoder =
    Json.Decode.map StopReturn
        ( field "playing" string )

playDecoder : Decoder ReturnVal
playDecoder =
    Json.Decode.map2 ReturnVal
        ( field "playing" string )
        ( field "connections" (Json.Decode.list connectionDecoder))


connectionDecoder : Decoder Connection
connectionDecoder =
    Json.Decode.map5 Connection
        ( field "bus_id" int )
        ( field "bus_type" string )
        ( field "desc" string )
        ( field "from_node_param" nodeParamDecoder)
        ( field "to_node_param" nodeParamDecoder)

nodeParamDecoder : Decoder NodeParam
nodeParamDecoder =
    Json.Decode.map2 NodeParam
        ( field "node_id" int )
        ( field "param_name" string)

busValsDecoder : Decoder BusVals
busValsDecoder =
    Json.Decode.map BusVals
        ( field "bus_map" (Json.Decode.list busValDecoder))

busValDecoder : Decoder BusVal
busValDecoder =
    Json.Decode.map2 BusVal
        ( field "bus_id" int )
        ( field "bus_val" float )
