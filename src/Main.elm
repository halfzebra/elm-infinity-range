module Main exposing (..)

import Html exposing (Html, Attribute, div, text, input, br)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)


-- import Html.Events exposing (onInput)

import InfinityRange exposing (InfinityRange, InfinityRangeConfiguration, InfinityRangeInternalMsg(ExternalUpdate), configureInfinityRange, getValue)


---- MODEL ----


type alias Model =
    { range : InfinityRange
    , inputValue : String
    }


config : InfinityRangeConfiguration
config =
    { floor = 100
    , ceiling = 1000000000
    , step = 100
    , chunkSize = 500000
    , defaultValue = 100
    }


ir =
    configureInfinityRange config


init : ( Model, Cmd Msg )
init =
    ( { range = ir.init, inputValue = toString config.defaultValue }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | InfinityRangeMsg InfinityRangeInternalMsg
    | InputUpdate String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        next =
            case msg of
                InfinityRangeMsg irMsg ->
                    { model | range = ir.update irMsg model.range }

                _ ->
                    model
    in
        ( next, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        attrs =
            ir.attributesFromState model.range
    in
        div []
            [ Html.map InfinityRangeMsg <|
                input
                    ([ type_ "range" ]
                        ++ attrs
                    )
                    []
            , br [] []
            , Html.map InfinityRangeMsg <|
                input
                    [ type_ "text"
                    , onInput ExternalUpdate
                    , value <| getValue model.range
                    ]
                    []
            , br [] []
            , text <| toString model
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
