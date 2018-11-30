module InfinityRange
    exposing
        ( InfinityRange
        , InfinityRangeConfiguration
        , InfinityRangeInternalMsg(ExternalUpdate)
        , configureInfinityRange
        , init
        , attributesFromState
        , internalUpdate
        , updateValue
        , externalUpdate
        , getValue
        )

{-|

This library is aimed at creation of an infinity-range input.

It looks like the original `<input type="range">`, but allows users to enter very large values.

# Definition
@docs InfinityRange, InfinityRangeConfiguration, InfinityRangeInternalMsg

# Creators
@docs configureInfinityRange

# Low Level API

-}

import Html exposing (Attribute)
import Html.Attributes
import Html.Events exposing (onMouseUp, onInput)


type InfinityRange
    = InfinityRange InfinityRangeState


type alias InfinityRangeState =
    { value : String
    , min : String
    , max : String
    }


type alias InfinityRangeConfiguration =
    { floor : Int
    , ceiling : Int
    , step : Int
    , chunkSize : Int
    , defaultValue : Int
    }

default: InfinityRangeConfiguration
default =
    { floor = 100
    , ceiling = 1000000000
    , step = 100
    , chunkSize = 500000
    , defaultValue = 100
    }

attributesFromState : InfinityRangeConfiguration -> InfinityRange -> List (Attribute InfinityRangeInternalMsg)
attributesFromState { step } (InfinityRange { min, max, value }) =
    [ Html.Attributes.min min
    , Html.Attributes.max max
    , Html.Attributes.value value
    , Html.Attributes.step (toString step)
    , onMouseUp InternalUpdate
    , onInput UpdateValue
    ]


init : InfinityRangeConfiguration -> InfinityRange
init config =
    let
        initialValue =
            if defaultValue < floor then
                floor
            else if ceiling < defaultValue then
                ceiling
            else
                defaultValue

        { chunkSize, ceiling, floor, defaultValue } =
            config
    in
        if chunkSize < defaultValue then
            internalUpdate config
                (state_
                    initialValue
                    (initialValue - chunkSize)
                    (initialValue + chunkSize)
                )
        else
            state_ initialValue floor chunkSize


internalUpdate : InfinityRangeConfiguration -> InfinityRange -> InfinityRange
internalUpdate { chunkSize, floor, defaultValue } state =
    let
        (InfinityRange { value, min, max }) =
            state

        min_ : Int
        min_ =
            Result.withDefault floor (String.toInt min)

        max_ : Int
        max_ =
            Result.withDefault chunkSize (String.toInt max)

        value_ : Int
        value_ =
            value
                |> String.toInt
                |> Result.withDefault defaultValue

        nextMin : Int
        nextMin =
            value_ - chunkSize
    in
        if value_ == min_ then
            state_
                value_
                (if nextMin < floor then
                    floor
                 else
                    nextMin
                )
                (if value_ == floor then
                    chunkSize
                 else
                    value_ + chunkSize
                )
        else if value_ == max_ then
            state_
                value_
                (if nextMin < floor then
                    floor
                 else
                    nextMin
                )
                (value_ + chunkSize)
        else
            state


state_ : Int -> Int -> Int -> InfinityRange
state_ value min max =
    InfinityRange
        { value = toString value
        , min = toString min
        , max = toString max
        }


updateValue : InfinityRangeConfiguration -> InfinityRange -> String -> InfinityRange
updateValue { defaultValue } (InfinityRange { min, max }) rawValue =
    InfinityRange
        (InfinityRangeState
            rawValue
            min
            max
        )


externalUpdate : InfinityRangeConfiguration -> String -> InfinityRange
externalUpdate config str =
    init { config | defaultValue = (Result.withDefault config.defaultValue (String.toInt str)) }


type alias ConfiguredInfinityRange =
    { update : InfinityRangeInternalMsg -> InfinityRange -> InfinityRange
    , attributesFromState : InfinityRange -> List (Attribute InfinityRangeInternalMsg)
    , init: InfinityRange
    }


configureInfinityRange : InfinityRangeConfiguration -> ConfiguredInfinityRange
configureInfinityRange config =
    { update = update config
    , attributesFromState = attributesFromState config
    , init = init config
    }


type InfinityRangeInternalMsg
    = UpdateValue String
    | ExternalUpdate String
    | InternalUpdate


update : InfinityRangeConfiguration -> InfinityRangeInternalMsg -> InfinityRange -> InfinityRange
update config msg model =
    case msg of
        UpdateValue str ->
            updateValue config model str

        ExternalUpdate str ->
            externalUpdate config str

        InternalUpdate ->
            internalUpdate config model

getValue : InfinityRange -> String
getValue (InfinityRange { value }) =
    value