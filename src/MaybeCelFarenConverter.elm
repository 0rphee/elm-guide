module MaybeCelFarenConverter exposing (main)

import Browser
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { celsiusFarenheitInput : String
    , inchMeterInput : String
    }


init : Model
init =
    { celsiusFarenheitInput = ""
    , inchMeterInput = ""
    }



-- UPDATE


type Msg
    = CelsiusFarChange String
    | InchMeterChange String


update : Msg -> Model -> Model
update msg model =
    case msg of
        CelsiusFarChange newCelsius ->
            { model | celsiusFarenheitInput = newCelsius }

        InchMeterChange newInch ->
            { model | inchMeterInput = newInch }



-- VIEW


celsiusToFarenheit : Float -> Float
celsiusToFarenheit c =
    c * 1.8 + 32


inchesToMeters : Float -> Float
inchesToMeters i =
    i * 0.0254


view : Model -> Html Msg
view model =
    let
        celsiusFarenView =
            converter "°C = " "°F" CelsiusFarChange celsiusToFarenheit model.celsiusFarenheitInput

        inchMetersView =
            converter "in = " "m" InchMeterChange inchesToMeters model.inchMeterInput
    in
    div []
        [ celsiusFarenView
        , div [] []
        , inchMetersView
        ]


converter : String -> String -> (String -> Msg) -> (Float -> Float) -> String -> Html Msg
converter leftLabel rightLabel msgConstructor conversionFunc userInputStr =
    let
        ( resultStr, resultColor, formStyle ) =
            case String.toFloat userInputStr of
                Just float ->
                    ( String.fromFloat <| conversionFunc float, "blue", [ style "width" "40px" ] )

                Nothing ->
                    ( "???", "red", [ style "width" "40px", style "border" "1px solid red" ] )
    in
    span []
        [ input ([ value userInputStr, onInput msgConstructor ] ++ formStyle) []
        , text leftLabel
        , span [ style "color" resultColor ] [ text resultStr ]
        , text rightLabel
        ]
