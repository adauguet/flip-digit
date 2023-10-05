module Main exposing (..)

import Array exposing (Array)
import Browser
import Element exposing (Element, centerX, centerY, column, el, fill, height, htmlAttribute, mouseOver, none, paddingXY, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Random exposing (Generator)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { digits : Array FlippableDigit }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { digits = Array.empty }, Random.generate GotFlippableDigits (Random.list 20 digitGenerator) )


type Msg
    = GotFlippableDigits (List FlippableDigit)
    | Flip Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFlippableDigits digits ->
            ( { model | digits = Array.fromList digits }, Cmd.none )

        Flip index ->
            case Array.get index model.digits of
                Just ( digit, flipped ) ->
                    ( { model | digits = Array.set index ( digit, not flipped ) model.digits }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill ] <| contentView model


contentView : Model -> Element Msg
contentView { digits } =
    let
        digitsView =
            row [ spacing 10 ] (List.indexedMap flippableDigitView (Array.toList digits))
    in
    column [ centerX, centerY, spacing 20 ] <|
        if Array.toList digits |> List.all isNotFlipped then
            [ digitsView, el [ Font.color (rgb255 0 200 0) ] <| text "Bravo !" ]

        else
            [ digitsView ]


flippableDigitView : Int -> FlippableDigit -> Element Msg
flippableDigitView index ( digit, flipped ) =
    let
        label =
            if flipped then
                el [ htmlAttribute (Html.Attributes.style "transform" "scale(-1, 1)") ] <|
                    text <|
                        String.fromChar digit

            else
                text <| String.fromChar digit
    in
    Input.button
        [ paddingXY 10 10
        , Background.color (rgb255 220 210 210)
        , Border.rounded 4
        , mouseOver [ Background.color (rgb255 230 220 220) ]
        ]
        { onPress = Just (Flip index)
        , label = label
        }


type alias FlippableDigit =
    ( Char, Bool )


isNotFlipped : FlippableDigit -> Bool
isNotFlipped ( _, flipped ) =
    not flipped


digitGenerator : Generator FlippableDigit
digitGenerator =
    Random.pair
        (Random.uniform '1' [ '2', '3', '4', '5', '6', '7', '9' ])
        (Random.uniform True [ False ])
