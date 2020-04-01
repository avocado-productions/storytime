module App exposing (main)

import Browser
import Cmd.Extra exposing (pure)
import Cyoa exposing (script, start)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import ScriptTypes as Script


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { script : Script.Script
    , previous : List ( String, Script.Choice )
    , current : Script.Scene
    }



init : () -> ( Model, Cmd Msg )
init () =
    { script = script
    , previous = []
    , current = start
    }
        |> pure



-- Update


type Msg
    = Select Script.Choice


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Select option ->
            let
                nextScene =
                    model.script
                        |> List.filter (\scene -> option.key == scene.key)
                        |> List.head
            in
            case nextScene of
                Just scene ->
                    { model
                        | previous = model.previous ++ [ ( model.current.contents, option ) ]
                        , current = scene
                    }
                        |> pure

                _ ->
                    -- Error, can't (?) happen
                    model |> pure



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    Element.layout
        [ height fill, width fill, Background.color bgcolor ]
        (Element.column
            [ width (px 700), height fill, padding 30, spacing 20, Background.color columncolor, centerX ]
            ((List.map viewPrevious model.previous |> List.concat)
                ++ viewCurrent model
            )
        )


commonButton =
    [ padding 10
    , Border.width 1
    , Border.solid
    , Border.rounded 7
    ]


viewPrevious : ( String, Script.Choice ) -> List (Element Msg)
viewPrevious ( contents, choice ) =
    [ paragraph [] [text contents]
    , el
        (Border.color inactiveButtonColor
            :: Background.color bgcolor
            :: Font.color inactiveButtonColor
            :: commonButton
        )
        (text choice.text)
    ]


viewCurrent : Model -> List (Element Msg)
viewCurrent model =
    [ paragraph [] [text model.current.contents]
    , wrappedRow [ spacing 10 ]
        (List.map
            (\option ->
                el
                    (Border.color activeButtonColor
                        :: Font.color activeButtonColor
                        :: Events.onClick (Select option)
                        :: commonButton
                    )
                    (text option.text)
            )
            model.current.options
        )
    ]


bgcolor : Color
bgcolor =
    rgb255 240 240 240


columncolor : Color
columncolor =
    rgb255 255 255 255


activeButtonColor : Color
activeButtonColor =
    rgb255 10 150 130


inactiveButtonColor : Color 
inactiveButtonColor = rgb255 100 100 100
