port module App exposing (main)

import Browser
import Cmd.Extra exposing (pure)
import Convert
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode
import ScriptTypes as Script


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port contentsUpdated : String -> Cmd msg



-- Model


type alias Model =
    { code : String
    , script : Script.Script
    , state : State
    }


type State
    = Play { previous : List ( List (List Script.Text), Script.Choice ), current : Script.Scene }
    | Edit


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        contents =
            Decode.decodeValue (Decode.field "contents" Decode.string) flags |> Result.withDefault """Two roads diverged in a yellow wood,
and sorry I could not travel both
and be one traveler, long I stood
and looked down one as far as I could
to where it bent in the undergrowth.

! choice [Take the first, slightly less grassy and worn.]
  -> road more traveled
! choice [Take the second, grassy and perhaps wanting wear.]
  -> road less traveled

### road more traveled
It made no difference at all.

### road less traveled
It made all the difference."""

        script =
            Convert.convert contents
    in
    { code = contents
    , script = script
    , state = Edit
    }
        |> pure



-- Update


type Msg
    = Select Script.Choice
    | Code String
    | Swap


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model.state ) of
        ( Select option, Play state ) ->
            let
                nextScene =
                    model.script
                        |> List.filter (\scene -> option.key == scene.key)
                        |> List.head

                nextPrevious =
                    state.previous ++ [ ( state.current.contents, option ) ]
            in
            case nextScene of
                Just scene ->
                    { model | state = Play { previous = nextPrevious, current = scene } } |> pure

                _ ->
                    -- Error, can't (?) happen
                    model |> pure

        ( Code code, Edit ) ->
            ( { model | code = code, script = Convert.convert code }, contentsUpdated code )

        ( Swap, Edit ) ->
            case model.script of
                [] ->
                    model |> pure

                scene :: _ ->
                    { model | state = Play { previous = [], current = scene } } |> pure

        ( Swap, Play _ ) ->
            { model | state = Edit } |> pure

        _ ->
            model |> pure



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    let
        swap =
            case model.state of
                Play _ ->
                    "Edit"

                _ ->
                    "Play"

        switch =
            el [ alignRight, padding 20, Font.bold ] <|
                el [ Background.color (rgb255 200 230 255), padding 10, Events.onClick Swap, width (px 80) ] <|
                    el [ centerX ] <|
                        text swap
    in
    Element.layout
        [ height fill, width fill, Background.color bgcolor, inFront switch ]
    <|
        case model.state of
            Play { previous, current } ->
                Element.column
                    [ width (px 700), height fill, padding 30, spacing 20, Background.color columncolor, centerX ]
                    ((List.map viewPrevious previous |> List.concat)
                        ++ viewCurrentScene current
                    )

            Edit ->
                Input.multiline
                    [ width (px 700), height fill, padding 20, Background.color columncolor, centerX, Font.family [ Font.typeface "Operator Mono SSm", Font.typeface "Source Code Pro", Font.monospace ], Font.size 12 ]
                    { onChange = Code, text = model.code, placeholder = Nothing, label = Input.labelHidden "Wut", spellcheck = True }


commonButton =
    [ padding 10
    , Border.width 1
    , Border.solid
    , Border.rounded 7
    ]


viewPrevious : ( List (List Script.Text), Script.Choice ) -> List (Element Msg)
viewPrevious ( contents, choice ) =
    List.map viewParagraph contents
        ++ [ paragraph
                (Border.color inactiveButtonColor
                    :: Background.color bgcolor
                    :: Font.color inactiveButtonColor
                    :: commonButton
                )
                (List.map viewText choice.text)
           ]


viewCurrentScene : Script.Scene -> List (Element Msg)
viewCurrentScene current =
    List.map viewParagraph current.contents
        ++ List.map
            (\option ->
                paragraph
                    (Border.color activeButtonColor
                        :: Font.color activeButtonColor
                        :: Events.onClick (Select option)
                        :: commonButton
                    )
                    (List.map viewText option.text)
            )
            current.options


viewParagraph : List Script.Text -> Element Msg
viewParagraph content =
    paragraph [] (List.map viewText content)


viewText : Script.Text -> Element Msg
viewText section =
    case section of
        Script.Styled style str ->
            let
                attrs =
                    (if style.bold then
                        [ Font.bold ]

                     else
                        []
                    )
                        ++ (if style.italic then
                                [ Font.italic ]

                            else
                                []
                           )
                        ++ (if style.strike then
                                [ Font.strike ]

                            else
                                []
                           )
                        ++ (if style.under then
                                [ Font.underline ]

                            else
                                []
                           )
            in
            el attrs (text str)

        Script.Problem ->
            el [] (el [ height (px 10), width (px 10), Background.color (rgb255 255 180 190) ] none)


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
inactiveButtonColor =
    rgb255 100 100 100
