port module App exposing (main)

import Browser
import Cmd.Extra exposing (pure)
import Config
import Convert
import Display
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode
import Parse
import ScriptTypes as Script exposing (Key)
import Set
import Types
import View


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


colors =
    { gray1 = rgb255 248 248 248
    , gray2 = rgb255 184 184 184
    , gray3 = rgb255 120 120 120
    , gray4 = rgb255 56 56 56
    }


port contentsUpdated : String -> Cmd msg


config : Config.ParserConfig
config =
    { verbatimOpts = Set.fromList []
    , annotationOpts = []
    , replacements = []
    , replacementFirstChars = Set.fromList []
    , escapable = Set.fromList [ '\\', '\'', '#', '!', '?', ':', '[', ']', '(', ')' ]
    , meaningful = Set.fromList [ '\\', '[', ']', '\n' ]
    , markers = [ "!", "?", "#", ":" ]
    }



{- }
   Config.createParserConfig
       { verbatimChars = []
       , annotationChars = []
       , replacements = []
       , verbatimBlocks = []
       }
       |> (\( _, y ) -> y)
-}
-- Model


type alias Model =
    { code : String
    , camperdown : Types.ParsedDocument
    , script : Script.Script
    , state : State
    }


type State
    = Play
        { previous : List ( List (List Script.Text), Script.Choice )
        , current : Script.Scene
        , callstack : List Script.Key
        }
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

! choices vv
? [Take the first, slightly less grassy and worn.]
  -> road more traveled
? [Take the second, grassy and perhaps wanting wear.]
  -> road less traveled

### road more traveled
It made no difference at all.

### road less traveled
It made all the difference."""

        camperdown =
            Parse.parse config contents

        script =
            Convert.convert camperdown
    in
    { code = contents
    , camperdown = camperdown
    , script = script
    , state = Edit
    }
        |> pure



-- Update


type Msg
    = Select Script.Choice
    | Code String
    | ModeEdit
    | ModePlay


plain =
    { bold = False, italic = False, strike = False, under = False }


loadScene scene state =
    case ( scene.options, scene.continuation, state.callstack ) of
        ( Nothing, _, [] ) ->
            { state
                | current = { scene | contents = scene.contents ++ [ [ Script.Styled plain "Fin." ] ] }
            }

        ( Nothing, _, key :: rest ) ->
            { state
                | current = { scene | options = Just [ { key = key, text = [ Script.Styled plain "..." ] } ] }
                , callstack = rest
            }

        ( _, Just key, _ ) ->
            { state
                | current = scene
                , callstack = key :: state.callstack
            }

        _ ->
            { state | current = scene }


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
                    let
                        nextState =
                            loadScene scene state
                    in
                    { model | state = Play { nextState | previous = nextPrevious } }
                        |> pure

                {-
                   { model | state = Play { previous = nextPrevious
                       , current = scene
                       , callstack =
                           case scene.continuation of
                               Nothing -> state.callstack
                               Just key -> key :: state.callstack } } |> pure
                -}
                Nothing ->
                    -- Error, can't (?) happen
                    model |> pure

        ( Code code, Edit ) ->
            let
                camperdown =
                    Parse.parse config code
            in
            ( { model | code = code, camperdown = camperdown, script = Convert.convert camperdown }, contentsUpdated code )

        ( ModePlay, Edit ) ->
            case model.script of
                [] ->
                    model |> pure

                scene :: _ ->
                    let
                        state =
                            { previous = [], current = scene, callstack = [] }
                    in
                    { model | state = Play <| loadScene scene state } |> pure

        ( ModeEdit, Play _ ) ->
            { model | state = Edit } |> pure

        _ ->
            model |> pure



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


pick : Bool -> Msg -> String -> String -> Element Msg
pick isActive msg img desc =
    let
        highlight =
            if isActive then
                colors.gray1

            else
                colors.gray4

        extn =
            if isActive then
                "1"

            else
                "3"
    in
    row []
        [ el [ height fill, width (px 2), Background.color highlight ] none
        , image [ height (px 50), width (px 50), centerX, Events.onClick msg ] { src = img ++ "-gray" ++ extn ++ ".png", description = desc }
        ]


view : Model -> Html Msg
view model =
    let
        isEdit =
            model.state == Edit

        {- }
           switch =
               el [ alignRight, padding 20, Font.bold ] <|
                   el [ Background.color (rgb255 200 230 255), padding 10, Events.onClick Swap, width (px 80) ] <|
                       el [ centerX ] <|
                           text swap
        -}
    in
    layout
        [ height fill, width fill, Background.color bgcolor ]
    <|
        row
            [ height fill, width fill ]
            [ column [ height fill, width (px 57), paddingXY 0 5, spacing 5, Background.color colors.gray4 ]
                [ pick isEdit ModeEdit "edit" "Edit source code"
                , pick (not isEdit) ModePlay "play" "Run script"
                ]
            , case model.state of
                Edit ->
                    row [ height fill, width fill ]
                        [ Input.multiline
                            [ width (px 500), height fill, padding 20, scrollbarY, Font.family [ Font.typeface "Operator Mono SSm", Font.typeface "Source Code Pro", Font.monospace ], Font.size 12 ]
                            { onChange = Code, text = model.code, placeholder = Nothing, label = Input.labelHidden "Wut", spellcheck = True }
                        , el [ height fill, width (px 2), Background.color colors.gray1 ] none
                        , column [ height fill, width fill, padding 20, spacing 20, scrollbarY, clipX, Background.color colors.gray1 ] <|
                            View.viewElements (Display.rewrite model.camperdown.prelude)
                                :: List.map View.viewPassage model.camperdown.sections
                        ]

                Play { previous, current } ->
                    Element.column
                        [ width (px 700), height fill, padding 30, spacing 20, Background.color columncolor, centerX ]
                        ((List.map viewPrevious previous |> List.concat)
                            ++ viewCurrentScene current
                        )
            ]



{- }        , el [ height fill, width (px 600), Background.color (rgb255 100 100 100)] none
   , el [ height fill, width (px 2), Background.color (rgb255 300 300 300) ] none
   , el [ height fill, width fill, Background.color (rgb255 100 100 100)] none ]
-}
{- }
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

-}


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
        ++ (case current.options of
                Nothing ->
                    []

                Just options ->
                    List.map
                        (\option ->
                            paragraph
                                (Border.color activeButtonColor
                                    :: Font.color activeButtonColor
                                    :: Events.onClick (Select option)
                                    :: commonButton
                                )
                                (List.map viewText option.text)
                        )
                        options
           )


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

        Script.Problem str ->
            el [] (el [ height (px 10), width (px 10), Background.color (rgb255 255 180 190) ] (text str))


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
