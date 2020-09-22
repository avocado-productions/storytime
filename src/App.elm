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
import Occurs exposing (Occurs(..))
import Parse
import ScriptTypes as Script exposing (Key)
import Set exposing (Set)
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
    , annotationOpts =
        [ { startSymbol = "**", endSymbol = "**", commandOccursAfterwards = Never }
        , { startSymbol = "*", endSymbol = "*", commandOccursAfterwards = Never }
        , { startSymbol = "__", endSymbol = "__", commandOccursAfterwards = Never }
        , { startSymbol = "_", endSymbol = "_", commandOccursAfterwards = Never }
        , { startSymbol = "~", endSymbol = "~", commandOccursAfterwards = Never }
        , { startSymbol = "~", endSymbol = "~", commandOccursAfterwards = Never }
        , { startSymbol = "\"", endSymbol = "\"", commandOccursAfterwards = Never }
        , { startSymbol = "[", endSymbol = "]", commandOccursAfterwards = Always }
        ]
    , replacements = [ ( "...", "…" ), ( "---", "—" ), ( "--", "–" ) ]
    , replacementFirstChars = Set.fromList [ '.', '-' ]
    , escapable = Set.fromList [ '\\', '\'', '#', '!', '?', ':', '[', ']', '(', ')', '*', '_', '~', '"', '.', '-' ]
    , meaningful = Set.fromList [ '\\', '[', ']', '\n', '*', '_', '~', '"', '.', '-' ]
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
    = Play ScriptState
    | Edit


type alias ScriptState =
    { previous : List ( List (List Script.Text), Maybe Script.Choice )
    , current : ( List (List Script.Text), List Script.Choice )
    , callstack : List Script.Choice
    , setVars : Set String
    }


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

## road more traveled
It made no difference at all.

## road less traveled
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


testPredicate state ( bool, var ) =
    bool == Set.member var state.setVars


loadOptions state options =
    case options of
        [] ->
            Nothing

        ( tests, template ) :: rest ->
            if List.all (testPredicate state) tests then
                Just template

            else
                loadOptions state rest


loadSubscene subscene state accum =
    case subscene of
        Script.Paragraph markup rest ->
            loadSubscene rest state (loadMarkup state markup :: accum)

        Script.Set var rest ->
            loadSubscene rest { state | setVars = Set.insert var state.setVars } accum

        Script.Unset var rest ->
            loadSubscene rest { state | setVars = Set.remove var state.setVars } accum

        Script.Toggle var rest ->
            loadSubscene rest
                { state
                    | setVars =
                        if Set.member var state.setVars then
                            Set.remove var state.setVars

                        else
                            Set.insert var state.setVars
                }
                accum

        Script.Conditional options continuation ->
            case ( continuation, loadOptions state options ) of
                ( Nothing, Nothing ) ->
                    loadSubscene Script.Return state accum

                ( Nothing, Just rest ) ->
                    loadSubscene rest state accum

                ( Just rest, Nothing ) ->
                    loadSubscene rest state accum

                ( Just rest, Just subsubscene ) ->
                    let
                        ( newState, newAccum ) =
                            loadSubscene subsubscene state accum
                    in
                    loadSubscene rest newState newAccum

        Script.Return ->
            ( state, accum )

        Script.Choices { options, continuation } ->
            ( state, [ Script.Problem "Choices where choices should not be" ] :: accum )


loadScene scene state accum =
    case scene of
        Script.Paragraph markup rest ->
            loadScene rest state (loadMarkup state markup :: accum)

        Script.Set var rest ->
            loadScene rest { state | setVars = Set.insert var state.setVars } accum

        Script.Unset var rest ->
            loadScene rest { state | setVars = Set.remove var state.setVars } accum

        Script.Toggle var rest ->
            loadScene rest
                { state
                    | setVars =
                        if Set.member var state.setVars then
                            Set.remove var state.setVars

                        else
                            Set.insert var state.setVars
                }
                accum

        Script.Conditional options continuation ->
            case ( continuation, loadOptions state options ) of
                ( Nothing, Nothing ) ->
                    loadScene Script.Return state accum

                ( Nothing, Just rest ) ->
                    loadScene rest state accum

                ( Just rest, Nothing ) ->
                    loadScene rest state accum

                ( Just rest, Just subscene ) ->
                    let
                        ( newState, newAccum ) =
                            loadSubscene subscene state accum
                    in
                    loadScene rest newState newAccum

        Script.Return ->
            case state.callstack of
                [] ->
                    { state | current = ( List.reverse accum ++ [ [ Script.Styled plain "Fin." ] ], [] ) }

                choice :: rest ->
                    { state | current = ( List.reverse accum, [ choice ] ), callstack = rest }

        Script.Choices { options, continuation } ->
            case continuation of
                Nothing ->
                    { state | current = ( List.reverse accum, options ) }

                Just return ->
                    { state | current = ( List.reverse accum, options ), callstack = return :: state.callstack }


loadMarkup state markup =
    List.map (loadText state) markup |> List.concat


loadText : ScriptState -> Script.Text -> List Script.Text
loadText state text =
    case text of
        Script.Styled _ _ ->
            [ text ]

        Script.Problem _ ->
            [ text ]

        Script.InlineConditional { var, ifSet, ifUnset } ->
            let
                child =
                    if Set.member var state.setVars then
                        ifSet

                    else
                        ifUnset
            in
            case child of
                Nothing ->
                    []

                Just markup ->
                    loadMarkup state markup


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
                    if option.vanishing then
                        state.previous ++ [ ( Tuple.first state.current, Nothing ) ]

                    else
                        state.previous ++ [ ( Tuple.first state.current, Just option ) ]
            in
            case nextScene of
                Just scene ->
                    let
                        callstack =
                            if option.break then
                                []

                            else
                                state.callstack

                        nextState =
                            loadScene scene.contents { state | callstack = callstack, previous = nextPrevious } []
                    in
                    { model | state = Play nextState } |> pure

                Nothing ->
                    -- Error, can't (?) happen
                    model |> pure

        ( Code code, Edit ) ->
            let
                camperdown =
                    Parse.parse config code
            in
            ( { model | code = code, camperdown = camperdown, script = Convert.convert camperdown }, contentsUpdated code )

        ( ModePlay, _ ) ->
            case model.script of
                [] ->
                    model |> pure

                scene :: _ ->
                    let
                        state =
                            { previous = [], current = ( [], [] ), callstack = [], setVars = Set.empty }
                    in
                    { model | state = Play <| loadScene scene.contents state [] } |> pure

        -- { model | state = Play <| loadScene scene state } |> pure
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
        ( playIcon, playAlt ) =
            if isEdit then
                ( "play", "Run script" )

            else
                ( "restart", "Start script over" )
    in
    layout
        [ height fill, width fill, Background.color bgcolor ]
    <|
        row
            [ height fill, width fill ]
            [ column [ height fill, width (px 57), paddingXY 0 5, spacing 5, Background.color colors.gray4 ]
                [ pick isEdit ModeEdit "edit" "Edit source code"
                , pick (not isEdit) ModePlay playIcon "Run script"
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


commonButton =
    [ padding 10
    , Border.width 1
    , Border.solid
    , Border.rounded 7
    ]


viewPrevious : ( List (List Script.Text), Maybe Script.Choice ) -> List (Element Msg)
viewPrevious ( contents, selected ) =
    List.map viewParagraph contents
        ++ (case selected of
                Nothing ->
                    []

                Just choice ->
                    [ paragraph
                        (Border.color inactiveButtonColor
                            :: Background.color bgcolor
                            :: Font.color inactiveButtonColor
                            :: commonButton
                        )
                        (List.map viewText choice.text)
                    ]
           )


viewCurrentScene : ( List (List Script.Text), List Script.Choice ) -> List (Element Msg)
viewCurrentScene ( story, options ) =
    List.map viewParagraph story
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
            options


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
            el [] (el [ Background.color (rgb255 255 180 190), padding 10 ] (text str))

        Script.InlineConditional f ->
            el [] (el [ Background.color (rgb255 255 180 190), padding 10 ] (text <| "Error: unresolved inline conditional" ++ Debug.toString f))


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
