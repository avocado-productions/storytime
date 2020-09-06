module Convert exposing (..)

import Config
import Loc
import ScriptTypes as Script
import Set
import Tuple
import Types


nextifyPassage : Maybe Types.Label -> Types.ParsedPassage -> List Types.FlatPassage
nextifyPassage next { level, contents, label } =
    let
        ( newContents, newPassages ) =
            nextify next contents
    in
    { level = level, contents = newContents, label = label }
        :: newPassages


nextify : Maybe Types.Label -> List Types.ParsedElement -> ( List Types.FlatElement, List Types.FlatPassage )
nextify next elems =
    List.map (nextifyElement next) elems
        |> List.unzip
        |> Tuple.mapSecond List.concat


nextifyElement : Maybe Types.Label -> Types.ParsedElement -> ( Types.FlatElement, List Types.FlatPassage )
nextifyElement next elem =
    case elem of
        Types.Paragraph text ->
            ( Types.Paragraph text, [] )

        Types.Preformatted block ->
            ( Types.Preformatted block, [] )

        Types.Item { line, indent, markLoc, children } ->
            let
                ( newChildren, passages ) =
                    nextify next children
            in
            ( Types.Item { line = line, indent = indent, markLoc = markLoc, children = newChildren }
            , passages
            )

        Types.Command { mark, command, child, line, indent } ->
            let
                base =
                    { command = command, line = line, indent = indent, mark = mark, child = Types.None }
            in
            case child of
                Types.None ->
                    ( Types.Command { base | child = Types.None }, [] )

                Types.Divert (Types.Reference ( loc, "next" )) ->
                    case next of
                        Nothing ->
                            ( Types.Command { base | child = Types.Divert <| Types.Named ( loc, "next" ) }, [] )

                        Just ref ->
                            ( Types.Command { base | child = Types.Divert ref }, [] )

                Types.Divert (Types.Reference ref) ->
                    ( Types.Command { base | child = Types.Divert <| Types.Named ref }, [] )

                Types.Divert (Types.Immediate children) ->
                    let
                        ( newChildren, newPassages ) =
                            nextify next children

                        label =
                            Types.Anonymous (Loc.location mark).start.line
                    in
                    ( Types.Command { base | child = Types.Divert label }
                    , { level = 1
                      , contents = newChildren
                      , label = label
                      }
                        :: newPassages
                    )

                Types.Divert (Types.Nested children) ->
                    let
                        ( newChildren, newPassages ) =
                            nextify next children

                        label =
                            Types.Anonymous (Loc.location mark).start.line
                    in
                    ( Types.Command { base | child = Types.Divert label }
                    , { level = 1
                      , contents = newChildren
                      , label = label
                      }
                        :: newPassages
                    )

        Types.Problem problem ->
            ( Types.Problem problem, [] )


convert : Types.ParsedDocument -> Script.Script
convert { prelude, sections } =
    let
        ( firstNext, restRawScenes ) =
            List.foldr
                (\section ( next, accum ) ->
                    ( Just section.label, nextifyPassage next section ++ accum )
                )
                ( Nothing, [] )
                sections

        firstLabel =
            Types.Anonymous 0

        firstRawScene =
            nextifyPassage firstNext { level = 1, label = firstLabel, contents = prelude }

        firstKey =
            convertLabel firstLabel
    in
    case scriptDFS (firstRawScene ++ restRawScenes) (Set.fromList [ firstKey ]) [ firstKey ] [] of
        Err msg ->
            [ { key = "", contents = [ [ Script.Problem msg ] ], options = Nothing, continuation = Nothing } ]

        Ok script ->
            script


scriptDFS script known frontier accum =
    case frontier of
        [] ->
            Ok (List.reverse accum)

        key :: rest ->
            scriptDFSStep script known rest key accum


scriptDFSStep script known frontier key accum =
    case lookup key script of
        Err msg ->
            Err msg

        Ok rawScene ->
            let
                ( scene, options ) =
                    convertElements script rawScene.contents []

                neighbors =
                    optionsToNeighbors options
                        |> List.filter
                            (\neighbor -> not (Set.member neighbor known))

                newKnown =
                    List.foldr
                        (\neighbor set -> Set.insert neighbor set)
                        known
                        neighbors
            in
            scriptDFS script newKnown (neighbors ++ frontier) <|
                ({ key = key
                 , contents = scene
                 , options = Maybe.map Tuple.first options
                 , continuation = options |> Maybe.andThen Tuple.second |> Maybe.map convertLabel
                 }
                    :: accum
                )


optionsToNeighbors : Maybe ( List Script.Choice, Maybe Types.Label ) -> List Script.Key
optionsToNeighbors options =
    case options of
        Nothing ->
            []

        Just ( choices, Nothing ) ->
            List.map .key choices

        Just ( choices, Just label ) ->
            List.map .key choices ++ [ convertLabel label ]


convertElements : List Types.FlatPassage -> List Types.FlatElement -> List (List Script.Text) -> ( List (List Script.Text), Maybe ( List Script.Choice, Maybe Types.Label ) )
convertElements script elems accum =
    case elems of
        [] ->
            ( List.reverse accum, Nothing )

        (Types.Paragraph markup) :: rest ->
            convertElements script rest (convertMarkup plain markup :: accum)

        [ Types.Command { mark, command, child } ] ->
            if Loc.value mark /= Types.Bang then
                ( List.reverse ([ Script.Problem "Only `!choices` or `!continue` commands allowed in text." ] :: accum), Nothing )

            else
                case ( command, child ) of
                    ( ( Just ( _, "continue" ), ( [], [] ) ), Types.Divert label ) ->
                        ( List.reverse accum, Just ( [ { key = convertLabel label, text = [ Script.Styled plain "..." ] } ], Nothing ) )

                    ( ( Just ( _, "choices" ), ( [], [] ) ), Types.Divert label ) ->
                        case convertChoices script label of
                            Err str ->
                                ( List.reverse ([ Script.Problem str ] :: accum), Nothing )

                            Ok choices ->
                                ( List.reverse accum, Just choices )

                    _ ->
                        ( List.reverse ([ Script.Problem "Unexpected or ill-formed command" ] :: accum), Nothing )

        _ :: rest ->
            convertElements script rest ([ Script.Problem "Everything except for the last thing in a section must just be a paragraph" ] :: accum)


convertChoices : List Types.FlatPassage -> Types.Label -> Result String ( List Script.Choice, Maybe Types.Label )
convertChoices script label =
    lookup label script
        |> Result.andThen (\{ contents } -> convertChoicesImpl script contents ( [], Nothing ))


convertChoicesImpl : List Types.FlatPassage -> List Types.FlatElement -> ( List Script.Choice, Maybe Types.Label ) -> Result String ( List Script.Choice, Maybe Types.Label )
convertChoicesImpl script options ( accumChoices, accumCont ) =
    case options of
        [] ->
            Ok ( List.reverse accumChoices, accumCont )

        (Types.Command { mark, command, child }) :: rest ->
            if Loc.value mark /= Types.Huh then
                Err "Children of a `!choices` command must all be options `?`"

            else
                case ( command, child ) of
                    ( ( Nothing, ( [ ( _, Types.Markup markup ) ], [] ) ), Types.Divert label ) ->
                        convertChoicesImpl script rest <|
                            ( { key = convertLabel label, text = convertMarkup plain markup } :: accumChoices, accumCont )

                    ( ( Just ( _, "continue" ), ( [], [] ) ), Types.Divert label ) ->
                        case accumCont of
                            Nothing ->
                                convertChoicesImpl script rest <|
                                    ( accumChoices, Just label )

                            _ ->
                                Err "Multiple continues inside of a `!choices` command."

                    _ ->
                        Err "Unexpeced subcommand inside a `!choices` command."

        (Types.Problem { problem }) :: _ ->
            Err problem

        _ ->
            Err "Unexpected element inside a `!choices` command."


plain : Script.Style
plain =
    { bold = False, italic = False, under = False, strike = False }


lookup : Types.Label -> List Types.FlatPassage -> Result String Types.FlatPassage
lookup label sections =
    case sections of
        [] ->
            case label of
                Types.Named ( loc, string ) ->
                    Err <| "No luck finding section " ++ string ++ " referenced on line " ++ String.fromInt loc.start.line

                Types.Anonymous n ->
                    Err <| "Lookup error for section on line " ++ String.fromInt n

        section :: rest ->
            if label == section.label then
                Ok section

            else
                lookup label rest


lookupKey : Script.Key -> List Types.FlatPassage -> Result String Types.FlatPassage
lookupKey key sections =
    case sections of
        [] ->
            Err <| "No luck finding " ++ key

        section :: rest ->
            if key == convertLabel section.label then
                Ok section

            else
                lookupKey key rest


convertText : Script.Style -> Types.Text -> List Script.Text
convertText style text =
    case text of
        Types.Raw str ->
            [ Script.Styled style str ]

        Types.Annotation ( _, "*" ) contents _ Nothing ->
            convertMarkup { style | bold = not style.bold } contents

        Types.Annotation ( _, "**" ) contents _ Nothing ->
            convertMarkup { style | bold = not style.bold } contents

        Types.Annotation ( _, "_" ) contents _ Nothing ->
            convertMarkup { style | italic = not style.italic } contents

        Types.Annotation ( _, "__" ) contents _ Nothing ->
            convertMarkup { style | bold = not style.bold } contents

        Types.Annotation ( _, "~" ) contents _ Nothing ->
            convertMarkup { style | strike = not style.strike } contents

        _ ->
            [ Script.Problem "Unexpected annotation" ]


convertMarkup : Script.Style -> List Types.Text -> List Script.Text
convertMarkup style markup =
    List.map (convertText style) markup |> List.concat


convertLabel : Types.Label -> String
convertLabel label =
    case label of
        Types.Anonymous n ->
            "anon_" ++ String.fromInt n

        Types.Named ( loc, str ) ->
            "named_" ++ String.fromInt loc.start.line ++ "_" ++ str
