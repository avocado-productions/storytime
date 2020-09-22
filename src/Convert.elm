module Convert exposing (..)

import Config
import Loc exposing (Loc)
import ScriptTypes as Script
import Set exposing (Set)
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
            [ { key = "", set = [], contents = Script.Paragraph [ Script.Problem msg ] Script.Return } ]

        Ok script ->
            script


scriptDFS : List Types.FlatPassage -> Set String -> List String -> List Script.Scene -> Result String (List Script.Scene)
scriptDFS script known frontier accum =
    case frontier of
        [] ->
            Ok (List.reverse accum)

        key :: rest ->
            case lookupKey key script of
                Err msg ->
                    Err msg

                Ok rawScene ->
                    let
                        contents =
                            convertElements True script rawScene.contents

                        neighbors =
                            getNeighbors contents
                                |> List.filter (\neighbor -> not (Set.member neighbor known))

                        newKnown =
                            List.foldr
                                (\neighbor set -> Set.insert neighbor set)
                                known
                                neighbors
                    in
                    scriptDFS script newKnown (neighbors ++ rest) <|
                        ({ key = key
                         , set = []
                         , contents = contents
                         }
                            :: accum
                        )


getNeighbors : Script.Template -> List Script.Key
getNeighbors template =
    case template of
        Script.Paragraph _ rest ->
            getNeighbors rest

        Script.Conditional choices continuation ->
            List.concat (List.map (Tuple.second >> getNeighbors) choices) ++ Maybe.withDefault [] (Maybe.map getNeighbors continuation)

        Script.Choices { options, continuation } ->
            let
                conoption =
                    Maybe.map List.singleton continuation |> Maybe.withDefault []
            in
            List.map .key (conoption ++ options)

        Script.Set _ rest ->
            getNeighbors rest

        Script.Unset _ rest ->
            getNeighbors rest

        Script.Toggle _ rest ->
            getNeighbors rest

        Script.Return ->
            []


convertChoice args parameters label =
    (case args of
        [] ->
            Ok <| ( [ Script.Styled plain "…" ], True )

        [ ( _, Types.Markup markup ) ] ->
            Ok <| ( convertMarkup plain markup, False )

        _ ->
            Err "Choices can only have at most one parameter, markup text describing the label."
    )
        |> Result.andThen
            (\( text, vanishing ) ->
                List.foldr
                    (\parameter ->
                        Result.andThen
                            (\choice ->
                                case parameter of
                                    ( _, ( ( _, "break" ), [] ) ) ->
                                        Ok { choice | break = True }

                                    ( _, ( ( _, "vanishing" ), [ ( _, Types.Variable "true" ) ] ) ) ->
                                        Ok { choice | vanishing = True }

                                    ( _, ( ( _, "vanishing" ), [ ( _, Types.Variable "false" ) ] ) ) ->
                                        Ok { choice | vanishing = False }

                                    ( _, ( ( _, param ), _ ) ) ->
                                        Err <| "Unexpected or ill-formed parameter " ++ param
                            )
                    )
                    (Ok { key = convertLabel label, text = text, vanishing = vanishing, break = False })
                    parameters
            )


convertPredicateParameters : List (Loc Types.Parameter) -> Script.Predicate -> Result String Script.Predicate
convertPredicateParameters parameters accum =
    case parameters of
        [] ->
            Ok <| List.reverse accum

        ( _, ( ( _, "isSet" ), [ ( _, Types.Variable var ) ] ) ) :: rest ->
            convertPredicateParameters rest (( True, var ) :: accum)

        ( _, ( ( _, "isUnset" ), [ ( _, Types.Variable var ) ] ) ) :: rest ->
            convertPredicateParameters rest (( False, var ) :: accum)

        ( _, ( ( _, key ), _ ) ) :: _ ->
            Err <| "Unknown or ill-formed parameter " ++ key


convertElements : Bool -> List Types.FlatPassage -> List Types.FlatElement -> Script.Template
convertElements choicesAreAllowed script elems =
    case elems of
        [] ->
            Script.Return

        (Types.Paragraph markup) :: rest ->
            if a then
                Script.Paragraph (convertMarkup plain markup) (convertElements choicesAreAllowed script rest)

            else if b then
                Fopo

            else if bar then
                alsadfas

            else if baz then
                sdfa

            else
                let
                    foo =
                        0
                in
                if boo then
                    dsf

                else
                    sdf

        (Types.Command { command, child }) :: rest ->
            case ( command, child ) of
                ( ( Just ( _, "set" ), ( [ ( _, Types.Variable var ) ], [] ) ), Types.None ) ->
                    Script.Set var (convertElements choicesAreAllowed script rest)

                ( ( Just ( _, "unset" ), ( [ ( _, Types.Variable var ) ], [] ) ), Types.None ) ->
                    Script.Unset var (convertElements choicesAreAllowed script rest)

                ( ( Just ( _, "toggle" ), ( [ ( _, Types.Variable var ) ], [] ) ), Types.None ) ->
                    Script.Toggle var (convertElements choicesAreAllowed script rest)

                ( ( Just ( _, "if" ), ( [], parameters ) ), Types.Divert label ) ->
                    case Result.map2 Tuple.pair (lookup label script) (convertPredicateParameters parameters []) of
                        Err msg ->
                            Script.Paragraph [ Script.Problem msg ] (convertElements choicesAreAllowed script rest)

                        Ok ( subscript, params ) ->
                            if List.length rest == 0 then
                                Script.Conditional [ ( params, convertElements choicesAreAllowed script subscript.contents ) ]
                                    Nothing

                            else
                                Script.Conditional [ ( params, convertElements False script subscript.contents ) ]
                                    (Just <| convertElements choicesAreAllowed script rest)

                ( ( Just ( _, "cond" ), ( [], [] ) ), Types.Divert label ) ->
                    case convertConds (List.length rest == 0) script label of
                        Err msg ->
                            Script.Paragraph [ Script.Problem msg ] (convertElements choicesAreAllowed script rest)

                        Ok conditionals ->
                            if List.length rest == 0 then
                                Script.Conditional conditionals Nothing

                            else
                                Script.Conditional conditionals (Just <| convertElements choicesAreAllowed script rest)

                ( ( Just ( _, "continue" ), ( args, parameters ) ), Types.Divert label ) ->
                    let
                        anyProblem =
                            if not choicesAreAllowed then
                                \_ -> Script.Paragraph [ Script.Problem "!continue command in a non-terminal passage ignored" ] Script.Return

                            else if List.length rest == 0 then
                                identity

                            else
                                Script.Paragraph [ Script.Problem "A !continue can't be followed by other text or commands in a passage." ]
                    in
                    case convertChoice args parameters label of
                        Ok choice ->
                            anyProblem <| Script.Choices { options = [ choice ], continuation = Nothing }

                        Err str ->
                            anyProblem <| Script.Paragraph [ Script.Problem str ] Script.Return

                ( ( Just ( _, "choices" ), ( [], [] ) ), Types.Divert label ) ->
                    let
                        anyProblem =
                            if not choicesAreAllowed then
                                \_ -> Script.Paragraph [ Script.Problem "!choice command in a non-terminal passage ignored" ] Script.Return

                            else if List.length rest == 0 then
                                identity

                            else
                                Script.Paragraph [ Script.Problem "A !choice can't be followed by other text or commands in a passage." ]
                    in
                    case convertChoices script label of
                        Ok choices ->
                            anyProblem <| Script.Choices choices

                        Err str ->
                            anyProblem <| Script.Paragraph [ Script.Problem str ] Script.Return

                ( ( Just ( _, name ), _ ), _ ) ->
                    Script.Paragraph [ Script.Problem <| "Unexpected or ill-formed command" ++ name ] Script.Return

                ( ( Nothing, _ ), _ ) ->
                    Script.Paragraph [ Script.Problem <| "Unexpected or ill-formed unnamed command" ] Script.Return

        (Types.Problem { problem }) :: rest ->
            Script.Paragraph [ Script.Problem problem ] (convertElements choicesAreAllowed script rest)

        _ ->
            Script.Paragraph [ Script.Problem "Unexpected element in this passage" ] Script.Return


convertConds choicesAreAllowed script label =
    lookup label script
        |> Result.andThen (\{ contents } -> convertCondsImpl choicesAreAllowed script contents [])


convertChoices : List Types.FlatPassage -> Types.Label -> Result String { options : List Script.Choice, continuation : Maybe Script.Choice }
convertChoices script label =
    lookup label script
        |> Result.andThen (\{ contents } -> convertChoicesImpl script contents { options = [], continuation = Nothing })


convertCondsImpl choicesAreAllowed script conditionals accum =
    case conditionals of
        [] ->
            Ok <| List.reverse accum

        (Types.Command { command, child }) :: rest ->
            case ( command, child ) of
                ( ( Just ( _, "if" ), ( [], parameters ) ), Types.Divert label ) ->
                    Result.map2 Tuple.pair (lookup label script) (convertPredicateParameters parameters [])
                        |> Result.andThen
                            (\( { contents }, params ) ->
                                convertCondsImpl choicesAreAllowed script rest <|
                                    ( params, convertElements choicesAreAllowed script contents )
                                        :: accum
                            )

                ( ( Just ( _, "default" ), ( [], [] ) ), Types.Divert label ) ->
                    if List.length rest > 0 then
                        Err <| "A ?default case has to come last, but here it is followed by " ++ String.fromInt (List.length rest) ++ " more element(s)."

                    else
                        lookup label script
                            |> Result.andThen
                                (\{ contents } ->
                                    Ok <| List.reverse (( [], convertElements choicesAreAllowed script contents ) :: accum)
                                )

                ( ( Just ( _, cmd ), _ ), _ ) ->
                    Err <| "Unknown or ill-formed subcommand " ++ cmd

                ( ( Nothing, _ ), _ ) ->
                    Err <| "Cond statements cannot have unnamed subcommands"

        (Types.Problem { problem }) :: _ ->
            Err problem

        _ ->
            Err "A !cond command can only contain subcommands."


convertChoicesImpl : List Types.FlatPassage -> List Types.FlatElement -> { options : List Script.Choice, continuation : Maybe Script.Choice } -> Result String { options : List Script.Choice, continuation : Maybe Script.Choice }
convertChoicesImpl script options accum =
    case options of
        [] ->
            Ok { accum | options = List.reverse accum.options }

        (Types.Command { mark, command, child }) :: rest ->
            if Loc.value mark /= Types.Huh then
                Err "Children of a `!choices` command must all be options `?`"

            else
                case ( command, child ) of
                    ( ( Nothing, ( args, parameters ) ), Types.Divert label ) ->
                        convertChoice args parameters label
                            |> Result.andThen
                                (\choice ->
                                    convertChoicesImpl script rest <|
                                        { accum | options = choice :: accum.options }
                                )

                    ( ( Just ( _, "continue" ), ( args, parameters ) ), Types.Divert label ) ->
                        convertChoice args parameters label
                            |> Result.andThen
                                (\choice ->
                                    case accum.continuation of
                                        Nothing ->
                                            convertChoicesImpl script rest <|
                                                { accum | continuation = Just choice }

                                        _ ->
                                            Err "Multiple continues inside of a `!choices` command."
                                )

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

        Types.Annotation ( _, "\"" ) contents _ Nothing ->
            Script.Styled style "“"
                :: convertMarkup style contents
                ++ [ Script.Styled style "”" ]

        Types.Annotation ( _, "[" ) _ _ (Just ( _, ( Just ( _, var ), ( [], params ) ) )) ->
            case convertInlineConditional style params { var = var, ifSet = Nothing, ifUnset = Nothing } of
                Ok cond ->
                    [ Script.InlineConditional cond ]

                Err msg ->
                    [ Script.Problem msg ]

        _ ->
            [ Script.Problem "Unexpected annotation" ]


convertInlineConditional style params accum =
    case params of
        [] ->
            Ok accum

        ( _, ( ( _, "isSet" ), [ ( _, Types.Markup markup ) ] ) ) :: rest ->
            convertInlineConditional style rest { accum | ifSet = Just <| convertMarkup style markup }

        ( _, ( ( _, "isUnset" ), [ ( _, Types.Markup markup ) ] ) ) :: rest ->
            convertInlineConditional style rest { accum | ifUnset = Just <| convertMarkup style markup }

        ( _, ( ( _, param ), _ ) ) :: _ ->
            Err ("Unknown parameter " ++ param)


convertMarkup : Script.Style -> List Types.Text -> List Script.Text
convertMarkup style markup =
    List.map (convertText style) markup |> List.concat


convertLabel : Types.Label -> String
convertLabel label =
    case label of
        Types.Anonymous n ->
            "anon_" ++ String.fromInt n

        Types.Named ( loc, str ) ->
            "named_" ++ str
