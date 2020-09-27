module Convert exposing (..)

import Camperdown as Camp
import Loc exposing (Loc)
import ScriptTypes as Script
import Set exposing (Set)
import Tuple


type alias Divert =
    Camp.Divert String String Char ( String, String, Maybe String ) String


type alias NestedDocument =
    Camp.Document Divert String String Char ( String, String, Maybe String ) String


type alias NestedSection =
    Camp.Section Divert String String Char ( String, String, Maybe String ) String


type alias NestedElement =
    Camp.Element Divert String String Char ( String, String, Maybe String ) String


type alias FlatDocument =
    Camp.Document Camp.Label String String Char ( String, String, Maybe String ) String


type alias FlatSection =
    Camp.Section Camp.Label String String Char ( String, String, Maybe String ) String


type alias FlatElement =
    Camp.Element Camp.Label String String Char ( String, String, Maybe String ) String


type alias Parameter =
    Camp.Parameter Char ( String, String, Maybe String ) String


type alias Text =
    Camp.Text Char ( String, String, Maybe String ) String


type alias Value =
    Camp.Value Char ( String, String, Maybe String ) String


nextifySection : Maybe Camp.Label -> NestedSection -> List FlatSection
nextifySection next { level, contents, label } =
    let
        ( newContents, newPassages ) =
            nextify next contents
    in
    { level = level, contents = newContents, label = label }
        :: newPassages


nextify : Maybe Camp.Label -> List NestedElement -> ( List FlatElement, List FlatSection )
nextify next elems =
    List.map (nextifyElement next) elems
        |> List.unzip
        |> Tuple.mapSecond List.concat

nextifyChild lines next child =
    case child of
        Nothing ->
            ( Ok Nothing, [] )

        Just (Camp.Pointy ( loc, "next" )) ->
            case next of
                Nothing ->
                    ( Err <|
                        Camp.Problem
                            { lines = lines
                            , loc = loc
                            , problem = "You can't use the `next` label in the last step of a block."
                            }
                    , []
                    )

                Just ref ->
                    ( Ok <| Just ref , [] )

        Just (Camp.Pointy ref) ->
            ( Ok <| Just (Camp.Named ref), [] )

        Just (Camp.Immediate children) ->
            let
                ( newChildren, newPassages ) =
                    nextify next children

                label =
                    Camp.Anonymous lines.start
            in
            ( Ok <| Just label
            , { level = 2
              , contents = newChildren
              , label = label
              }
                :: newPassages
            )

        Just (Camp.Nested children) ->
            let
                ( newChildren, newPassages ) =
                    nextify next children

                label =
                    Camp.Anonymous lines.start
            in
            ( Ok <| Just label
            , { level = 2
              , contents = newChildren
              , label = label
              }
                :: newPassages
            )


nextifyElement : Maybe Camp.Label -> NestedElement -> ( FlatElement, List FlatSection )
nextifyElement next elem =
    case elem of
        Camp.Paragraph text ->
            ( Camp.Paragraph text, [] )

        Camp.Preformatted block ->
            ( Camp.Preformatted block, [] )

        Camp.Item { lines, children } ->
            let
                ( newChildren, passages ) =
                    nextify next children
            in
            ( Camp.Item { lines = lines, children = newChildren }
            , passages
            )

        Camp.Subcommand { lines, subcommand, child } ->
            let
                ( result, passages ) =
                    nextifyChild lines next child
            in
            case result of
                Err msg ->
                    ( msg, passages )

                Ok newChild ->
                    ( Camp.Subcommand { lines = lines, subcommand = subcommand, child = newChild }, passages )

        Camp.Command { lines, command, child } ->
            let
                ( result, passages ) =
                    nextifyChild lines next child
            in
            case result of
                Err msg ->
                    ( msg, passages )

                Ok newChild ->
                    ( Camp.Command { lines = lines, command = command, child = newChild }, passages )

        Camp.Problem problem ->
            ( Camp.Problem problem, [] )


convert : NestedDocument -> Script.Script
convert { prelude, sections } =
    let
        ( firstNext, restRawScenes ) =
            List.foldr
                (\section ( next, accum ) ->
                    ( Just section.label, nextifySection next section ++ accum )
                )
                ( Nothing, [] )
                sections

        firstLabel =
            Camp.Anonymous 0

        firstRawScene =
            nextifySection firstNext { level = 1, label = firstLabel, contents = prelude }

        firstKey =
            convertLabel firstLabel
    in
    case scriptDFS (firstRawScene ++ restRawScenes) (Set.fromList [ firstKey ]) [ firstKey ] [] of
        Err msg ->
            [ { key = "", set = [], contents = Script.Paragraph [ Script.Problem msg ] Script.Return } ]

        Ok script ->
            script


scriptDFS : List FlatSection -> Set String -> List String -> List Script.Scene -> Result String (List Script.Scene)
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


convertChoice : List (Loc Value) -> List (Loc Parameter) -> Camp.Label -> Result String Script.Choice
convertChoice args parameters label =
    (case args of
        [] ->
            Ok <| ( [ Script.Styled plain "…" ], True )

        [ ( _, Camp.Markup markup ) ] ->
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
                                    ( "break", [] ) ->
                                        Ok { choice | break = True }

                                    ( "vanishing", [ ( _, Camp.Variable "true" ) ] ) ->
                                        Ok { choice | vanishing = True }

                                    ( "vanishing", [ ( _, Camp.Variable "false" ) ] ) ->
                                        Ok { choice | vanishing = False }

                                    ( param, _ ) ->
                                        Err <| "Unexpected or ill-formed parameter " ++ param
                            )
                    )
                    (Ok { key = convertLabel label, text = text, vanishing = vanishing, break = False })
                    (List.map Loc.value parameters)
            )


convertPredicateParameters : List (Loc Parameter) -> Script.Predicate -> Result String Script.Predicate
convertPredicateParameters parameters accum =
    case parameters of
        [] ->
            Ok <| List.reverse accum

        ( _, ( "isSet", [ ( _, Camp.Variable var ) ] ) ) :: rest ->
            convertPredicateParameters rest (( True, var ) :: accum)

        ( _, ( "isUnset", [ ( _, Camp.Variable var ) ] ) ) :: rest ->
            convertPredicateParameters rest (( False, var ) :: accum)

        ( _, ( key, _ ) ) :: _ ->
            Err <| "Unknown or ill-formed parameter " ++ key


convertElements : Bool -> List FlatSection -> List FlatElement -> Script.Template
convertElements choicesAreAllowed script elems =
    case elems of
        [] ->
            Script.Return

        (Camp.Paragraph markup) :: rest ->
            Script.Paragraph (convertMarkup plain markup) (convertElements choicesAreAllowed script rest)

        (Camp.Command { command, child }) :: rest ->
            case ( command, child ) of
                ( ( "set", ( [ ( _, Camp.Variable var ) ], [] ) ), Nothing ) ->
                    Script.Set var (convertElements choicesAreAllowed script rest)

                ( ( "unset", ( [ ( _, Camp.Variable var ) ], [] ) ), Nothing ) ->
                    Script.Unset var (convertElements choicesAreAllowed script rest)

                ( ( "toggle", ( [ ( _, Camp.Variable var ) ], [] ) ), Nothing ) ->
                    Script.Toggle var (convertElements choicesAreAllowed script rest)

                ( ( "if", ( [], parameters ) ), Just label ) ->
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

                ( ( "cond", ( [], [] ) ), Just label ) ->
                    case convertConds (List.length rest == 0) script label of
                        Err msg ->
                            Script.Paragraph [ Script.Problem msg ] (convertElements choicesAreAllowed script rest)

                        Ok conditionals ->
                            if List.length rest == 0 then
                                Script.Conditional conditionals Nothing

                            else
                                Script.Conditional conditionals (Just <| convertElements choicesAreAllowed script rest)

                ( ( "continue", ( args, parameters ) ), Just label ) ->
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

                ( ( "choices", ( [], [] ) ), Just label ) ->
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

                ( ( name, _ ), _ ) ->
                    Script.Paragraph [ Script.Problem <| "Unexpected or ill-formed command" ++ name ] Script.Return

        (Camp.Problem { problem }) :: rest ->
            Script.Paragraph [ Script.Problem problem ] (convertElements choicesAreAllowed script rest)

        _ ->
            Script.Paragraph [ Script.Problem "Unexpected element in this passage" ] Script.Return


convertConds choicesAreAllowed script label =
    lookup label script
        |> Result.andThen (\{ contents } -> convertCondsImpl choicesAreAllowed script contents [])


convertChoices : List FlatSection -> Camp.Label -> Result String { options : List Script.Choice, continuation : Maybe Script.Choice }
convertChoices script label =
    lookup label script
        |> Result.andThen (\{ contents } -> convertChoicesImpl script contents { options = [], continuation = Nothing })


convertCondsImpl choicesAreAllowed script conditionals accum =
    case conditionals of
        [] ->
            Ok <| List.reverse accum

        (Camp.Command { command, child }) :: rest ->
            case ( command, child ) of
                ( ( "if", ( [], parameters ) ), Just label ) ->
                    Result.map2 Tuple.pair (lookup label script) (convertPredicateParameters parameters [])
                        |> Result.andThen
                            (\( { contents }, params ) ->
                                convertCondsImpl choicesAreAllowed script rest <|
                                    ( params, convertElements choicesAreAllowed script contents )
                                        :: accum
                            )

                ( ( "default", ( [], [] ) ), Just label ) ->
                    if List.length rest > 0 then
                        Err <| "A ?default case has to come last, but here it is followed by " ++ String.fromInt (List.length rest) ++ " more element(s)."

                    else
                        lookup label script
                            |> Result.andThen
                                (\{ contents } ->
                                    Ok <| List.reverse (( [], convertElements choicesAreAllowed script contents ) :: accum)
                                )

                ( ( cmd, _ ), _ ) ->
                    Err <| "Unknown or ill-formed subcommand " ++ cmd

        (Camp.Problem { problem }) :: _ ->
            Err problem

        _ ->
            Err "A !cond command can only contain subcommands."



-- convertChoicesImpl : List Camp.FlatPassage -> List Camp.FlatElement -> { options : List Script.Choice, continuation : Maybe Script.Choice } -> Result String { options : List Script.Choice, continuation : Maybe Script.Choice }


convertChoicesImpl script options accum =
    case options of
        [] ->
            Ok { accum | options = List.reverse accum.options }

        (Camp.Subcommand { subcommand, child }) :: rest ->
            case ( subcommand, child ) of
                ( ( "(none)", ( args, parameters ) ), Just label ) ->
                    convertChoice args parameters label
                        |> Result.andThen
                            (\choice ->
                                convertChoicesImpl script rest <|
                                    { accum | options = choice :: accum.options }
                            )

                ( ( "continue", ( args, parameters ) ), Just label ) ->
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

        (Camp.Problem { problem }) :: _ ->
            Err problem

        _ ->
            Err "Unexpected element inside a `!choices` command."


plain : Script.Style
plain =
    { bold = False, italic = False, under = False, strike = False }



-- lookup : Camp.Label -> List Camp.FlatPassage -> Result String Camp.FlatPassage


lookup label sections =
    case sections of
        [] ->
            case label of
                Camp.Named ( loc, string ) ->
                    Err <| "No luck finding section " ++ string ++ " referenced on line " ++ String.fromInt loc.start.line

                Camp.Anonymous n ->
                    Err <| "Lookup error for section on line " ++ String.fromInt n

        section :: rest ->
            if label == section.label then
                Ok section

            else
                lookup label rest


lookupKey : Script.Key -> List FlatSection -> Result String FlatSection
lookupKey key sections =
    case sections of
        [] ->
            Err <| "No luck finding " ++ key

        section :: rest ->
            if key == convertLabel section.label then
                Ok section

            else
                lookupKey key rest


convertText : Script.Style -> Text -> List Script.Text
convertText style text =
    case text of
        Camp.Raw str ->
            [ Script.Styled style str ]

        Camp.Annotation { markup, annotation } ->
            case Loc.value annotation of
                ( ( "*", _, _ ), _ ) ->
                    convertMarkup { style | bold = not style.bold } (Loc.value markup)

                ( ( "**", _, _ ), _ ) ->
                    convertMarkup { style | bold = not style.bold } (Loc.value markup)

                ( ( "_", _, _ ), _ ) ->
                    convertMarkup { style | italic = not style.italic } (Loc.value markup)

                ( ( "__", _, _ ), _ ) ->
                    convertMarkup { style | bold = not style.bold } (Loc.value markup)

                ( ( "~", _, _ ), _ ) ->
                    convertMarkup { style | bold = not style.strike } (Loc.value markup)

                ( ( "\"", _, _ ), _ ) ->
                    Script.Styled style "“"
                        :: convertMarkup style (Loc.value markup)
                        ++ [ Script.Styled style "”" ]

                ( ( "[", "]", Just var ), ( [], params ) ) ->
                    case convertInlineConditional style params { var = var, ifSet = Nothing, ifUnset = Nothing } of
                        Ok cond ->
                            [ Script.InlineConditional cond ]

                        Err msg ->
                            [ Script.Problem msg ]

                _ ->
                    [ Script.Problem "Unexpected annotation" ]

        _ ->
            [ Script.Problem "Unexpected annotation" ]


convertInlineConditional style params accum =
    case params of
        [] ->
            Ok accum

        ( _, ( "isSet", [ ( _, Camp.Markup markup ) ] ) ) :: rest ->
            convertInlineConditional style rest { accum | ifSet = Just <| convertMarkup style markup }

        ( _, ( "isUnset", [ ( _, Camp.Markup markup ) ] ) ) :: rest ->
            convertInlineConditional style rest { accum | ifUnset = Just <| convertMarkup style markup }

        ( _, ( param, _ ) ) :: _ ->
            Err ("Unknown parameter " ++ param)


convertMarkup : Script.Style -> List Text -> List Script.Text
convertMarkup style markup =
    List.map (convertText style) markup |> List.concat


convertLabel : Camp.Label -> String
convertLabel label =
    case label of
        Camp.Anonymous n ->
            "anon_" ++ String.fromInt n

        Camp.Named ( loc, str ) ->
            "named_" ++ str
