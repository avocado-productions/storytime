module Convert exposing (..)

import Camperdown as Camp
import List.Extra as List
import Loc exposing (Loc)
import ScriptTypes as Script
import Set exposing (Set)
import Traversal
import Tuple


type alias Divert =
    Camp.Divert (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias NestedDocument =
    Camp.Document Divert (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias NestedSection =
    Camp.Section Divert (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias NestedElement =
    Camp.Element Divert (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias FlatDocument =
    Camp.Document Camp.Label (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias FlatSection =
    Camp.Section Camp.Label (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias FlatElement =
    Camp.Element Camp.Label (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias Parameter =
    Camp.Parameter Char ( String, Maybe String, Maybe String ) String


type alias Text =
    Camp.Text Char ( String, Maybe String, Maybe String ) String


type alias Value =
    Camp.Value Char ( String, Maybe String, Maybe String ) String


rearrangeLastElement accum elem =
    case elem of
        Camp.Command { lines, command, child } ->
            case ( Tuple.first command, child ) of
                ( Camp.Bang "choice", Just (Camp.Nested elems) ) ->
                    [ Camp.Command { lines = lines, command = command, child = Just (Camp.Nested (rearrangeChoice accum elems)) } ]

                ( Camp.Bang "choice", Just (Camp.Immediate elems) ) ->
                    [ Camp.Command { lines = lines, command = command, child = Just (Camp.Immediate (rearrangeChoice accum elems)) } ]

                ( Camp.Bang "cond", Just (Camp.Nested elems) ) ->
                    [ Camp.Command { lines = lines, command = command, child = Just (Camp.Nested (rearrangeChoice accum elems)) } ]

                ( Camp.Bang "cond", Just (Camp.Immediate elems) ) ->
                    [ Camp.Command { lines = lines, command = command, child = Just (Camp.Immediate (rearrangeChoice accum elems)) } ]

                ( Camp.Bang "continue", _ ) ->
                    [ Camp.Command { lines = lines, command = command, child = rearrangeChild accum child } ]

                ( Camp.Bang "pause", _ ) ->
                    [ Camp.Command { lines = lines, command = command, child = rearrangeChild accum child } ]

                _ ->
                    elem :: accum

        _ ->
            elem :: accum


rearrangeChild accum child =
    case child of
        Nothing ->
            Just (Camp.Nested accum)

        Just (Camp.Nested elems) ->
            Just (Camp.Nested (rearrangePassage accum elems))

        Just (Camp.Immediate elems) ->
            Just (Camp.Immediate (rearrangePassage accum elems))

        _ ->
            child


rearrangePassage accum elems =
    case List.unconsLast elems of
        Nothing ->
            accum

        Just ( last, rest ) ->
            rest ++ rearrangeLastElement accum last


rearrangeChoice accum elems =
    let
        spanner elem =
            case elem of
                Camp.Command { command } ->
                    case Tuple.first command of
                        Camp.Huh _ ->
                            True

                        _ ->
                            False

                _ ->
                    False

        ( options, content ) =
            List.span spanner elems

        newAccum =
            rearrangePassage accum content
    in
    List.map
        (\option ->
            case option of
                Camp.Command { lines, command, child } ->
                    Camp.Command { lines = lines, command = command, child = rearrangeChild newAccum child }

                _ ->
                    -- impossible
                    option
        )
        options


nextifySection : Maybe Camp.Label -> NestedSection -> List FlatSection
nextifySection next { level, contents, label } =
    let
        ( newPassages, newContents ) =
            nextify next contents
    in
    { level = level, contents = newContents, label = label }
        :: newPassages


nextify : Maybe Camp.Label -> List NestedElement -> ( List FlatSection, List FlatElement )
nextify next elems =
    List.map (nextifyElement next >> flattenElement) elems
        |> List.unzip
        |> Tuple.mapFirst List.concat


type IntermediateDivert
    = Pointy Camp.Label
    | Nested (List (Camp.Element IntermediateDivert (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String))
    | Immediate (List (Camp.Element IntermediateDivert (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String))


nextifyElement next =
    Traversal.mapWithProblems
        (\_ _ child ->
            case child of
                Camp.Pointy ( loc, "next" ) ->
                    case next of
                        Nothing ->
                            Err ( loc, "You can't use the `next` label in the last section." )

                        Just ref ->
                            Ok <| Pointy ref

                Camp.Pointy label ->
                    Ok <| Pointy (Camp.Named label)

                Camp.Nested contents ->
                    Ok <| Nested (List.map (nextifyElement next) contents)

                Camp.Immediate contents ->
                    Ok <| Immediate (List.map (nextifyElement next) contents)
        )


flattenElement =
    Traversal.mapAccum
        (\lines _ child accum ->
            case child of
                Pointy ref ->
                    ( [], ref )

                Nested children ->
                    let
                        ( newAccum, newChildren ) =
                            List.map flattenElement children
                                |> List.unzip
                                |> Tuple.mapFirst List.concat

                        label =
                            Camp.Anonymous lines.start

                        new =
                            { level = 2, contents = newChildren, label = label }
                    in
                    ( new :: newAccum ++ accum, label )

                Immediate children ->
                    let
                        ( newAccum, newChildren ) =
                            List.map flattenElement children
                                |> List.unzip
                                |> Tuple.mapFirst List.concat

                        label =
                            Camp.Anonymous lines.start

                        new =
                            { level = 2, contents = newChildren, label = label }
                    in
                    ( new :: newAccum ++ accum, label )
        )
        []


convert : NestedDocument -> Script.Script
convert document =
    let
        prelude =
            rearrangePassage [] document.prelude

        sections =
            List.map
                (\{ level, label, contents } ->
                    { level = level, label = label, contents = rearrangePassage [] contents }
                )
                document.sections

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
            [ { key = "", set = [], contents = Script.Paragraph [ Script.Problem msg ] Script.Fin } ]

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
                            convertElements script rawScene.contents

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

        Script.Conditional choices ->
            List.concat (List.map (Tuple.second >> getNeighbors) choices)

        Script.Choices options ->
            List.map .key options

        Script.Set _ rest ->
            getNeighbors rest

        Script.Unset _ rest ->
            getNeighbors rest

        Script.Toggle _ rest ->
            getNeighbors rest

        Script.Fin ->
            []


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


convertElements : List FlatSection -> List FlatElement -> Script.Template
convertElements script elems =
    case elems of
        [] ->
            Script.Fin

        (Camp.Paragraph markup) :: rest ->
            Script.Paragraph (convertMarkup plain markup) (convertElements script rest)

        (Camp.Command { command, child }) :: rest ->
            case ( command, child ) of
                ( ( Camp.Bang "set", ( [ ( _, Camp.Variable var ) ], [] ) ), Nothing ) ->
                    Script.Set var (convertElements script rest)

                ( ( Camp.Bang "unset", ( [ ( _, Camp.Variable var ) ], [] ) ), Nothing ) ->
                    Script.Unset var (convertElements script rest)

                ( ( Camp.Bang "toggle", ( [ ( _, Camp.Variable var ) ], [] ) ), Nothing ) ->
                    Script.Toggle var (convertElements script rest)

                ( ( Camp.Bang "cond", ( [], [] ) ), Just label ) ->
                    case convertConds script label of
                        Ok conditionals ->
                            flagIfNotLast "cond" rest <| Script.Conditional conditionals

                        Err msg ->
                            flagIfNotLast "cond" rest <| Script.Paragraph [ Script.Problem msg ] Script.Fin

                ( ( Camp.Bang "pause", ( args, parameters ) ), Just label ) ->
                    case convertChoice args parameters label of
                        Ok choice ->
                            flagIfNotLast "pause" rest <| Script.Choices [ choice ]

                        Err str ->
                            flagIfNotLast "pause" rest <| Script.Paragraph [ Script.Problem str ] Script.Fin

                ( ( Camp.Bang "continue", ( [], [] ) ), Just label ) ->
                    case lookup label script of
                        Ok more ->
                            flagIfNotLast "continue" rest <| convertElements script more.contents

                        Err str ->
                            flagIfNotLast "continue" rest <| Script.Paragraph [ Script.Problem str ] Script.Fin

                ( ( Camp.Bang "choice", ( [], [] ) ), Just label ) ->
                    case convertChoices script label of
                        Ok choices ->
                            flagIfNotLast "choice" rest <| Script.Choices choices

                        Err str ->
                            flagIfNotLast "choice" rest <| Script.Paragraph [ Script.Problem str ] Script.Fin

                ( ( Camp.Bang name, _ ), _ ) ->
                    Script.Paragraph [ Script.Problem <| "Unexpected or ill-formed command !" ++ name ] Script.Fin

                ( ( Camp.Huh name, _ ), _ ) ->
                    Script.Paragraph [ Script.Problem <| "This is not a place where we expect subcommands like ?" ++ name ] Script.Fin

        (Camp.Problem { problem }) :: rest ->
            Script.Paragraph [ Script.Problem problem ] (convertElements script rest)

        _ ->
            Script.Paragraph [ Script.Problem "Unexpected element in this passage" ] Script.Fin


flagIfNotLast name rest =
    if List.length rest == 0 then
        identity

    else
        Script.Paragraph [ Script.Problem <| "A !" ++ name ++ "can't be followed by other text or commands in a passage." ]


convertChoices : List FlatSection -> Camp.Label -> Result String (List Script.Choice)
convertChoices script label =
    lookup label script
        |> Result.andThen (\{ contents } -> convertChoicesImpl script contents [])


convertChoicesImpl script options accum =
    case options of
        [] ->
            Ok <| List.reverse accum

        (Camp.Command { command, child }) :: rest ->
            case ( command, child ) of
                ( ( Camp.Huh "(none)", ( args, parameters ) ), Just label ) ->
                    convertChoice args parameters label
                        |> Result.andThen
                            (\choice ->
                                convertChoicesImpl script rest (choice :: accum)
                            )

                _ ->
                    Err "Unexpected element inside a `!choices` command."

        (Camp.Problem { problem }) :: _ ->
            Err problem

        _ ->
            Err "Unexpected element inside a `!choices` command."


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
                                    ( "vanishing", [ ( _, Camp.Variable "true" ) ] ) ->
                                        Ok { choice | vanishing = True }

                                    ( "vanishing", [ ( _, Camp.Variable "false" ) ] ) ->
                                        Ok { choice | vanishing = False }

                                    ( "isSet", [ ( _, Camp.Variable var ) ] ) ->
                                        Ok { choice | appears = ( True, var ) :: choice.appears }

                                    ( "isUnset", [ ( _, Camp.Variable var ) ] ) ->
                                        Ok { choice | appears = ( False, var ) :: choice.appears }

                                    ( param, _ ) ->
                                        Err <| "Unexpected or ill-formed parameter " ++ param
                            )
                    )
                    (Ok { key = convertLabel label, text = text, vanishing = vanishing, appears = [] })
                    (List.map Loc.value parameters)
            )


convertConds script label =
    lookup label script
        |> Result.andThen (\{ contents } -> convertCondsImpl script contents [])


convertCondsImpl script conditionals accum =
    case conditionals of
        [] ->
            Ok <| List.reverse accum

        (Camp.Command { command, child }) :: rest ->
            case ( command, child ) of
                ( ( Camp.Huh "if", ( [], parameters ) ), Just label ) ->
                    Result.map2 Tuple.pair (lookup label script) (convertPredicateParameters parameters [])
                        |> Result.andThen
                            (\( { contents }, params ) ->
                                convertCondsImpl script rest <|
                                    ( params, convertElements script contents )
                                        :: accum
                            )

                ( ( Camp.Huh "default", ( [], [] ) ), Just label ) ->
                    if List.length rest > 0 then
                        Err <| "A ?default case has to come last, but here it is followed by " ++ String.fromInt (List.length rest) ++ " more element(s)."

                    else
                        lookup label script
                            |> Result.andThen
                                (\{ contents } ->
                                    Ok <| List.reverse (( [], convertElements script contents ) :: accum)
                                )

                ( ( Camp.Huh cmd, _ ), _ ) ->
                    Err <| "Unknown or ill-formed subcommand ?" ++ cmd

                ( ( Camp.Bang cmd, _ ), _ ) ->
                    Err <| "Unexpected position for a command !" ++ cmd

        (Camp.Problem { problem }) :: _ ->
            Err problem

        _ ->
            Err "A !cond command can only contain subcommands."


plain : Script.Style
plain =
    { bold = False, italic = False, under = False, strike = False }


lookup label sections =
    let
        eq x y =
            case ( x, y ) of
                ( Camp.Named ( _, a ), Camp.Named ( _, b ) ) ->
                    a == b

                ( Camp.Anonymous a, Camp.Anonymous b ) ->
                    a == b

                _ ->
                    False
    in
    case sections of
        [] ->
            case label of
                Camp.Named ( loc, string ) ->
                    Err <| "No luck finding section " ++ string ++ " referenced on line " ++ String.fromInt loc.start.line

                Camp.Anonymous n ->
                    Err <| "Lookup error for section on line " ++ String.fromInt n

        section :: rest ->
            if eq label section.label then
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

                ( ( "...", _, _ ), _ ) ->
                    [ Script.Styled style "…" ]

                ( ( "--", _, _ ), _ ) ->
                    [ Script.Styled style "–" ]

                ( ( "---", _, _ ), _ ) ->
                    [ Script.Styled style "—" ]

                ( ( "[", _, Just "if" ), ( [], params ) ) ->
                    case convertPredicateParameters params [] of
                        Ok cond ->
                            [ Script.InlineConditional
                                { markup = convertMarkup style (Loc.value markup)
                                , appears = cond
                                }
                            ]

                        Err msg ->
                            [ Script.Problem msg ]

                _ ->
                    [ Script.Problem "Unexpected annotation" ]

        _ ->
            [ Script.Problem "Unexpected annotation" ]


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
