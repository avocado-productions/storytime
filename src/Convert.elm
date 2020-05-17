module Convert exposing (..)

import Config
import ScriptTypes as Script
import Types


nextifyPassage : Maybe Types.Label -> Types.Passage Types.Parsed -> List (Types.Passage Types.Label)
nextifyPassage next { level, contents, label } =
    let
        new =
            List.map (nextify next) contents

        newContents =
            List.map (\( x, _ ) -> x) new

        newPassages =
            List.map (\( _, x ) -> x) new |> List.concat
    in
    { level = level, contents = newContents, label = label } :: newPassages


nextify : Maybe Types.Label -> Types.ParsedElement -> ( Types.FlatElement, List Types.FlatPassage )
nextify next elem =
    case elem of
        Types.Paragraph text ->
            ( Types.Paragraph text, [] )

        Types.Preformatted block ->
            ( Types.Preformatted block, [] )

        Types.Problem problem ->
            ( Types.Problem problem, [] )

        Types.OldProblem problem ->
            ( Types.OldProblem problem, [] )

        Types.Command block ->
            let
                ( ( cmdloc, _ ), _ ) =
                    block.command

                base =
                    { command = block.command, line = block.line, indent = block.indent, text = block.text, child = Types.None }
            in
            case block.child of
                Types.None ->
                    ( Types.Command { base | child = Types.None }, [] )

                Types.Options opts ->
                    ( Types.Problem { contents = block.text, indent = block.indent, line = block.line, loc = cmdloc, problem = "No support for options" }, [] )

                Types.Divert (Types.Reference ( loc, "next" )) ->
                    case next of
                        Nothing ->
                            ( Types.Command { base | child = Types.Divert <| Types.Named ( loc, "next" ) }, [] )

                        Just ref ->
                            ( Types.Command { base | child = Types.Divert ref }, [] )

                Types.Divert (Types.Reference ref) ->
                    ( Types.Command { base | child = Types.Divert <| Types.Named ref }, [] )

                Types.Divert (Types.Immediate child) ->
                    ( Types.Problem { contents = block.text, indent = block.indent, line = block.line, loc = cmdloc, problem = "No support for >>>" }, [] )

                Types.Divert (Types.Nested child) ->
                    ( Types.Problem { contents = block.text, indent = block.indent, line = block.line, loc = cmdloc, problem = "No support for vvv" }, [] )


convert : ( List Types.ParsedElement, List Types.ParsedPassage ) -> Script.Script
convert ( rawPrelude, rawPassages ) =
    let
        ( firstNext, restOfPassages ) =
            List.foldr
                (\rawPassage ( next, ps ) ->
                    let
                        newPassages =
                            nextifyPassage next rawPassage
                    in
                    ( Just rawPassage.label, newPassages :: ps )
                )
                ( Nothing, [] )
                rawPassages

        firstPassages =
            nextifyPassage firstNext { level = 1, label = Types.Anonymous 0, contents = rawPrelude }

        rawScript =
            List.concat <| (firstPassages :: restOfPassages)

        script =
            List.map
                (\{ label, contents } -> List.foldr convertElement { key = convertLabel label, contents = [], options = [] } contents)
                rawScript
    in
    script


convertElement : Types.Element Types.Label -> Script.Scene -> Script.Scene
convertElement elem scene =
    case elem of
        Types.Paragraph contents ->
            let
                text =
                    List.map (convertText { bold = False, italic = False, under = False, strike = False }) contents |> List.concat
            in
            { scene | contents = text :: scene.contents }

        Types.Command block ->
            { scene | contents = scene.contents }

        {- }
           Types.Command ( ( _, "choice" ), ( [ ( _, Types.Markup contents ) ], [] ) ) (Types.Continuation ref) ->
               let
                   text =
                       List.map (convertText { bold = False, italic = False, under = False, strike = False }) contents |> List.concat

                   new =
                       { key = convertLabel ref, text = text }
               in
               { scene | options = new :: scene.options }
        -}
        _ ->
            { scene | contents = [ Script.Problem ] :: scene.contents }


convertText : Script.Style -> Types.Text -> List Script.Text
convertText style text =
    case text of
        Types.Raw str ->
            [ Script.Styled style str ]

        Types.Annotation contents (Just ( ( _, "*" ), ( [], [] ) )) ->
            List.map (convertText { style | bold = not style.bold }) contents
                |> List.concat

        Types.Annotation contents (Just ( ( _, "~" ), ( [], [] ) )) ->
            List.map (convertText { style | strike = not style.strike }) contents
                |> List.concat

        Types.Annotation contents (Just ( ( _, "/" ), ( [], [] ) )) ->
            List.map (convertText { style | italic = not style.italic }) contents
                |> List.concat

        Types.Annotation contents (Just ( ( _, "_" ), ( [], [] ) )) ->
            List.map (convertText { style | under = not style.under }) contents
                |> List.concat

        _ ->
            [ Script.Problem ]


convertLabel : Types.Label -> String
convertLabel lab =
    case lab of
        Types.Named ( _, n ) ->
            "named_" ++ n

        Types.Anonymous i ->
            "line_" ++ String.fromInt i
