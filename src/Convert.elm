module Convert exposing (..)

import Loc
import Parse
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


nextify : Maybe Types.Label -> Types.Element Types.Parsed -> ( Types.Element Types.Label, List (Types.Passage Types.Label) )
nextify next elem =
    case elem of
        Types.Paragraph text ->
            ( Types.Paragraph text, [] )

        Types.Preformatted config text ->
            ( Types.Preformatted config text, [] )

        Types.Problem problem ->
            ( Types.Problem problem, [] )

        Types.CommandMark command Types.None ->
            ( Types.CommandMark command Types.None, [] )

        Types.CommandMark command (Types.Options opts) ->
            Debug.todo ""

        Types.CommandMark command (Types.Continuation (Types.Reference ( _, "next" ))) ->
            case next of
                Nothing ->
                    ( Types.CommandMark command (Types.Continuation (Types.Named "next")), [] )

                Just ref ->
                    ( Types.CommandMark command (Types.Continuation ref), [] )

        Types.CommandMark command (Types.Continuation (Types.Reference ( _, ref ))) ->
            ( Types.CommandMark command (Types.Continuation (Types.Named ref)), [] )

        Types.CommandMark (( ( loc, _ ), _ ) as command) (Types.Continuation (Types.Elements elems)) ->
            let
                label =
                    Types.Anonymous loc.start.line

                children =
                    List.map (nextify next) elems

                childPassage =
                    { label = ( loc, label )
                    , contents = List.map (\( x, _ ) -> x) children
                    , level = 1
                    }

                childPassages =
                    List.map (\( _, x ) -> x) children |> List.concat
            in
            ( Types.CommandMark command (Types.Continuation label), childPassage :: childPassages )


convert : String -> Script.Script
convert source =
    let
        ( rawPrelude, rawPassages ) =
            Parse.parse source

        ( firstNext, restOfPassages ) =
            List.foldr
                (\rawPassage ( next, ps ) ->
                    let
                        newPassages =
                            nextifyPassage next rawPassage

                        ( _, label ) =
                            rawPassage.label
                    in
                    ( Just label, newPassages :: ps )
                )
                ( Nothing, [] )
                rawPassages

        firstPassages =
            nextifyPassage firstNext { level = 1, label = Loc.todoDummyLocate <| Types.Anonymous 0, contents = rawPrelude }

        rawScript =
            List.concat <| (firstPassages :: restOfPassages)

        script =
            List.map
                (\{ label, contents } -> List.foldr convertElement { key = convertLabel (Loc.value label), contents = [], options = [] } contents)
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

        Types.CommandMark ( ( _, "choice" ), ( [ ( _, Types.Markup contents ) ], [] ) ) (Types.Continuation ref) ->
            let
                text =
                    List.map (convertText { bold = False, italic = False, under = False, strike = False }) contents |> List.concat

                new =
                    { key = convertLabel ref, text = text }
            in
            { scene | options = new :: scene.options }

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
        Types.Named n ->
            "named_" ++ n

        Types.Anonymous i ->
            "line_" ++ String.fromInt i
