module NoFloatIds exposing (rule)

{-| In the rare situation that generated Elm code isn't of the highest quality,
the `NoFloatIds` rule will help to enforce record properties that look like IDs
from being assigned `Float` types.


# Usage

After adding [`elm-review`](elm-review) to your project, import this rule from
your `ReviewConfig.elm` file and add it to the config. E.g.:

    import NoFloatIds
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ NoFloatIds.rule ]

[elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation
    exposing
        ( RecordDefinition
        , RecordField
        , TypeAnnotation(..)
        )
import Review.Rule as Rule exposing (Error, Rule, error)



-- Create a new rule


{-| A rule for elm-review that discourages the use of Float types for "Id"
properties of records.

For example, the rule would return an error for the first two aliases:

    type alias Foo =
        { qux : Qux
        , someId : Float
        }

    type alias Bar =
        { qux : Qux
        , id : Float
        }

But not the third:

    type alias Baz =
        { qux : Qux
        , id : Int
        }

-}
rule : Rule
rule =
    -- Define the rule with the same name as the module it is defined in
    Rule.newSchema "NoFloatIds"
        -- Make it look at declarations
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromSchema


details =
    { message =
        "Do not accept Float values for IDs from any API."
    , details =
        [ "It looks like this property will be treated as an Id, and has a type"
            ++ "of Float. Floating point numbers are not guaranteed to exactly"
            ++ " represent all integers, which is not great when the choice of"
            ++ " how to transform a Float to an Int is an open question."
            ++ " (Should it be truncated? Rounded? Something else?)"
        , "Push back on the maintainers of the API, and ask them to modify it"
            ++ " to use integers instead."
        ]
    }


declarationVisitor : Node Declaration -> List Error
declarationVisitor node =
    case Node.value node of
        FunctionDeclaration _ ->
            []

        AliasDeclaration typeAlias ->
            case typeAlias.typeAnnotation |> Node.value of
                GenericType _ ->
                    []

                Typed _ _ ->
                    []

                Unit ->
                    []

                Tupled _ ->
                    []

                Record recordDefinition ->
                    recordDefinition
                        |> List.concatMap checkForFloatIds

                GenericRecord _ recordDefinition ->
                    recordDefinition
                        |> Node.value
                        |> List.concatMap checkForFloatIds

                FunctionTypeAnnotation _ _ ->
                    []

        CustomTypeDeclaration _ ->
            []

        PortDeclaration _ ->
            []

        InfixDeclaration _ ->
            []

        Destructuring _ _ ->
            []


checkForFloatIds : Node RecordField -> List Error
checkForFloatIds recordDefinition =
    let
        ( nameNode, typeAnno ) =
            recordDefinition
                |> Node.value

        name =
            nameNode |> Node.value
    in
    if (name |> String.endsWith "Id") || name == "id" then
        case typeAnno |> Node.value of
            GenericType _ ->
                []

            Typed moduleNameAndStringNode _ ->
                if
                    moduleNameAndStringNode
                        |> Node.value
                        |> (==) ( [], "Float" )
                then
                    [ error details
                        (Node.combine Tuple.pair nameNode typeAnno
                            |> Node.range
                        )
                    ]

                else
                    []

            Unit ->
                []

            Tupled _ ->
                []

            Record _ ->
                []

            GenericRecord _ _ ->
                []

            FunctionTypeAnnotation _ _ ->
                []

    else
        []
