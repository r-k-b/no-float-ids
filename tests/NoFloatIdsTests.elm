module NoFloatIdsTests exposing (testRule, tests)

import NoFloatIds
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule =
    Review.Test.run NoFloatIds.rule


standardErrorUnder : String -> { message : String, details : List String, under : String }
standardErrorUnder under =
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
    , under = under
    }


tests : Test
tests =
    describe "NoFloatIds"
        [ describe "in Plain records"
            [ test "should not warn about Int ids" <|
                \() ->
                    testRule """module A exposing (..)
type alias R = {someId:Int}"""
                        |> Review.Test.expectNoErrors
            , test "should not warn about non-id Floats" <|
                \() ->
                    testRule """module A exposing (..)
type alias R = {someFoo:Float}"""
                        |> Review.Test.expectNoErrors
            , test "should warn about _Id Floats" <|
                \() ->
                    testRule """module A exposing (..)
type alias R = {someId:Float}"""
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                (standardErrorUnder "someId:Float")
                            ]
            , test "should warn about id Floats" <|
                \() ->
                    testRule """module A exposing (..)
type alias R = {id:Float}"""
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                (standardErrorUnder "id:Float")
                            ]
            , test "should highlight just the offending property" <|
                \() ->
                    testRule """module A exposing (..)
type alias R =
    { someId : Float
    , foo : Bar
    }"""
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                (standardErrorUnder "someId : Float")
                                |> Review.Test.atExactly
                                    { start = { row = 3, column = 7 }
                                    , end = { row = 3, column = 21 }
                                    }
                            ]
            ]
        , describe "in Generic records"
            [ test "should not warn about Int ids" <|
                \() ->
                    testRule """module A exposing (..)
type alias R = {a|someId:Int}"""
                        |> Review.Test.expectNoErrors
            , test "should not warn about non-id Floats" <|
                \() ->
                    testRule """module A exposing (..)
type alias R = {a|someFoo:Float}"""
                        |> Review.Test.expectNoErrors
            , test "should warn about _Id Floats" <|
                \() ->
                    testRule """module A exposing (..)
type alias R = {a|someId:Float}"""
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                (standardErrorUnder "someId:Float")
                            ]
            , test "should warn about id Floats" <|
                \() ->
                    testRule """module A exposing (..)
type alias R = {a|id:Float}"""
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                (standardErrorUnder "id:Float")
                            ]
            , test "should highlight just the offending property" <|
                \() ->
                    testRule """module A exposing (..)
type alias Foo a =
    { a
        | fooId : Float
        , bar : String
    }"""
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                (standardErrorUnder "fooId : Float")
                                |> Review.Test.atExactly
                                    { start = { row = 4, column = 11 }
                                    , end = { row = 4, column = 24 }
                                    }
                            ]
            ]
        ]
