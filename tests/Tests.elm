module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html as Html exposing (Html, li)
import Html.Attributes exposing (style, value)
import Main exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Validator


type alias TestCase =
    String


sampleValidatorTest : TestCase -> Int -> List SampleError -> Test
sampleValidatorTest testCase num sampleErrorLst =
    test testCase <|
        \_ ->
            let
                actual =
                    Validator.errors sampleValidator <| Just num

                expected =
                    sampleErrorLst
            in
            Expect.equal actual expected


anotherValidatorTest : TestCase -> String -> List AnotherError -> Test
anotherValidatorTest testCase url anotherErrorList =
    test testCase <|
        \_ ->
            let
                actual =
                    Validator.errors anotherValidator <|
                        Just url

                expected =
                    anotherErrorList
            in
            Expect.equal actual expected


form2formErrorsTest : TestCase -> Form -> FormErrors -> Test
form2formErrorsTest testCase form formErrors =
    test testCase <|
        \_ ->
            let
                actual =
                    form2formErrors
                        form

                expected =
                    formErrors
            in
            Expect.equal actual expected


sampleViewTest : TestCase -> Maybe Int -> List String -> String -> Test
sampleViewTest testCase sampleInputMaybe errors sampleInputText =
    let
        view =
            sampleInputView sampleInputMaybe errors
    in
    test testCase <|
        Expect.all
            [ inputTextTest view sampleInputText
            , listTest view errors
            ]


anotherViewTest : TestCase -> Maybe String -> List String -> String -> Test
anotherViewTest testCase anotherInputMaybe errors anotherInputText =
    let
        view =
            anotherInputView anotherInputMaybe errors
    in
    test testCase <|
        Expect.all
            [ inputTextTest view anotherInputText
            , listTest view errors
            ]


inputTextTest : Html Msg -> String -> (subject -> Expectation)
inputTextTest view inputText =
    \_ ->
        view
            |> Query.fromHtml
            |> Query.find [ Selector.tag "input" ]
            |> Query.has [ Selector.tag "input", Selector.attribute <| value inputText ]


listTest : Html Msg -> List String -> (subject -> Expectation)
listTest view texts =
    let
        children =
            view
                |> Query.fromHtml
                |> Query.find [ Selector.tag "ul" ]
                |> Query.children []
    in
    if List.isEmpty texts then
        always Expect.pass

    else
        Expect.all
            (List.indexedMap
                (\index text ->
                    \_ ->
                        children
                            |> Query.index index
                            |> Query.has [ Selector.text text ]
                )
                texts
            )


suite : Test
suite =
    describe "The Main module"
        [ describe "form2formErrors"
            [ form2formErrorsTest
                "sampleInputとanotherInputを満たしていること"
                { sampleInput = Just 15, anotherInput = Just "http://bar.com" }
                { sampleErrors = [], anotherErrors = [] }
            , form2formErrorsTest
                "sampleInputが範囲外エラーを生じ、anotherInputがMatchエラーを生じているとき、それぞれのエラーテキストが表示されること"
                { sampleInput = Just 5, anotherInput = Just "foo" }
                { sampleErrors = [ "Sample Input is out of bounds" ]
                , anotherErrors = [ "Another Input must begin with `http://` or `https://`" ]
                }
            , form2formErrorsTest
                "sampleInputが範囲外エラーを生じ、anotherInputが文字数超エラーとマッチエラーを生じているとき、それぞれのエラーテキストが表示されること"
                { sampleInput = Just 5, anotherInput = Just "foooooooooooooooooooooooo" }
                { sampleErrors = [ "Sample Input is out of bounds" ]
                , anotherErrors =
                    [ "Length of Another Input is toooo long"
                    , "Another Input must begin with `http://` or `https://`"
                    ]
                }
            ]
        , describe "sampleViewModel"
            [ sampleViewTest
                "入力が10で、エラーがない"
                (Just 10)
                []
                "10"
            , sampleViewTest
                "入力が0で、エラーが表示される"
                (Just 0)
                [ "some error" ]
                "0"
            ]
        , describe "anotherViewModel"
            [ anotherViewTest
                "入力がなく、エラーがない"
                Nothing
                []
                ""
            , anotherViewTest
                "入力がhttp://から始まる20文字以内のURLで、エラーがない"
                (Just "http://foo.com")
                []
                "http://foo.com"
            , anotherViewTest
                "入力がhttp://から始まる20文字超えのURLで、文字数超えエラーが表示される"
                (Just "http://foooooooooooooooooooooooooooooooo.com")
                [ "Length of Another Input is toooo long" ]
                "http://foooooooooooooooooooooooooooooooo.com"
            , anotherViewTest
                "入力がURLの形式ではない20文字超えのURLで、パターンエラーと文字数超えエラーが表示される"
                (Just "foooooooooooooooooooooooooooooooo")
                [ "Length of Another Input is toooo long"
                , "Another Input must begin with `http://` or `https://`"
                ]
                "foooooooooooooooooooooooooooooooo"
            ]
        , describe "submitTextView"
            [ test "submit済であれば、h1が表示されている" <|
                \_ ->
                    submitTextView
                        True
                        |> Query.fromHtml
                        |> Query.has [ Selector.tag "h1", Selector.text "Submitted" ]
            , test "submitしていなければ、何も表示されない" <|
                \_ ->
                    submitTextView
                        False
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "" ]
            ]
        , describe "updateSampleInput"
            [ test "与えられた数字によって、sampleInputが更新されたFormが作られること" <|
                \_ ->
                    let
                        actual =
                            updateSampleInput "5" { sampleInput = Just 1, anotherInput = Nothing }

                        expected =
                            { sampleInput = Just 5, anotherInput = Nothing }
                    in
                    Expect.equal actual expected
            ]
        , describe "updateAnotherInput"
            [ test "入力された文字によって、anotherInputが更新されたFormが作られること" <|
                \_ ->
                    let
                        actual =
                            updateAnotherInput "aiueo" { sampleInput = Nothing, anotherInput = Nothing }

                        expected =
                            { sampleInput = Nothing, anotherInput = Just "aiueo" }
                    in
                    Expect.equal actual expected
            , test "入力文字された文字が空の場合、anotherInputがNothingで更新されたFormが作られること" <|
                \_ ->
                    let
                        actual =
                            updateAnotherInput "" { sampleInput = Nothing, anotherInput = Just "abcd" }

                        expected =
                            { sampleInput = Nothing, anotherInput = Nothing }
                    in
                    Expect.equal actual expected
            ]
        ]
