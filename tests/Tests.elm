module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)
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


model2SampleViewModelTest : TestCase -> Maybe Int -> List String -> SampleViewModel -> Test
model2SampleViewModelTest testCase sampleInputMaybe errors sampleViewModel =
    test testCase <|
        \_ ->
            let
                actual =
                    model2SampleViewModel sampleInputMaybe errors

                expected =
                    sampleViewModel
            in
            Expect.equal actual expected


model2AnotherViewModelTest : TestCase -> Maybe String -> List String -> AnotherViewModel -> Test
model2AnotherViewModelTest testCase anotherInputMaybe errors anotherViewModel =
    test testCase <|
        \_ ->
            let
                actual =
                    model2AnotherViewModel anotherInputMaybe errors

                expected =
                    anotherViewModel
            in
            Expect.equal actual expected


suite : Test
suite =
    describe "The Main module"
        [ describe "sampleValidator(10~20)"
            [ sampleValidatorTest
                "15は、範囲内の数字であること"
                15
                []
            , sampleValidatorTest
                "5は、範囲より小さい数字は、範囲外エラーが生じること"
                5
                [ SampleBoundError ]
            , sampleValidatorTest
                "21は、範囲より大きい数字は、範囲外エラーが生じること"
                21
                [ SampleBoundError ]
            ]
        , describe "anotherValidator(20文字以下のURL)"
            [ anotherValidatorTest
                "20文字を超えるURLは、文字数エラーが生じること"
                "http://fooooooooooooooo.com"
                [ AnotherLengthError ]
            , anotherValidatorTest
                "20文字を超える文字は、パターンエラーと文字数エラーが生じること"
                "fooooooooooooooooooooooooooo"
                [ AnotherLengthError, AnotherPatternError ]
            , anotherValidatorTest
                "http:// から始まる20文字以下のURLであること"
                "http://foo.com"
                []
            , anotherValidatorTest
                "https:// から始まる20文字以下のURLであること"
                "https://foo.com"
                []
            , anotherValidatorTest
                "https:// | https:// が含まれないURLは、パターンエラーが生じること"
                "foo"
                [ AnotherPatternError ]
            ]
        , describe "form2formErrors"
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
            ]
        , describe "model2SampleViewModel"
            [ model2SampleViewModelTest
                "入力が10で、エラーが無い"
                (Just 10)
                []
                { sampleInputText = "10"
                , sampleErrors = []
                }
            , model2SampleViewModelTest
                "入力が5で、範囲外エラーが表示される"
                (Just 5)
                [ "Sample Input is out of bounds" ]
                { sampleInputText = "5"
                , sampleErrors = [ "Sample Input is out of bounds" ]
                }
            ]
        , describe "model2AnotherViewModel"
            [ model2AnotherViewModelTest
                "入力がなく、エラーがない"
                Nothing
                []
                { anotherInputText = ""
                , anotherErrors = []
                }
            , model2AnotherViewModelTest
                "入力がhttp://から始まる20文字以内のURLで、エラーがない"
                (Just "http://foo.com")
                []
                { anotherInputText = "http://foo.com"
                , anotherErrors = []
                }
            , model2AnotherViewModelTest
                "入力がhttp://から始まる20文字超えのURLで、文字数超えエラーが表示される"
                (Just "http://foooooooooooooooooooooooooooooooo.com")
                [ "Length of Another Input is toooo long" ]
                { anotherInputText = "http://foooooooooooooooooooooooooooooooo.com"
                , anotherErrors = [ "Length of Another Input is toooo long" ]
                }
            , model2AnotherViewModelTest
                "入力がURLの形式ではない20文字超えのURLで、パターンエラーと文字数超えエラーが表示される"
                (Just "foooooooooooooooooooooooooooooooo")
                [ "Length of Another Input is toooo long"
                , "Another Input must begin with `http://` or `https://`"
                ]
                { anotherInputText = "foooooooooooooooooooooooooooooooo"
                , anotherErrors =
                    [ "Length of Another Input is toooo long"
                    , "Another Input must begin with `http://` or `https://`"
                    ]
                }
            ]
        ]
