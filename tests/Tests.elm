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
            [ test "sampleInputとanotherInputを満たしていること" <|
                \_ ->
                    let
                        actual =
                            form2formErrors
                                { sampleInput = Just 15, anotherInput = Just "http://bar.com" }

                        expected =
                            { sampleErrors = [], anotherErrors = [] }
                    in
                    Expect.equal actual expected
            , test "sampleInputが範囲外エラーを生じ、anotherInputがMatchエラーを生じているとき、それぞれのエラーテキストが表示されること" <|
                \_ ->
                    let
                        actual =
                            form2formErrors
                                { sampleInput = Just 5, anotherInput = Just "foo" }

                        expected =
                            { sampleErrors = [ "Sample Input is out of bounds" ]
                            , anotherErrors = [ "Another Input must begin with `http://` or `https://`" ]
                            }
                    in
                    Expect.equal actual expected
            ]
        ]
