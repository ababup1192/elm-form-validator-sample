module Main exposing
    ( AnotherError(..)
    , Form
    , FormError(..)
    , FormErrors
    , Msg(..)
    , SampleError(..)
    , anotherInputView
    , anotherValidator
    , form2formErrors
    , formValidator
    , sampleInputView
    , sampleValidator
    , submitTextView
    , view
    )

import Browser
import Html exposing (Html, div, h1, input, li, text, ul)
import Html.Attributes exposing (src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Regex
import Validator exposing (Validator)



---- MODEL ----


type alias Form =
    { sampleInput : Maybe Int
    , anotherInput : Maybe String
    }


type SampleError
    = SampleBoundError
    | SampleRequiredError


sampleValidator : Validator (Maybe Int) SampleError
sampleValidator =
    Validator.required SampleRequiredError <|
        Validator.concat
            [ Validator.minBound SampleBoundError 10
            , Validator.maxBound SampleBoundError 20
            ]


type AnotherError
    = AnotherLengthError
    | AnotherPatternError


anotherValidator : Validator (Maybe String) AnotherError
anotherValidator =
    Validator.optional <|
        Validator.concat
            [ Validator.maxLength AnotherLengthError 20
            , Validator.pattern AnotherPatternError <|
                (Regex.fromString "^(http://|https://)" |> Maybe.withDefault Regex.never)
            ]


type FormError
    = SampleError SampleError
    | AnotherError AnotherError


formValidator : Validator Form FormError
formValidator =
    Validator.concat
        [ Validator.liftMap SampleError .sampleInput sampleValidator
        , Validator.liftMap AnotherError .anotherInput anotherValidator
        ]


displayFormError : FormError -> String
displayFormError err =
    case err of
        SampleError SampleRequiredError ->
            "Sample Input cannot be empty"

        SampleError SampleBoundError ->
            "Sample Input is out of bounds"

        AnotherError AnotherLengthError ->
            "Length of Another Input is toooo long"

        AnotherError AnotherPatternError ->
            "Another Input must begin with `http://` or `https://`"


type alias FormErrors =
    { sampleErrors : List String, anotherErrors : List String }


form2formErrors : Form -> FormErrors
form2formErrors form =
    List.foldl
        (\err ({ sampleErrors, anotherErrors } as formErrors) ->
            case err of
                SampleError _ ->
                    { formErrors | sampleErrors = sampleErrors ++ [ displayFormError err ] }

                AnotherError _ ->
                    { formErrors | anotherErrors = anotherErrors ++ [ displayFormError err ] }
        )
        (FormErrors [] [])
    <|
        Validator.errors formValidator form


type alias Model =
    { form : Form, isSubmitted : Bool }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { form = Form (Just 10) Nothing, isSubmitted = False }, Cmd.none )



---- UPDATE ----


type Msg
    = SampleInput String
    | AnotherInput String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form } as model) =
    case msg of
        SampleInput input ->
            ( { model | form = { form | sampleInput = String.toInt input } }, Cmd.none )

        AnotherInput input ->
            ( { model
                | form =
                    { form
                        | anotherInput =
                            if String.isEmpty input then
                                Nothing

                            else
                                Just input
                    }
              }
            , Cmd.none
            )

        Submit ->
            ( { model | isSubmitted = Validator.isValid formValidator form }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { form, isSubmitted } =
    div []
        [ sampleInputView form.sampleInput (form |> form2formErrors |> .sampleErrors)
        , anotherInputView form.anotherInput (form |> form2formErrors |> .anotherErrors)
        , input [ type_ "button", value "submit", onClick Submit ] []
        , submitTextView isSubmitted
        ]


sampleInputView : Maybe Int -> List String -> Html Msg
sampleInputView sampleInputMaybe sampleErrors =
    let
        sampleInputText =
            Maybe.withDefault "" (sampleInputMaybe |> Maybe.map String.fromInt)
    in
    div []
        [ input [ type_ "number", value sampleInputText, onInput SampleInput ] []
        , ul [ style "list-style-type" "none" ] <|
            (sampleErrors
                |> List.map
                    (\error ->
                        li [ style "color" "red" ]
                            [ text error
                            ]
                    )
            )
        ]


anotherInputView : Maybe String -> List String -> Html Msg
anotherInputView anotherInputMaybe anotherErrors =
    let
        anotherInputText =
            Maybe.withDefault "" anotherInputMaybe
    in
    div []
        [ input [ type_ "text", value anotherInputText, onInput AnotherInput ] []
        , ul [ style "list-style-type" "none" ] <|
            (anotherErrors
                |> List.map
                    (\error ->
                        li [ style "color" "red" ]
                            [ text error
                            ]
                    )
            )
        ]


submitTextView : Bool -> Html Msg
submitTextView isSubmitted =
    if isSubmitted then
        h1 [ style "color" "green" ] [ text "Submitted" ]

    else
        text ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
