module Main exposing (AnotherError(..), SampleError(..), anotherValidator, sampleValidator)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
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


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "../src/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]


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
